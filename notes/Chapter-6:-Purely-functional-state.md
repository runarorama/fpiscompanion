## Notes on chapter 6: Purely functional state

### `State` in Scalaz

The [Scalaz library](http://github.com/scalaz/scalaz) supplies a [`State` data type](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.package$$State$) that is a specialization of a more general type [`IndexedStateT`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.IndexedStateT), where `State[S,A]` = `IndexedStateT[Id, S, S, A]` and `Id[A]` = `A`.

You do not need to understand this more general type if you just want to use `State[S,A]`. But the general type has two additional features:

  1. The start state and end state of a state transition can have different types. That is, it's not necessarily a transition `S => (S, A)`, but `S1 => (S2, A)`. The ordinary `State` type is where `S1` and `S2` are fixed to be the same type.
  2. It is a [_monad transformer_](http://en.wikipedia.org/wiki/Monad_transformer) (see chapter 12). The type of a state transition is not `S => (S, A)`, but `S => F[(S, A)]` for some [monad](http://en.wikipedia.org/wiki/Monad_%28functional_programming%29) `F` (see chapter 11). The monad transformer allows us to bind across `F` and `State` in one operation. The ordinary `State` type is where this monad is fixed to be the identity monad `Id`.

### Pseudorandom number generation

The [Wikipedia article on pseudorandom number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator) is a good place to start for more information about such generators. It also makes the distinction between _random_ and _pseudo_-random generation.

There's also a good page on [Linear congruential generators](http://en.wikipedia.org/wiki/Linear_congruential_generator), including advantages and disadvantages and links to several implementations in various languages.

### Deterministic finite state automata

The `State` data type can be seen as a model of [Mealy Machines](http://en.wikipedia.org/wiki/Mealy_machine) in the following way. Consider a function `f` of a type like `A => State[S, B]`. It is a transition function in a Mealy machine where

* The type `S` is the set of states
* `State[S, B]`'s representation is a function of type `S => (B, S)`. Then the argument to that function is the initial state.
* The type `A` is the input alphabet of the machine.
* The type `B` is the output alphabet of the machine.

The function `f` itself is the transition function of the machine. If we expand `A => State[S, B]`, it is really `A => S => (B, S)` under the hood. If we uncurry that, it becomes `(A, S) => (B, S)` which is identical to a transition function in a Mealy machine. Specifically, the output is determined both by the state of type `S` and the input value of type `A`.

Contrast this with a [Moore machine](http://en.wikipedia.org/wiki/Moore_machine), whose output is determined solely by the current state. A Moore machine could be modeled by a data type like the following:

~~~ scala
case class Moore[S, I, A](t: (S, I) => S, g: S => A)
~~~

Together with an initial state `s` of type `S`. Here:

* `S` is the set of states.
* `I` is the input alphabet.
* `A` is the output alphabet.
* `t` is the transition function mapping the state and an input value to the next state.
* `g` is the output function mapping each state to the output alphabet.

As with Mealy machines, we could model the transition function and the output function as a single function:

~~~ scala
type Moore[S, I, A] = S => (I => S, A)
~~~

Since both the transition function `t` and the output function `g` take a value of type `S`, we can take that value as a single argument and from it determine the transition function of type `I => S` as well as the output value of type `A` at the same time.

Mealy and Moore machines are related in a way that is interesting to explore.

### Lenses

If we specialize `Moore` so that the input and output types are the same, we get a pair of functions `t: (S, A) => S` and `g: S => A`. We can view these as (respectively) a "getter" and a "setter" of `A` values on the type `S`:

~~~
get: S => A
set: (S, A) => S
~~~

Imagine for example where `S` is `Person` and `A` is `Name`.

~~~ scala
type Name = String

case class Person(name: Name, age: Int)
~~~

A function `getName` would have the type `Person => Name`, and `setName` would have the type `(Person, Name) => Person`. In the latter case, given a `Person` and a `Name`, we can set the `name` of the `Person` and get a new `Person` with the new `name`.

The getter and setter together form what's called a _lens_. A lens "focuses" on a part of a larger structure, and allows us to modify the value under focus. A simple model of lenses is:

~~~ scala
case class Lens[A, B](get: A => B, set: (A, B) => A)
~~~

Where `A` is the larger structure, and `B` is the part of that structure that is under focus.

Importantly, lenses _compose_. That is, if you have a `Lens[A,B]`, and a `Lens[B,C]`, you can get a composite `Lens[A,C]` that focuses on a `C` of a `B` of an `A`.

Lenses are handy to use with the `State` data type. Given a `State[S,A]`. If we're interested in looking at or modifying a portion of the state, and the portion has type `T`, it can be useful to focus on a portion of the state that we're interested in using a `Lens[S,T]`. The getter and setter of a lens can be readily converted to a `State` action:

~~~scala
def getS[S,A](l: Lens[S, A]): State[S,A] = State(s => (l.get(s), s))
def setS[S,A](l: Lens[S, A], a: A): State[S,Unit] = State(s => (l.set(s, a), ()))
~~~

We cannot, however, turn a `State` action into a `Lens`, for the same reason that we cannot convert a Moore machine into a Mealy machine.

See the [Scalaz library's lenses](http://eed3si9n.com/learning-scalaz/Lens.html), the [Monocle library for Scala](https://github.com/julien-truffaut/Monocle), and the [Lens library for Haskell](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial), for more information about how to take advantage of lenses.

### Stack overflow issues in State

The `State` data type as represented in chapter 6 suffers from a problem with stack overflows for long-running state machines. The problem is that `flatMap` contains a function call that is in tail position, but this tail call is not eliminated on the JVM.

The solution is to use a _trampoline_. Chapter 13 gives a detailed explanation of this technique. See also Rúnar's paper ["Stackless Scala With Free Monads"](http://blog.higher-order.com/assets/trampolines.pdf).

Using the trampolining data type `TailRec` from chapter 13, a stack-safe `State` data type could be written as follows:

~~~ scala
case class State[S,A](run: S => TailRec[(A, S)])
~~~

This is identical to the `State` data type we present in chapter 6, except that the result type of `run` is `TailRec[(S,A)]` instead of just `(S,A)`. See chapter 13 for a thorough discussion of `TailRec`. The important part is that the _result type_ of the `State` transition function needs to be a data type like `TailRec` that gets run at a later time by a tail recursive trampoline function.
