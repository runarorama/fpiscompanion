# Chapter notes #

Chapter notes provide historical context, links to further reading, and additional discussion or connections to help tie the material we've covered in the book to the larger practice and research of the FP community. If you're interested, we encourage you to do some Wikipedia surfing and further exploration using some of the content covered here as a branching-off point.

Many of the chapter notes link to resources that use the [Haskell language](http://www.haskell.org). We recommend reading the [*brief introduction to Haskell*](#hask), which gives an overview of Haskell for Scala programmers.

A more up-to-date version of this content is available on [the FPiS community Wiki.][1]

## Getting answers to your questions ##

If you have any questions about the book, or about functional programming in Scala (or in general), here are some suggestions:

  * Ask on the [scala-functional mailing list](https://groups.google.com/forum/#!forum/scala-functional)
  * Talk to [Paul](http://twitter.com/pchiusano) and [Rúnar](http://twitter.com/runarorama) directly on Twitter.
  * For questions regarding functional programming with the Scalaz library, ask on the [Scalaz mailing list](https://groups.google.com/forum/#!forum/scalaz).
  * Post your question on [the FPiS community Wiki.][1]
  
  
[1]: https://github.com/fpinscala/fpinscala/wiki

## Notes on chapter 1: What is functional programming?

See the Wikipedia articles on [functional programming](http://en.wikipedia.org/wiki/Functional_programming) and [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_%28computer_science%29).

### Why functional programming matters 

In this chapter we highlight some of the benefits of functional programming. A classic article that gives some justification for FP is John Hughes's [*Why Functional Programming Matters*](http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf).

### Referential transparency and side effects 

We introduce functional programming as programming with _referentially transparent_ expressions. Our definition of referential transparency in the chapter is a little bit simplistic, but it serves our purposes for the time being. We discuss a more expanded notion of referential transparency in chapter 14 on "local effects".

A subtlety in the definition of RT given in the chapter is the meaning of "without affecting the meaning". What is the _meaning_ of a program? To answer this, we need to consider the program _with regard to_ some evaluator, or in the context of some other program. That is, the meaning of a program depends very much on how we interpret or evaluate it, and whether some effect of evaluation is to be considered a _side_ effect depends on the observer. For example, the fact that memory allocations occur as a side effect of data construction is not something we usually care to track or are even able to observe on the JVM. So ultimately what we consider to break referential transparency depends very much on what we can or care to observe.

See for example [*What purity is and isn't*](http://blog.higher-order.com/blog/2012/09/13/what-purity-is-and-isnt/) for a discussion of purity with regard to an evaluator, and [*Is network I/O always worth tracking?*](http://pchiusano.github.io/2014-05-21/what-effects-are-worth-tracking.html) for a more philosophical take on what constitutes a side effect.

## Notes on chapter 2: Getting started

We assume in this chapter that you have the Scala compiler and interpreter already up and running. See [the documentation page of Scala's website](http://www.scala-lang.org/documentation/) for more details about how to get Scala set up and links to lots of supplementary material about Scala itself.

### Factorial and Fibonacci ###

We also assume some familiarity with the [factorial function](http://en.wikipedia.org/wiki/Factorial) and we give only a brief explanation of the [Fibonacci sequence](http://en.wikipedia.org/wiki/Fibonacci_number).

### Lambda calculus ###

The notions of first-class and higher-order functions are formalized by the [lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus).

### Parametric polymorphism ###

For more on parametric polymorphism, see [the Wikipedia article.](http://en.wikipedia.org/wiki/Type_variable)

### Parametricity ###

When we can "follow the type" of a function to derive the only possible implementation, we say that the definition is _given by parametricity_. See [the Wikipedia article on parametricity](http://en.wikipedia.org/wiki/Parametricity), and Philip Wadler's paper [*Theorems for free!*](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html)

### Curry ###

The idea of [Currying](http://en.wikipedia.org/wiki/Currying) is named after the mathematician [Haskell Curry.](http://en.wikipedia.org/wiki/Haskell_Curry) He also discovered one of the most important results in computer science, the [Curry-Howard isomorphism](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) which says that _a program is a logical proof, and the hypothesis that it proves is its type_.

### Function composition ###

Function composition in functional programming is closely related to [function composition in mathematics.](http://en.wikipedia.org/wiki/Function_composition)

### FAQ for chapter 2

#### Are tail calls optimized if the `@annotation.tailrec` annotation isn't there?

They are still optimized, but the compiler won't warn you if it can't do the tail call optimization. 

#### Is there a list of other annotation types somewhere? 

See the [Scaladoc for the Annotation class](http://www.scala-lang.org/api/current/index.html#scala.annotation.Annotation), and expand the 'known subclasses section'. 

#### Is the common style to define loops using local function, rather than a (private) standalone function? 

Yes, this is much more common. There's no need to pollute the namespace with helper functions you aren't expecting to be called by anyone. 

#### Is `a || go(x)` considered a tail call? What about `a && go(x)`?

Yes

## Notes on chapter 3: Functional data structures

The Wikipedia article on [algebraic data types](http://en.wikipedia.org/wiki/Algebraic_data_type) has further discussion about the theory behind ADTs.

### Linked lists ###

The [singly-linked list](http://en.wikipedia.org/wiki/Linked_list) (also called a _cons list_) we cover in this chapter is one of the simplest purely functional data structures. It has good performance for linear traversal, but it's not very good for random access or list concatenation.

### Random access vectors and finger trees ###

A better structure for random access is [Vector](http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Vector) in the standard library. It provides constant time (or nearly enough to constant time) access to arbitrary elements in the structure. Vectors can be seen as a specialization of the idea of a [Finger Tree](http://en.wikipedia.org/wiki/Finger_tree).

### Difference lists ###

The [Difference List](http://www.haskell.org/haskellwiki/Difference_list) can provide efficient (constant time) concatenation of lists. The idea is that instead of having a list, we simply compose functions that operate on lists. We can compose functions in constant time, and pass an actual list to the composite function as late as possible.

### Cost amortization ###

Reasoning about complexity of algorithms works a bit differently in a persistent (immutable) setting. We often make use of the fact that the cost of an expensive operation can be amortized over a vastly larger number of very inexpensive operations. An example of this kind of amortization is the cost of the concatenation operation on difference lists (see above). Operating on an actual list takes O(n) time, but we can spread this cost over a number of operations that we compose using the DList. This is an example of [cost amortization](http://en.wikipedia.org/wiki/Amortized_analysis).

### Purely Functional Data Structures ###

Chris Okasaki's book [_Purely Functional Data Structures_](http://books.google.com/books/about/Purely_Functional_Data_Structures.html?id=SxPzSTcTalAC) (Cambridge University Press, 1999; ISBN: 0521663504) gives a thorough treatment of amortization. It is also the canonical text on efficient data structures, both classic and new, from the perspective of functional programming. We highly recommend picking up this book if you're interested in data structures. The dissertation that the book is based on [is also available from Carnegie Mellon University's website](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf).

### Rose trees ###

The tree structure that we introduce at the end of the chapter is called a [Rose Tree](http://en.wikipedia.org/wiki/Rose_tree). It is a nonempty multi-way tree that contains data at the nodes rather than at the leaves.

### The algebra of data types ###

The "algebraic" in algebraic data types means something specific. This is a reference to the fact that such data types are composed of sums and products of other types. More specifically, data types form a [seminearring](http://en.wikipedia.org/wiki/Near-semiring).

See the following links:

* [*The Algebra of Algebraic Data Types*](http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/) by Chris Taylor.
* [*Species and Functors and Types, Oh My!*](http://www.cis.upenn.edu/~byorgey/papers/species-pearl.pdf) by Brent Yorgey
* [*Clowns to the left of me, jokers to the right*](http://personal.cis.strath.ac.uk/~conor/Dissect.pdf) by Conor McBride

### Zippers ###

Since an algebraic data type is a type-level function involving sums and products, we can take the derivative of such a function, yielding a data structure called a [zipper](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29). The zipper for a given structure is like the original structure, but with a movable "focus" or pointer into the structure. This can be used to insert, remove, and modify elements under the focus.

For example, a [list zipper](http://eed3si9n.com/learning-scalaz/Zipper.html) consists of one element under focus together with two lists: one enumerating all the elements to the left of the focus, and another enumerating all the elements to the right of the focus. The focus can me moved left or right (like a zipper on a piece of clothing) and elements will move through the focus. The element under focus can then be removed or modified, and we can insert a new element by consing onto the lists to its left or right.

### Type inference ###

Scala's type system is complicated by the presence of path-dependent types and subtyping. As a result Scala has only very limited, _local_ type inference.

Whereas other programming languages like Haskell or ML may have some species of [Hindley-Milner](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type inference, Scala has what's called "flow-based" inference. That is, type information "flows" from arguments to results. Given the types of the arguments, Scala is able to infer the result type of a function, unless that function is recursive. This also goes for values that are not functions, which can be considered 0-argument functions for the purpose of this discussion. 

We gain type inference benefits from grouping arguments into two argument lists, as in `xs.foldRight(0)(_ + _)`. Type information flows from the first argument list to the second when inferring type arguments to a function call. Note that no inference benefit can be gained from adding more than two argument lists to a function. When inferring the type arguments to a function call, Scala's typer does not consult any argument lists beyond the first.

See the [*Scala Language Specification*](http://www.scala-lang.org/docu/files/ScalaReference.pdf) for more information on Scala's type inference. Specifically sections 4.6.4 (Method Return Type Inference), 6.26.4 (Local Type Inference), and 8.3 (Type Parameter Inference In Patterns).
## <a id="s4"/> Links

* [*Object-Oriented Programming Versus
Abstract Data Types*](http://www.cs.utexas.edu/users/wcook/papers/OOPvsADT/CookOOPvsADT90.pdf)


### FAQ for chapter 3

#### Why do you declare `Nil` as a `case object` instead of a `case class` within the definition of our functional `sealed trait List`?

`case object` is more appropriate because `Nil` is a singleton. We can still use pattern matching in this case. However, there won't be a companion object with `apply`, `unapply`, etc. `case class Nil` will actually cause an error because case classes require an explicit parameter list:

```
scala> case class Nil
<console>:1: error: case classes without a parameter list are not allowed;
use either case objects or case classes with an explicit `()' as a parameter list.
case class Nil
              ^
```

## Notes on chapter 4: Handling errors without exceptions

### Partial and total functions ###

A [partial function](http://en.wikipedia.org/wiki/Partial_function), as opposed to a _total function_, is a function that is not defined for some of its inputs. That is, it's not true for partial functions that every value of the input type maps to  a value of the output type. There are different approaches to representing this kind of function, but in functional programming we commit to using only total functions. The approach we take in this chapter is to augment the return type of partial functions to make it "the same size" as the input type.

For example, the `Option[T]` type simply adds one data constructor, `None`, to the underlying type `T`. That way we can have a total function of type `X => Option[T]` represent a partial function from `X` to `T`. Wherever the function is not defined for a given `X`, we simply map that value to `None` in the result type.

### Turing completeness and the halting problem ###

Another possibility in languages that are [Turing complete](http://en.wikipedia.org/wiki/Turing-complete), which includes Scala, is that a function may "hang" or "run forever" in an infinite loop. Functions that do this are also considered to be partial. However, in a language like Scala we cannot prevent this kind of partiality nor can we recover from it in the general case. It is equivalent to the [halting problem](http://en.wikipedia.org/wiki/Halting_problem).

### Bottoms ###

A program that hangs (by running in an infinite loop) or crashes (by throwing an exception) is said to "evaluate to bottom". The term "bottom" (sometimes "falsum") comes from logic, where it is used to denote a contradiction. The [Curry-Howard isomorphism](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) says that types are propositions, and a program of a given type is a proof of that proposition. Since a non-terminating program can have _any type_, we can say that it's a proof of _every proposition_. This is exactly like the situation with contradictions in logic--if you assume a contradiction, you can prove any proposition.

### Total languages ###

Some programming languages are [total languages](http://en.wikipedia.org/wiki/Total_functional_programming), and do not provide facilities for unbounded recursion like Scala does. A program in such a language _provably terminates_, and there are no bottoms. The price of this is that such languages are not Turing complete, since there exist in theory some programs that they cannot express, although the capabilities of total languages like [Agda](http://en.wikipedia.org/wiki/Agda_(programming_language)), [Coq](http://coq.inria.fr/), and [Idris](http://www.idris-lang.org/) are suggesting to us that Turing completeness may not be necessary for a large majority of useful programs. Also see the paper [Total Functional Programming](http://www.jucs.org/jucs_10_7/total_functional_programming/jucs_10_07_0751_0768_turner.pdf).

### Covariance and contravariance ###

In this chapter we briefly touch on the subject of _variance_ in Scala. This is a feature of the subtyping in Scala's type system. Throughout this book we tend to largely ignore this feature since we find that in practice it unnecessarily complicates Scala's type system. We find that it's not beneficial to functional programming and can in fact often be a barrier to good API design. Outside of the [language specification](http://www.scala-lang.org/docu/files/ScalaReference.pdf), there is not much freely available documentation of how variance works in Scala specifically, but for a general discussion see the [Wikipedia article on covariance and contravariance.](http://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29)

#### Variance in `Option[+A]` ####

Recall that the `+A` in `Option[+A]` declares that, for example, `Option[Dog]` is a subtype of `Option[Animal]` (and that `None`, having type `Option[Nothing]`, is a subtype of any `Option[A]`). But why are we forced to accept a _supertype_ of `A` (as indicated by the `[B >: A]`) in `getOrElse` and `orElse`?

Another way to state the `+A` annotation is that we're telling Scala that in _all contexts_ it's safe to convert this `A` to a supertype of `A`--`Dog` may be converted to `Animal`, for instance. Scala (correctly) won't let us provide this annotation unless all members of a type _agree_ it's safe to do this conversion. Let's see the contradiction if `orElse` had the following (simpler) signature:

```scala
trait Option[+A] {
  def orElse(o: Option[A]): Option[A]
  ...
}
```

This is problematic--since `orElse` is a function accepting an `Option[A]` as an argument, this is a place where we may only convert `A` to a _subtype_ of `A`. Why? Like any function, `orElse` must be passed a subtype of the type of argument it accepts--a `Dog => R` can be called with a `Poodle` or `Dog`, not an arbitrary `Animal`. (We therefore say that such functions are _contravariant_ in their argument type.) But the fact that we have a member of `Option[A]` that only allows subtypes of `A` contradicts the `+A` in `Option[A]`, which says that in _all contexts_ we can convert this `A` to any _supertype_ of `A`. Contradictions like this will result in a compile error like `"covariant type A occurs in contravariant position"`. Scala must enforce that no such contradictions exist, or we could circumvent the type-checker and easily write programs that gave a type error at runtime, in the form of a `ClassCastException`.

The more complicated signature fixes the contradiction by not mentioning `A` in any of the function arguments:

```
def orElse[B >: A](o: Option[B]): Option[B]
```

#### Covariant and contravariant positions ####

A type is in _covariant position_ (positive) if it is in the result type of a function, or more generally is the type of a value that is _produced_.

A type is in _contravariant position_ (negative) if it's in the argument type of a function, or more generally is the type of a value that is _consumed_.

For example, in `def foo(a: A): B`, the type `A` is in contravariant position and `B` is in covariant position, all things being equal.

We can extend this reasoning to higher-order functions. In `def foo(f: A => B): C`, the type `A => B` appears in negative (contravariant) position. This means the variance of the types `A` and `B` is flipped. The type `A` appears in a negative position of a type in negative position. So just like the negation of a negative is a positive, this means `A` is actually in covariant position. And since `B` is in the covariant position of a type in contravariant position, it's the negation of a positive, so `B` is in contravariant position overall.

We can always count the position of a polymorphic type this way. Result types are positive, and argument types are negative. The arguments to arguments are positive, arguments to arguments to arguments are negative, and so on.

## Notes on chapter 5: Strictness and laziness

### Non-strictness vs laziness

[The Haskell website](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict) has a good explanation of the difference between _non-strictness_ and _laziness_.

In short, "non-strict" just means "not strict". There are many possible [evaluation strategies](http://en.wikipedia.org/wiki/Evaluation_strategy) one could employ when evaluating a program, and [strict evaluation](http://en.wikipedia.org/wiki/Strict_evaluation) is one of them. [Non-strict evaluation](http://en.wikipedia.org/wiki/Non-strict_evaluation#Non-strict_evaluation) is a _class_ of evaluation strategies, and [lazy evaluation](http://en.wikipedia.org/wiki/Lazy_evaluation) is one non-strict strategy (also known as "call-by-need").

In Scala, non-strict arguments are sometimes called "by name" arguments, in reference to the fact that the evaluation strategy Scala employs for those arguments is [call-by-name](http://en.wikipedia.org/wiki/Call_by_name#Call_by_name). We can turn an argument into call-by-need by caching it in a `lazy val` inside the function:

``` scala
def pairIf[A](b: Boolean, x: => A) = {
  lazy val y = x
  if (b) (y, y)
}
```

This function will evaluate `x` only once, or never if the boolean `b` is `false`. If we said `(x, x)` instead of `(y, y)`, it would evaluate `x` twice.

The chapter explains that when an expression does not terminate, it is said to evaluate to _bottom_.  At first glance this is counter intuitive because there is a natural tendency to think of infinity as having _no_ bottom.  But the bottom to which the chapter refers is actually the [bottom type](http://en.wikipedia.org/wiki/Bottom_type). See Haskell's definition of [bottom](http://www.haskell.org/haskellwiki/Bottom) for a more thorough description. Scala refers to it as [Nothing](http://www.scala-lang.org/api/current/#scala.runtime.Nothing$), which is at the _bottom_ of the inheritance hierarchy.

### Corecursion and codata

The [Wikipedia article on corecursion](http://en.wikipedia.org/wiki/Corecursion) is a good starting point for understanding the concept.

The article on [Coinduction](http://en.wikipedia.org/wiki/Coinduction) has some further links. Dan Piponi's article [*Data and Codata*](http://blog.sigfpe.com/2007/07/data-and-codata.html) talks about corecursion as "guarded" recursion.

Ralf Hinze's paper [*Reasoning about Codata*](http://www.cs.ox.ac.uk/ralf.hinze/publications/CEFP09.pdf) brings equational reasoning to corecursive programs by employing applicative functors. Hinze's paper will be more comprehensible to readers who have finished part 3 of our book.

### Tying the knot

Non-strictness allows us to create cyclic streams such as:

``` scala
val cyclic: Stream[Int] = 0 #:: 1 #:: cyclic
```

This may seem like it shouldn't work. The stream is referencing itself in its own tail! But the trick is that the `#::` constructor is non-strict in its second argument. The evaluation of `cyclic` will stop without expanding the expression `1 #:: cyclic`. It's not until somebody takes the `tail` of the `tail` of `cyclic` that the recursive reference is expanded, and again it expands only one element at a time, allowing for an infinite, cyclic stream.

Note that the `cyclic` stream is reusing its own structure. `cyclic.tail.tail` is not a new stream that looks like `cyclic`. It really is the same object as `cyclic` in every sense:

```
scala> cyclic.tail.tail eq cyclic
res0: Boolean = true
```

This technique is sometimes called "Tying the Knot". For more information see the [Haskell.org article](http://www.haskell.org/haskellwiki/Tying_the_Knot).

However, be careful of creating such structures using the Scala standard library's `Stream`. They can be quite fragile. The reason is that `scala.Stream` is is strict in the head element and it will also [memoize](http://en.wikipedia.org/wiki/Memoization) the computed contents, which can lead to memory leaks.

### Stream.apply

Be careful using Stream.apply, both in the standard library and in the exercises: they are constructed using _repeated parameters_, which are always strict. This means that e.g.

```scala
Stream({println("One"); 1}, {println("Two"); 2}, {println("Three"); 3})
```

will immediately print One, Two, Three. Although the Stream will be constructed lazily, the contents have already been evaluated. 

For truly lazily constructed Streams you can always resort to `#::` (which still evaluates the head value strictly!) or nested cons(..,cons(..,..)) operators in the exercises. 

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

``` scala
case class Moore[S, I, A](t: (S, I) => S, g: S => A)
```

Together with an initial state `s` of type `S`. Here:

* `S` is the set of states.
* `I` is the input alphabet.
* `A` is the output alphabet.
* `t` is the transition function mapping the state and an input value to the next state.
* `g` is the output function mapping each state to the output alphabet.

As with Mealy machines, we could model the transition function and the output function as a single function:

``` scala
type Moore[S, I, A] = S => (I => S, A)
```

Since both the transition function `t` and the output function `g` take a value of type `S`, we can take that value as a single argument and from it determine the transition function of type `I => S` as well as the output value of type `A` at the same time.

Mealy and Moore machines are related in a way that is interesting to explore.

### Lenses

If we specialize `Moore` so that the input and output types are the same, we get a pair of functions `t: (S, A) => S` and `g: S => A`. We can view these as (respectively) a "getter" and a "setter" of `A` values on the type `S`:

```
get: S => A
set: (S, A) => S
```

Imagine for example where `S` is `Person` and `A` is `Name`.

``` scala
type Name = String

case class Person(name: Name, age: Int)
```

A function `getName` would have the type `Person => Name`, and `setName` would have the type `(Person, Name) => Person`. In the latter case, given a `Person` and a `Name`, we can set the `name` of the `Person` and get a new `Person` with the new `name`.

The getter and setter together form what's called a _lens_. A lens "focuses" on a part of a larger structure, and allows us to modify the value under focus. A simple model of lenses is:

``` scala
case class Lens[A, B](get: A => B, set: (A, B) => A)
```

Where `A` is the larger structure, and `B` is the part of that structure that is under focus.

Importantly, lenses _compose_. That is, if you have a `Lens[A,B]`, and a `Lens[B,C]`, you can get a composite `Lens[A,C]` that focuses on a `C` of a `B` of an `A`.

Lenses are handy to use with the `State` data type. Given a `State[S,A]`. If we're interested in looking at or modifying a portion of the state, and the portion has type `T`, it can be useful to focus on a portion of the state that we're interested in using a `Lens[S,T]`. The getter and setter of a lens can be readily converted to a `State` action:

```scala
def getS[S,A](l: Lens[S, A]): State[S,A] =
  State(s => (l.get(s), s))

def setS[S,A](l: Lens[S, A], a: A): State[S,Unit] =
  State(s => (l.set(s, a), ()))
```

We cannot, however, turn a `State` action into a `Lens`, for the same reason that we cannot convert a Moore machine into a Mealy machine.

See the [Scalaz library's lenses](http://eed3si9n.com/learning-scalaz/Lens.html), the [Monocle library for Scala](https://github.com/julien-truffaut/Monocle), and the [Lens library for Haskell](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial), for more information about how to take advantage of lenses.

### Stack overflow issues in State

The `State` data type as represented in chapter 6 suffers from a problem with stack overflows for long-running state machines. The problem is that `flatMap` contains a function call that is in tail position, but this tail call is not eliminated on the JVM.

The solution is to use a _trampoline_. Chapter 13 gives a detailed explanation of this technique. See also Rúnar's paper [*Stackless Scala With Free Monads*](http://blog.higher-order.com/assets/trampolines.pdf).

Using the trampolining data type `TailRec` from chapter 13, a stack-safe `State` data type could be written as follows:

``` scala
case class State[S,A](run: S => TailRec[(A, S)])
```

This is identical to the `State` data type we present in chapter 6, except that the result type of `run` is `TailRec[(S,A)]` instead of just `(S,A)`. See chapter 13 for a thorough discussion of `TailRec`. The important part is that the _result type_ of the `State` transition function needs to be a data type like `TailRec` that gets run at a later time by a tail recursive trampoline function.

## Notes on chapter 7: Purely functional parallelism

FP has a long history of using combinator libraries for expressing parallelism, and there are a lot of variations of the general idea. The main design choices in this sort of library are around how explicit to make the _forking_ and _joining_ of parallel computations. That is, should the API force the programmer to be fully explicit about when parallel tasks are being forked off into a separate logical thread, or should this be done automatically? And similarly, when waiting for the results of multiple logical threads (say, for the implementation of `map2`), should the order of these joins be something the programmer explicitly specifies or chosen by the framework? 

The library we developed in this chapter sits somewhere in the middle--it is explicit about where tasks are forked, but not when tasks are joined (notice that `map2` picks the order it waits for the two tasks whose results it is combining). The join order can be made more explicit. Simon Marlow, one of the [GHC Haskell](http://www.haskell.org/ghc/) developers, discusses this in [Parallel programming in Haskell with explicit futures](http://ghcmutterings.wordpress.com/2010/08/20/parallel-programming-in-haskell-with-explicit-futures/). Also see the full paper, [Seq no more: Better Strategies for Parallel Haskell](http://www.haskell.org/~simonmar/papers/strategies.pdf), which does a nice job of explaining some of the history of approaches for parallelism in Haskell.

Note that because Scala is a strict-by-default language, being more explicit about the join order isn't necessarily as beneficial as in Haskell. That is, we can get away with reasoning about join order much like we think about evaluation in normal strict function application.

This style of library isn't particularly good at expressing _pipeline parallelism_ that's possible when transforming streams of data. For instance, if we have a `Stream[Int]` of 10000 items, and we wish to square each value, then compute a running sum of the squared values, there is a potential for parallelism--as we are squaring values, we can be passing the squared values off to another consumer that is emitting the running sum of the values it receives. We'll be discussing stream processing and pipeline parallelism more in part 4. 

### Notes about map fusion ###

We noted in this chapter that one of our laws for `Par`, sometimes called _map fusion_, can be used as an optimization:

``` scala
map(map(y)(g))(f) == map(y)(f compose g)
```

That is, rather than spawning a separate parallel computation to compute the second mapping, we can fold it into the first mapping. We mentioned that our representation of `Par` doesn't allow for this, as it's too 'opaque'. If we make `Par` a proper data type and give it constructors that we can pattern match on, then it's easy to implement map fusion:

``` scala
trait Par[+A] {
  def map[B](f: A => B): Par[B] = this match {
    case MapPar(p, g) => MapPar(p, g andThen f)
    case _ => MapPar(
  }
case class MapPar[A,+B](par: Par[A], f: A => B) extends Par[B]
```

Baking ad hoc optimization rules like this into our data type works, but it can sometimes get unwieldy, and it's not very modular (we don't get to reuse code if there's some other data type needing similar optimizations). There are various ways of factoring out these sorts of optimizations so our core data type (be it `Par` or some other type) stays clean, and the optimization is handled as a separate concern. Edward Kmett has a nice [blog series discussing this approach](http://comonad.com/reader/2011/free-monads-for-less/). Before embarking on that series you'll need to be familiar with the content in part 3 of this book, and you should read [the Haskell appendix](https://github.com/pchiusano/fpinscala/wiki/A-brief-introduction-to-Haskell,-and-why-it-matters) as well.

## Notes on chapter 8: Property-based testing

The style of combinator library for testing we developed in this chapter was introduced in a 2000 paper by Koen Claessen and John Hughes, [*QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs*](http://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf) (PDF). In that paper, they presented a Haskell library, called [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck), which became quite popular in the FP world and has inspired similar libraries in other languages, including [ScalaCheck](https://github.com/rickynils/scalacheck/wiki/User-Guide). Many programmers who adopt this style of testing find it to be extraordinarily effective (see, for instance, this [experience report](http://blog.moertel.com/pages/seven-lessons-from-the-icfp-programming-contest) on [Tom Moertel's blog](http://blog.moertel.com/)).

The wikipedia page on [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) and the [Haskell wiki page](http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck) are good places to start if you're interested in learning more about these sorts of libraries. QuickCheck sparked a number of variations, including the Haskell library [SmallCheck](https://github.com/feuerbach/smallcheck), which is focused on exhaustive enumeration.

Although property-based testing works quite well for testing pure functions, it can also be used for testing imperative code. The general idea is to generate _lists of instructions_, which are then fed to an interpreter of these actions. We then check that the pre and post-conditions are as expected. Here's a simple example of testing the mutable stack implementation from Scala's standard library ([API docs](http://www.scala-lang.org/api/current/scala/collection/mutable/ArrayStack.html)):

``` scala
forAll(Gen.listOf(Gen.choose(1,10))) { l => 
  val buf = new collection.mutable.ArrayStack[Int]
  val beforeSize = buf.size 
  l.foreach(buf.push)
  buf.beforeSize == 0 && buf.size == l.size
}
```

In this case, the "interpreter" is the `push` method on `ArrayStack`, which modifies the stack in place, and the "instructions" are simply the integers from the input list. But the basic idea can be extended to testing richer interfaces--for instance, we could generate instructions that could either `push` or `pop` elements from an `ArrayStack` (perhaps represented as a `List[Option[Int]]`), and write a property that sequences of `push` and `pop` preserve the invariants of `ArrayStack` (for instance, the final size of the stack should be the number of `push` calls minus the number of `pop` calls). Care must be taken to craft generators that produce valid sequences of instructions (for instance, `pop` without a corresponding prior `push` is not a valid input).

Similar ideas have been used for testing thread safety of concurrent programs. (See [*Finding Race Conditions in Erlang with QuickCheck and PULSE*](http://www.protest-project.eu/upload/paper/icfp070-claessen.pdf) (PDF)) The key insight here is that thread-safe code does not allow the nondeterminism of thread scheduling to be _observable_. That is, for any _partial order_ of instructions run concurrently, we ought to able to find some single-threaded linear sequence of these instructions with the same observable behavior (this criteria is often called _linearizability_). For instance, if our `ArrayStack` were thread-safe, we would expect that if 2 `push` operations were performed sequentially, followed by two `pop` operations and two `push` operations performed concurrently, this should yield the same result as some deterministic linear sequence of these `push` and `pop` operations). There are some subtleties and interesting questions about how to model this and how to report and minimize failing test cases. In particular, doing it the "obvious" way ends up being intractable due to having to search through a combinatorial number of interleavings to find one that satisfies the observation. The Erlang paper linked above has more details, in particular see section 4. You may be interested to explore how to incorporate these ideas into the library we developed, possibly building on the parallelism library we wrote last chapter. 

Lastly, we mention that one design goal of some libraries in this style is to avoid having to _explicitly_ construct generators. The QuickCheck library makes use of a Haskell type class to provide instances of `Gen` "automatically", and this idea has also been borrowed by [ScalaCheck](https://github.com/rickynils/scalacheck/wiki/User-Guide). This can certainly be convenient, especially for simple examples, though we often find that explicit generators are necessary to capture all the interesting constraints on the shape or form of the inputs to a function.

## Notes on chapter 9: Parser combinators

### Different approaches to functional design

There are many different approaches to functional design. The approach we take in this chapter we are calling "algebraic design" to emphasize the fact that we are mainly concerned with the abstract data types and laws of our API. The actual representation is a later concern, and we choose a representation that best facilitates our algebra. This perspective is common to [Category Theory](http://en.wikipedia.org/wiki/Category_theory), where mathematical objects (or a _type_ in the more concretely programming-related case) are defined solely by their relationship to other objects, not their "internal" structure.

[Conal Elliott](http://conal.net/) advocates for an approach he calls [denotational design](http://conal.net/papers/type-class-morphisms), also see [this nice exposition by Luke Palmer](http://lukepalmer.wordpress.com/2008/07/18/semantic-design/). In this style of design, the choice of a _precise meaning_ or _denotation_ (see [denotational semantics](http://en.wikipedia.org/wiki/Denotational_semantics)) for the data types in our library guides the design process. This denotation is not (necessarily) similar to the actual concrete representation we ultimately select when implementing our library--that happens later. The denotation is instead a hopefully simple, precise mathematical object that lets us understand what the data types of our library and their various operations _mean_ (in terms of how they transform these denotations). In this style of design, we begin with some initial idea of a denotation for our data type, and refine this denotation in response to the operations we wish to support.

There is no "right" answer in approaches to the design of software, and the higher your perspective, the more the lines between different approaches blur.

### Design issues in parser combinator libraries

Parser combinator libraries are very common in the FP world, and there are a lot of different variations. A few of the key design issues:

_Error reporting_: Generating good error messages is a delicate problem, and is easy to get wrong. The overall error-reporting strategy developed in this chapter is most similar to [Parsec](http://legacy.cs.uu.nl/daan/parsec.html), a widely used parsing library in Haskell, though Parsec does not support the _nested_ error messages we developed here. A key factor for good error reporting is having a clear model for _when backtracking is allowed to occur_ and giving the programmer the ability to control this when specifying the grammar. We chose to adopt Parsec's convention that all parsers commit by default if they consume at least one character, and that the `attempt` combinator "undoes" this commit. Another choice is to _not_ have parsers commit by default, and introduce a separate `commit` combinator (which is still "undone" by `attempt`). This has the same expressiveness but we find it is not usually the best default since most grammars require little lookahead. Interestingly, the grammar ends up looking almost the same with the introduction of an explicit `commit`: the leaf-level `token` parser is usually wrapped in `commit`, and higher levels of the grammar that require more lookahead use `attempt`.

_Input handling:_ Is the input type fixed to be `String` or can parsers be polymorphic over any sequence of tokens? The library developed in this chapter chose `String` for simplicity and speed. Separate tokenizers are not as commonly used with parser combinators--the main reason to use a separate tokenizer would be for speed, which is usually addressed by building a few fast low-level primitive like `regex` (which checks whether a regular expression matches a prefix of the input string). 

Having the input type be an arbitrary `Seq[A]` results in a library that could in principle be used for other sorts of computations (for instance, we can "parse" a sum from a `Seq[Int]`). There are usually better ways of doing these other sorts of tasks (see the discussion of stream processing in part 4 of the book) and we have found that there isn't much advantage in generalizing the library we have here to other input types. 

We can define a similar set of combinators for parsing _binary_ formats. Binary formats rarely have to deal with lookahead, ambiguity, and backtracking, and have simpler error reporting needs, so often a more specialized binary parsing library is sufficient. Some examples of binary parsing libraries are [Data.Binary](http://code.haskell.org/binary/) in Haskell, which has inspired similar libraries in other languages, for instance [scodec](https://github.com/scodec/scodec) in Scala.

You may be interested to explore generalizing or adapting the library we wrote enough to handle binary parsing as well (to get good performance, you may want to try using the [specialized annotation](http://www.scala-notes.org/2011/04/specializing-for-primitive-types/) to avoid the overhead of boxing and unboxing that would otherwise occur). 

_Streaming parsing and push vs. pull_: It is possible to make a parser combinator library that can operate on huge inputs, larger than what is possible or desirable to load fully into memory. Related to this is is the ability to produce results in a streaming fashion--for instance, as the raw bytes of an HTTP request are being read, the parse result is being constructed. The two general approaches here are to allow the parser to _pull_ more input from its input _source_, and inverting control and _pushing_ chunks of input to our parsers (this is the approach taken by the popular Haskell library [attoparsec](http://hackage.haskell.org/packages/archive/attoparsec/0.10.2.0/doc/html/Data-Attoparsec-ByteString.html)), which may report they are finished, failed, or requiring more input after each chunk. We will discuss this more in part 4 when discussing stream processing. 

_Handling of ambiguity and left-recursion_: The parsers developed in this chapter scan input from left to right and use a left-biased `or` combinator which selects the "leftmost" parse if the grammar is ambiguous--these are called [LL parsers](http://en.wikipedia.org/wiki/LL_parser). This is a design choice. There are other ways to handle ambiguity in the grammar--in [GLR parsing](http://en.wikipedia.org/wiki/GLR_parser), roughly, the `or` combinator explores both branches simultaneously and the parser can return multiple results if the input is ambiguous. (See [Daniel Spiewak's series on GLR parsing](http://www.codecommit.com/blog/scala/unveiling-the-mysteries-of-gll-part-1) also his [paper](http://www.cs.uwm.edu/~dspiewak/papers/generalized-parser-combinators.pdf)) GLR parsing is more complicated, there are difficulties around supporting good error messages, and we generally find that its extra capabilities are not needed in most parsing situations.

Related to this, the parser combinators developed here cannot directly handle _left-recursion_, for instance:

``` scala
def expr = (int or double) or (
           expr ** "+" ** expr ) or (
           "(" ** expr ** ")" )
```

This will fail with a stack overflow error, since we will keep recursing into `expr`. The invariant that prevents this is that all branches of a parser must consume at least one character before recursing. In this example, we could do this by simply factoring the grammar a bit differently:  

``` scala
def leaf = int or double or ("(" ** expr ** ")")
def expr = leaf.sep("+")
```

Where `sep` is a combinator that parses a list of elements, ignoring some delimiter ("separator") that comes between each element (it can be implemented in terms of `many`). This same basic approach can be used to handle operator precedence--for instance, if `leaf` were a parser that included binary expressions involving multiplication, this would make the precedence of multiplication higher than that of addition. Of course, it is possible to write combinators to abstract out patterns like this. It is also possible to write a monolithic combinator that uses a [more efficient (but specialized) algorithm](http://en.wikipedia.org/wiki/Shunting-yard_algorithm) to build operator parsers.

### Other interesting areas to explore related to parsing

* _Monoidal and incremental parsing_: This deals with the much harder problem of being able to incrementally reparse input in the presences of inserts and deletes, like what might be nice to support in a text-editor or IDE. See [Edward Kmett's articles on this](http://comonad.com/reader/2009/iteratees-parsec-and-monoid/) as a starting point.
* _Purely applicative parsers_: If we stopped before adding `flatMap`, we would still have a useful library--context-sensitivity is not often required, and there are some interesting tricks that can be played here. See this [stack overflow answer](http://stackoverflow.com/questions/7861903/what-are-the-benefits-of-applicative-parsing-over-monadic-parsing) for a discussion and links to more reading.

### Single pass parsing

Lastly, we mentioned that the JSON parser we wrote in this chapter was rather dumb in that it only constructed a parse tree. This is unfortunate--we will typically then have to traverse this parse tree to extract some meaningful type from the parse tree after the fact. We could instead build up whatever type we wish _as we go_. The general idea is to parameterize our grammar on functions that describe what to do at each level of the parse tree:
  
``` scala
def array[A,B](value: Parser[A])(f: List[A] => B): Parser[B] = 
  "[" *> value.sep(",").map(f) <* "]"
  // `*>` ignores the left parse result, in this case the `"["`, 
  // and `<*` ignores the right parse result, in this case the `"]"`.
```

This parses a JSON array, but rather than building a `JArray`, immediately converts the result to `B` using the given function, where `B` might be some more meaningful type. The same strategy can be applied for the rest of the grammar.

## Notes on chapter 10: Monoids

### Monoids in category theory

In [category theory](http://en.wikipedia.org/wiki/Category_theory), a [monoid](http://en.wikipedia.org/wiki/Monoid_(category_theory)) is a [category](http://en.wikipedia.org/wiki/Category_(mathematics)) with one object.

A _category_ is a purely algebraic structure consisting of "objects" and "arrows" between them, much like a directed graph with nodes and edges between them. A category will have objects like `A`, `B`, `C`, etc. and arrows between objects. Importantly, arrows _compose_. Given an arrow `f` from `A` to `B`, and another arrow `g` from `B` to `C`, their composition is an arrow from `A` to `C`. There is also an identity arrow from every object to itself.

For example, Scala forms a category where the objects are Scala types and the arrows are Scala functions. There's another category where the objects are Scala types and the arrows are subtype relationships.

Arrows in a category compose according to certain laws. Composition has to obey an _identity law_ such that any arrow `f` composed with the identity arrow is just `f`. That is, the identity arrow is _an identity with regard to composition_. Composition also has to obey an _associative law_. Denoting the composition operator as `*`, the associative law says that `(f * g) * h` should be the same as `f * (g * h)`. That is, it doesn't matter whether we compose on the left or the right first. This should remind you of the monoid laws!

A `Monoid[M]`, then, is just a category where the only object is the type `M`, and the arrows are the values of type `M`. The identity arrow is the identity element of the monoid, and arrow composition is the monoid's binary operation.

It can be a little difficult to see how the values of a type like `Int` can be _arrows_. Take for instance the monoid formed by integers with addition. Then the integer `5`, say, can be seen as an _operation_ that _adds 5_ to another integer. And `0` can be seen as an operation that leaves an integer alone.

See Rúnar's article [*On Monoids*](http://apocalisp.wordpress.com/2010/06/14/on-monoids/) for some of the deeper connections.

### The canonicity of a Scala monoid

In Scala, it's possible to have multiple `Monoid` instances associated with a type. For example, for the type `Int`, we can have a `Monoid[Int]` that uses addition with `0`, and another `Monoid[Int]` that uses multiplication with `1`.

This can lead to certain problems since we cannot count on a `Monoid` instance being _canonical_ in any way. To illustrate this problem, consider a "suspended" computation like the following:

``` scala
case class Suspended(acc: Int, m: Monoid[Int], remaining: List[Int])
```

This represents an addition that is "in flight" in some sense. It's an accumulated value so far, represented by `acc`, a monoid `m` that was used to accumulate `acc`, and a list of `remaining` elements to add to the accumulation using the monoid.

Now, if we have _two_ values of type `Suspended`, how would we add them together? We have no idea whether the two monoids are the same. And when it comes time to add the two `acc` values, which monoid should we use? There's no way of inspecting the monoids (since they are just functions) to see if they are equivalent. So we have to make an arbitrary guess, or just give up.

[Monoids in Haskell](http://www.haskell.org/haskellwiki/Monoid) work a bit differently. There can only ever be one `Monoid` instance for a given type in Haskell. This is because `Monoid` is a [type class](https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes). Instances of Haskell _type classes_ are not first-class values like they are in Scala. They are implicitly passed and never explicitly referenced. They do not have types per se, but [appear as constraints on types](http://www.haskell.org/tutorial/classes.html).

In Haskell, there is only one abstract monoidal operation and one zero. They are both polymorphic, and their signatures are:

``` haskell
mappend :: Monoid m => m -> m -> m
mempty :: Monoid m => m
```

This reads: "for any type `m`, if `m` is a monoid, `mappend` takes an `m` and another `m` and returns an `m`", and "`mempty` is a value of any type `m`, given that `m` is a monoid."

How do we then represent types that are monoids in more than one way, for example `Int` with addition as one monoid and multiplication as another? The answer is that we make each monoid a [newtype](http://www.haskell.org/haskellwiki/Newtype):

``` haskell
newtype Product = Product { getProduct :: Int }
newtype Sum = Sum { getSum :: Int }
```

A newtype in Haskell is a lot like a case class in Scala, except that newtypes can only have exactly one field, and they have _no runtime representation_ other than the underlying type of that field. It's a purely type-level construct, a kind of tagged type.

Using this mechanism, a product and a sum are actually different types, even though the underlying value is an `Int` in both cases. This way every type has its own canonical monoid, and values accumulated in one monoid can never be confused with values accumulated in another. But we can always convert between them if we need to.

[The Scalaz library](https://github.com/scalaz/scalaz) takes the same approach, where there is only one canonical monoid per type. However, since Scala doesn't have type constraints, the canonicity of monoids is more of a convention than something enforced by the type system. And since Scala doesn't have newtypes, we use [phantom types](http://www.haskell.org/haskellwiki/Phantom_type) to add tags to the underlying types. This is done with [`scalaz.Tag`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Tags$), which uses a couple of type aliases:

``` scala
type Tagged[A] = AnyRef { type T = A }
type @@[A,B] = A with Tagged[B]
```

Now the type `Int @@ Product`, for example, is just the type `Int`, but "tagged" with `Product` to make it explicit that `Monoid[Int @@ Product]` is distinct from `Monoid[Int @@ Sum]`. The types `Product` and `Sum` themselves are just empty traits with no members whatsoever.

Tim Perrett wrote a [blog post](http://timperrett.com/2012/06/15/unboxed-new-types-within-scalaz7/) detailing how tagged types work in Scalaz.

The great benefit of canonicity is that Scalaz can build a monoid instance for you just from the type. For example, if you have a `Map[Int, Map[String, (Int, Boolean)]]`, Scalaz can figure out a canonical composite monoid for this type:

``` scala
import scalaz._
import Scalaz._
import Tags._

val x = Map(1 -> Map("Foo" -> (1, Conjunction(false))))
val y = Map(1 -> Map("Foo" -> (2, Conjunction(true))))

val z = x |+| y
```

The value of `z` will be `Map(1 -> Map("Foo" -> (3, Conjunction(false))))`. Here, `Conjunction` is a "newtype" for `Boolean` to indicate that we want the "conjunction" monoid (`Boolean` with `&&` as the op and `true` as the identity element). There's a corresponding `Disjunction` for `||` with `false`. At the present time, Scalaz's default monoid for `Int` is addition, which is why we get `3` in `z`.

The syntax `x |+| y` works for any monoid. It works because there is an implicit class on the `Scalaz._` import. Be warned though, the `Scalaz._` import brings a lot of implicits into scope. You can import just the monoid syntax with `import scalaz.syntax.monoid._`.

### Monoid coproducts

If `A` and `B` are monoids, then `(A,B)` is a monoid, called their _product_. But what about _coproducts_? Is there a composite monoid where we can have _either_ an `A` or a `B`? Yes there is. See [Rúnar's blog post on monoid coproducts](http://blog.higher-order.com/blog/2014/03/19/monoid-morphisms-products-coproducts/) for the details.

### Links

[On Monoids, by Rúnar](http://apocalisp.wordpress.com/2010/06/14/on-monoids/) -- Provides some deeper insight on the relationship between monoids and lists, and looks at them from a category theory perspective.

## Notes on chapter 11: Monads

### Monad laws through `join`

There is a formulation of the monad laws that we don't discuss in the chapter. We talk about the associative law in terms of `flatMap` and `compose` (Kleisli composition), but we can also state it in terms of `join`:

``` scala
join(join(x)) == join(map(x)(join))
```

That is, if we have a value `x` of type `F[F[F[A]]]`, we can `join` twice to get `F[A]`, or we can `map` `join` over the outer `F` and then `join` the result of that.

This is saying that in "flattening" `F[F[F[A]]]` to `F[A]`, it should not matter whether we first join the two "inner" `F`s or the two "outer" `F`s. This is easy to see with the `List` monad. If we have a `List` of `List`s of `List`s, like this...

``` scala
val x: List[List[List[Int]]] =
  List(List(List(1,2), List(3,4)), List(List(5,6), List(7,8)))
```

...it should not matter whether we flatten the inner lists or the outer lists first:

```
scala> val y1 = x.flatten
y1: List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6), List(7, 8))

scala> val y2 = x.map(_.flatten)
y2: List[List[Int]] = List(List(1, 2, 3, 4), List(5, 6, 7, 8))

scala> y1.flatten
res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)

scala> y2.flatten
res1: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)
```

This is the same as saying that in the expression `((1 + 2) + (3 + 4)) + ((5 + 6) + (7 + 8))`, it doesn't matter whether we remove the inner brackets first to get `(1 + 2 + 3 + 4) + (5 + 6 + 7 + 8)` or the outer brackets first to get `(1 + 2) + (3 + 4) + (5 + 6) + (7 + 8)`. In both cases we end up with `1 + 2 + 3 + 4 + 5 + 6 + 7 + 8`. The reason it doesn't matter is that `+` is _associative_. So then it's easy to see how the monad law is an _associative law_.

The identity laws in terms of `join` are similarly simple:

``` scala
join(unit(x)) == x      // left identity
join(map(x)(unit)) == x // right identity
```

In terms of the list monad as above, the identity laws are saying that we can add brackets either on the inside or the outside. Whichever we do, `join` will behave the same way:

```
scala> val x = List(1,2,3)
x: List[Int] = List(1, 2, 3)

scala> List(x).flatten
res0: List[Int] = List(1, 2, 3)

scala> x.map(List(_)).flatten
res1: List[Int] = List(1, 2, 3)
```

### Monads in Category Theory

In [Category Theory](http://en.wikipedia.org/wiki/Category_theory), a [Monad](http://en.wikipedia.org/wiki/Monad_%28category_theory%29) is a [functor](http://en.wikipedia.org/wiki/Functor) equipped with a pair of [natural transformations](http://en.wikipedia.org/wiki/Natural_transformation) satisfying the laws of [associativity](http://en.wikipedia.org/wiki/Associativity) and [identity](http://en.wikipedia.org/wiki/Identity_%28mathematics%29).

What does this mean? If we restrict ourselves to the category of Scala types (with Scala types as the objects and functions as the arrows), we can state this in Scala terms.

A `Functor` is just a type constructor for which `map` can be implemented:

``` scala
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}
```

A _natural transformation_ from a functor `F` to a functor `G` is just a polymorphic function:

``` scala
trait Transform[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
```

The natural transformations that form a _monad_ for `F` are `unit` and `join`:

``` scala
type Id[A] = A

def unit[F](implicit F: Monad[F]) = new Transform[Id, F] {
  def apply(a: A): F[A] = F.unit(a)
}

def join[F](implicit F: Monad[F]) = new Transform[({type f[x] = F[F[x]]})#f, F] {
  def apply(ffa: F[F[A]]): F[A] = F.join(ffa)
}
```

### Monads and monoids

Monoids and monads are connected in various ways in category theory.

#### The monad for monoids

The `List` monad can be seen as a theory about monoids. Specifically, the `_.flatten` (monadic `join`) and `List(_)` (monadic `unit`) functions witness that we can add and remove parentheses in a monoid expression. That is, the parenthesization of an expression like `1 + 2 + 3 + 4` doesn't matter. The monad associativity law means that we can remove parentheses in any order we like, and the identity law means we can add them wherever we want.

See [Rúnar's article, *More on monoids and monads*](http://apocalisp.wordpress.com/2010/07/21/more-on-monoids-and-monads/) for more information about this connection.

#### Kleisli categories

Both monoids and monads form _categories_. In fact, we can see a category as a _generalized monoid_. Observe that with a monoid `M`, we can view each element of the monoid as a function of type `M => M`. For example, in the `Int` monoid with addition, these elements are `(_ + 0)`, `(_ + 1)`, `(_ + (-1))`, etc. Then the composition of these functions is the operation of the monoid. We can generalize this notion to consider not just the type `M`, but all types (`A`, `B`, `C`, etc.) and functions not just of type `M => M`, but `A => B` for any types `A` and `B`. Then ordinary function composition is an associative operation, with an identity element which is the `identity` function that just returns its argument. This more general notion does not form a monoid, but a _category_ which is more general. Specifically, it's the _category of Scala types with function composition_. Even more generally, whenever we have _arrows_ (a generalized notion of functions) whose composition is associative and has an identity element, we have a category.

A monad `F` can be described by what's called a [_Kleisli category_](http://en.wikipedia.org/wiki/Kleisli_category). The objects of this category are the ordinary Scala types, but the arrows are _Kleisli arrows_. That is, every arrow in this category is not of the general form `A => B`, but of the more specific form `A => F[B]`. The composition of these arrows is _Kleisli composition_ (given by the `compose` combinator in the chapter) and the identity for Kleisli composition is monadic `unit`. Every monad forms a Kleisli category in this way.

#### A monad is a monoid in a category of endofunctors

A monad is also a kind of monoid. If we think of a type like `(M, M) => M` as `M² => M` (taking 2 `M`s or the product of `M` with itself), and `M` as `1 => M` (where `1` is the `Unit` type), then we can think of a type like `F[F[A]] => F[A]` as `F²[A] => F[A]` or just `F² ~> F` (where `~>` denotes a [natural transformation](http://en.wikipedia.org/wiki/Natural_transformation)) and `A => F[A]` as `1[A] => F[A]` (where `1` is the identity functor) or just `1 ~> F`:

{width="narrow"}
|type        |zero/unit      |op/join   |
|------------|---------------|----------|
|`Monoid[M]` |`1 => M`       |`M² => M` |
|`Monad[F]`  |`1 ~> F`       |`F² ~> F` |

It's now clear that these are the same kind of thing, except `Monoid[M]` is operating in a category where the objects are Scala types and the arrows are Scala functions, and `Monad[F]` is operating in a category where the objects are Scala functors and the arrows are natural transformations.

See this StackOverflow question and its answers: [http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem](http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem).

### Reader monad

At the end of the chapter we have an exercise for the reader to implement the following monad:

``` scala
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}
```

This is the [reader monad](http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/). It is called that because it has the ability to _read_ a value of type `R`. In addition to the operations common to all monads (`flatMap`, `join`, `map`, `unit`, etc), it has a primitive operation, `read`:

``` scala
def read[R]: Reader[R, R] = Reader(r => r)
```

Note that this is just the identity function!

[In the Scalaz library, this operation is called `ask`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.MonadReader) and is generalized to any reader-like structure (any implementation of `MonadReader`) rather than being specific to `Reader`.

The meaning of `map` in `Reader` is function composition:

``` scala
def map[R,A,B](f: A => B): Reader[R, A] => Reader[R, B] =
  Reader(r => f compose r.run)
```

The meaning of `join` is to pass the same argument as both parameters to a binary function:

``` scala
def join[R,A](x: Reader[R, Reader[R, A]]): Reader[R, A] =
  Reader(r => x.run(r).run(r))
```

And the meaning of `unit` is to ignore the argument:

``` scala
def unit[R,A](a: A): Reader[R, A] = Reader(_ => a)
```

The reader monad subsumes (and is simpler than) [dependency injection](http://en.wikipedia.org/wiki/Dependency_injection). See Rúnar's talks on dependency injection with the reader monad:

["Dead-simple dependency inection", from the 2012 Northeast Scala Symposium in Boston](https://www.youtube.com/watch?v=ZasXwtTRkio)

["Lambda: the ultimate dependency injection framework", from the 2012 YOW! Summer Conference in Brisbane](http://yow.eventer.com/yow-2012-1012/lambda-the-ultimate-dependency-injection-framework-by-runar-bjarnason-1277)

See also Tony Morris's talk ["Dependency injection without the gymnastics"](http://vimeo.com/44502327) from ETE 2012.

### Eilenberg-Moore categories

Another categorical view of monads is through [Eilenberg-Moore categories](http://ncatlab.org/nlab/show/Eilenberg-Moore+category). The EM category of a monad is the category of its [algebras](https://www.fpcomplete.com/user/bartosz/understanding-algebras).

For example, the algebras for the `List` monad are Scala `Monoid`s. The EM category of the `List` monad is the category with monoids as its objects and [monoid morphisms](http://en.wikipedia.org/wiki/Monoid#Monoid_homomorphisms) (see chapter 10) as its arrows. 

In general, the EM category for a monad can be found by the following method (source: [Theory Lunch](http://theorylunch.wordpress.com/2013/06/06/an-initial-solution-to-the-monad-problem-and-then-some-more/#more-885)):

  1. An _`F`-algebra_ for the monad `F` is a type `A` together with a function `a: F[A] => A` such that `a(unit(x)) == x` and `a(join(x)) == a(map(x)(a))`.
  2. A morphism of `F`-algebras from an `F`-algebra `a: F[A] => A` to an `F`-algebra `b: F[B] => B` is a function `f: A => B` such that `b(map(x)(f)) == f(a(x))`.
  3. The Eilenberg-Moore category for the monad `F` is the category with `F`-algebras as objects, and morphisms between `F`-algebras as arrows. The identity arrow is just the `identity` function, and composition of arrows is ordinary function composition.

We can see how a `Monoid[A]` is precisely a `List`-algebra by this definition:

``` scala
def fold[A](implicit M: Monoid[A]): List[A] => A =
  _.foldRight(M.zero)(M.op)
```

It _is_ a `List`-algebra because `fold(List(x)) == x` (that is, putting something in a list and then folding that list is a no-op). And `fold(x.flatten) == fold(x.map(fold.apply))` (that is, concatenating a bunch of lists and folding the concatenated list is the same as folding a bunch of lists and then folding the list of the results of those folds).

This is a sense in which "`List` is the monad for monoids."

We can find the EM category for `Option` (thanks, Edward Kmett) easily. Every `Option`-algebra of type `Option[A] => A` is given by some value `a` of type `A` and is implemented by `_.getOrElse(a)`. So an object in the EM category for `Option` is a Scala type `A` together with a distinguished value `a:A`. An arrow in this category takes that value `a:A` to another value `b:B`. So it's a function `f: A => B` that satisfies `o.map(f).getOrElse(b) == f(o.getOrElse(a))` for all `o: Option[A]`. That is, it's just a function that returns `b` when given `a`. In other words, `Option` is the monad for [_pointed sets_](http://en.wikipedia.org/wiki/Pointed_set).

The EM category for the `Reader[R,_]` or `R => _` monad is the category with objects that are each given by a value `r:R`, implemented by `_(r)`.

The monad whose EM category is just the category of Scala types is `Id` (the identity monad).

### Adjunctions

An [_adjunction_](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Adjunction) consists of a pair of functors in a certain relationship to one another. Stated in `Scala`, it is an implementation of this interface:

``` scala
trait Adjunction[F[_], G[_]] {
  def unit[A](a: A): G[F[A]]
  def counit[A](fga: F[G[A]]): A

  def F: Functor[F]
  def G: Functor[G]
}
```

`counit` is pronounced "co-unit", not "cow-knit".

We say that `F` is _left adjoint_ to `G`, written {$$}F \dashv G{/$$} in mathematical notation.

There are two laws of adjunctions:

  1. `counit(F.map(x)(unit)) == x`
  2. `G.map(unit(x))(counit) == x`

Another way to view an adjunction is that there is an isomorphism between the types `F[A] => B` and `A => G[B]`:

``` scala
def leftAdjunct[A,B](k: F[A] => B): A => G[B] =
  a => G.map(unit(a))(k)
def rightAdjunct[A,B](k: A => G[B]): F[A] => B =
  fa => counit(F.map(fa)(k))
```

An adjunction has the property that `G[F[_]]` is a monad:

``` scala
def join[A](g: G[F[G[F[A]]]]): G[F[A]] =
  G.map(g)(counit)
def map[A,B](g: G[F[A]])(f: A => B): G[F[B]] =
  G.map(F.map(f))
def flatMap[A,B](g: G[F[A]])(f: A => G[F[B]]): G[F[B]] =
  join(map(g)(f))
```

For example, the `State` monad is formed by the adjoint functors `(_, S)` and `S => _`.

In fact, _every monad_ is formed by an adjunction.

### Comonads

[Dually](http://en.wikipedia.org/wiki/Dual_%28mathematics%29), the composite functor `F[G[A]]` is a [comonad](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Comonad). A comonad is exactly like a monad, except the direction of the function arrows is reversed:

``` scala
trait Comonad[F[_]] {
  def counit[A](a: F[A]): A
  def extend[A,B](a: F[A])(f: F[A] => B): F[B]
}
```

Instead of a `unit` that goes from `A` to `F[A]`, we have a `counit` that goes from `F[A]` to `A`. And instead of a `flatMap` that takes a function of type `A => F[B]`, we have `extend` that takes a function going the other way: `F[A] => B`.

A simple example of a comonad is the _reader_ comonad:

``` scala
type Coreader[R,A] = (A,R)

val readerComonad[R] = new Comonad[({type f[x] = Coreader[R,x]})#f] {
  def counit[A](a: (A,R)) = a._1
  def extend[A,B](a: (A,R))(f: (A,R) => B) = 
    (f(a), a._2)
}
```

How is this a _reader_? What is it reading? Well, there is a primitive `read` operation:

``` scala
def read[R](ar: (A,R)): R = ar._2
```

The reader comonad models computations that have a context (or configuration) of type `R`. The value computed so far is the `A`, and we can always read the context to copy it into our working memory. It supports the same operations as the reader monad and serves essentially the same purpose.

Note that unlike the reader _monad_ which is a function `R => A`, the reader _comonad_ is a pair `(A, R)`. The latter is _left adjoint_ to the former and their adjunction forms the `State` monad.

But their adjunction therefore also forms a comonad! The "dual" of the `State` monad in this sense is the `Store` comonad

``` scala
case class Store[S](s: S, f: S => A)
```

The `Store` comonad essentially models a Moore machine. The current state of the machine is `s`, and the output function is `f`. Note that the output depends only on the current state.

The type `A => Store[S, A]` is one possible representation of a [Lens](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.package$$Lens$).

Other useful comonads include [Rose Trees](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Tree), [Nonempty lists](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.NonEmptyList), [zippers over lists](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Zipper), [zippers over trees](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.TreeLoc), and [cofree comonads](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Cofree).

### Links

* [The essence of functional programming](http://www.eliza.ch/doc/wadler92essence_of_FP.pdf) by Philip Wadler.
* [Notions of computation and monads](http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf) by Eugenio Moggi.

## Notes on chapter 12: Applicative and traversable functors

### The cost of power

There is a tradeoff between applicative APIs and monadic ones. Monadic APIs are strictly more powerful and flexible, but the cost is a certain loss of algebraic reasoning.

The difference is easy to demonstrate in theory, but takes some experience to fully appreciate in practice.

Consider composition in a monad, via `compose` (Kleisli composition):

``` scala
val foo: A => F[B] = ???
val bar: B => F[C] = ???
val baz: A => F[C] = bar compose foo
```

There is no way that the implementation of the `compose` function in the `Monad[F]` instance can inspect the values `foo` and `bar`. They are functions, so the only way to "see inside" them is to give them arguments. The values of type `F[B]` and `F[C]` respectively are not determined until the composite function _runs_.

Contrast this with combining values with `map2`:

``` scala
val quux: F[A] = ???
val corge: F[B] = ???
val grault: F[C] = map2(quux, corge)(f)
```

Here the implementation of `map2` can actually look at the values `quux` and `corge`, and take different paths depending on what they are. For instance, it might rewrite them to a normal form for improved efficiency. If `F` is something like `Future`, it might decide to start immediately evaluating them on different threads. If the data type `F` is applicative but _not a monad_, then the implementation has this flexibility universally. There is then never any chance that an expression in `F` is going to involve functions of the form `A => F[B]` that it can't see inside of.

The lesson here is that power and flexibility in the interface often restricts power and flexibility in the implementation. And a more restricted interface often gives the implementation more options.

See [this StackOverflow question](http://stackoverflow.com/questions/7861903/what-are-the-benefits-of-applicative-parsing-over-monadic-parsing) for a discussion of the issue with regard to parsers.

See also the end of the note below on "Applicative laws", for an example of the loss of algebraic reasoning that comes with making an API monadic rather than applicative.

### Applicative laws

In the chapter, we decided to present the `Applicative` laws in terms of `map2`. We find that this works pretty well pedagogically. We used the following laws:

   * Left identity: `map2(unit(()), fa)((_,a) => a) == fa`
   * Right identity: `map2(fa, unit(()))((a,_) => a) == fa`
   * Associativity: `product(product(fa, fb),fc) == map(product(fa, product(fb, fc)))(assoc)`
   * Naturality: `map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))`

But there are other ways to state the laws for `Applicative`. Commonly the laws for applicative are stated in terms of `apply`, which is sometimes called _idiomatic function application_ (where the "idiom" is `F`):

``` scala
def apply[A,B](ff: F[A => B], fa: F[A]): F[B] =
  map2(ff, fa)(_(_))
```

The laws for `apply` are _identity_, _homomorphism_, _interchange_, and _composition_.

#### Identity law

The identity law for `apply` is stated as:

``` scala
apply(unit(id), v) == v
```

That is, `unit` of the identity function is an identity for `apply`.

#### Homomorphism law

The homomorphism law for `apply` is stated as:

``` scala
apply(unit(f), unit(x)) == unit(f(x))
```

In other words, idiomatic function application on `unit`s is the same as the `unit` of regular function application. In more precise words, `unit` is a homomorphism from `A` to `F[A]` with regard to function application.

#### Interchange law

The interchange law for `apply` is stated as:

``` scala
apply(u, unit(y)) == apply(unit(_(y)), u)
```

This law is essentially saying that `unit` is not allowed to carry an effect with regard to any implementation of our applicative functor. If one argument to `apply` is a `unit`, then the other can appear in either position. In other words, it should not matter when we evaluate a `unit`.

#### Composition

The composition law for `apply` is stated as:

``` scala
apply(u, apply(v, w)) == apply(apply(apply(unit(f => g => f compose g), u), v), w)
```

This is saying that applying `v` to `w` and then applying `u` to that is the same as applying composition to `u`, then `v`, and then applying the composite function to `w`. Intuitively it's saying the same as:

``` scala
u(v(w)) == (u compose v)(w)
```

We might state this law simply as: "function composition in an applicative functor works in the obvious way."

#### Applicative normal form

The applicative laws taken together can be seen as saying that we can rewrite any expression involving `unit` or `apply` (and therefore by extension `map2`), into a normal form having one of the following shapes:

``` scala
pure(x)          // for some x
map(x)(f)        // for some x and f
map2(x, y)(f)    // for some x, y, and f
map3(x, y, z)(f) // for some x, y, z, and f
// etc.
```

Where `f`, `x`, `y`, and `z` do not involve the `Applicative` primitives at all. That is, every expression in an applicative functor `A` can be seen as lifting some pure function `f` over a number of arguments in `A`.

Note that this reasoning is lost when the applicative happens to be a monad and the expressions involve `flatMap`. The applicative laws amount to saying that the arguments to `map`, `map2`, `map3`, etc can be reasoned about independently, and an expression like `flatMap(x)(f)` explicitly introduces a dependency (so that the result of `f` depends on `x`). See the note above on "The cost of power".

### Applicatives in Scalaz

[The Scalaz library](http://github.com/scalaz/scalaz) provides an [`Applicative` trait.](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Applicative). In this trait, `map2` et al are called `lift2`, `lift3`, and so on.

The `scalaz.syntax.applicative` object supplies implicit syntax for applicatives to lift a function of arbitrary arity:

``` scala
(x |@| y |@| z)(f)
```

This is equivalent to `lift3(x, y, z)(f)`.

### Traversable functors

For further reading on traversable functors, see:

[The Essence of the Iterator Pattern](http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator), by Jeremy Gibbons and Bruno Oliveira. Published in _Mathematically-Structured Functional Programming_, 2006.

[Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html), by Conor McBride and Ross Paterson. Published in _Journal of Functional Programming_, 2008.

[An Investigation of the Laws of Traversals](http://arxiv.org/pdf/1202.2919), by Mauro Jaskelioff and Ondrej Rypacek, published in _Mathematically-Structured Functional Programming_, 2012.

#### Laws of traversable functors

`Traverse[T[_]]` has two laws. There are many ways to state them, but here is one:

##### Identity law:

``` scala
sequence[Id,A](xs) == xs
```

That is, traversing in the identity applicative (`type Id[X] = X`) has no effect.

##### Fusion law:
  
``` scala
sequence[({type f[x] = F[G[x]]})#f, A](xs) ==
  map(sequence[F,G[A]](xs))(sequence[G,A])
```
  
That is, traversal in `F[_]` followed by traversal in `G[_]` can be fused into one traversal in the composite applicative `F[G[_]]`.

### Monad transformers

A _monad transformer_ is a data type that composes a particular monad with any other monad, giving us a composite monad that shares the behavior of both.

There is no general way of composing monads. Therefore we have to have a specific transformer for each monad.

For example, [`OptionT`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.OptionT) is a monad transformer that adds the behavior of `Option` to any other monad. The type `OptionT[M, A]` behaves like the composite monad `M[Option[_]]`. Its `flatMap` method binds over both the `M` and the `Option` inside, saving us from having to do the gymanstics of binding over both.

[Scalaz](http://github.com/scalaz/scalaz) provides many more monad transformers, including `StateT`, `WriterT`, `EitherT`, and `ReaderT` (also known as `Kleisli`).

### Links

* [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)

* [The essence of form abstraction](http://groups.inf.ed.ac.uk/links/formlets/) talks about writing compositional web forms using applicative functors.

* Brent Yorgey's [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) is a great resource on `Monad`, `Applicative`, `Traverse` and other type classes.

## Notes on chapter 13: External effects and I/O

Monads were discovered as a way of embedding effectful code into a pure language in the early 1990s--see the 1992 Phillip Wadler paper [The essence of functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps) and also [Imperative functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/imperative/imperative.ps) by Simon Peyton Jones (one of the creators of Haskell) and Wadler.

There are various ways of representing the `Free` data type we made use of here. The traditional formulation would be in terms of just two constructors, `Return` and `Suspend`:

``` scala
trait Free[F[_],A]
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F, A]]) extends Free[F,A]
```

This type forms a `Monad` given only a `Functor[F]`. See [Free Monads and the Yoneda Lemma](http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/) which talks about the relationship between these various formulations of `Free` and introduces some relevant category theory. 

### Performance of `Free`

The naive, two-constructor representation of `Free` above has quadratic complexity for left-associated sequences of `flatMap` (more standard terminology is "bind" rather than `flatMap`). For example:

``` scala
def f(i: Int): Free[F,Int] = ...
val acc: Free[F,Int] = ...

val bad = (0 until N).map(f).foldLeft(acc)(_.map2(_)(_ + _))
```

This requires repeated trips to the "end" of the increasingly long chain to attach a further `map2`. The first iteration requires 1 step, the next requires 2 steps, then 3 steps, and so on, up to `N`, leading to quadratic complexity. See [Reflection without remorse](http://homepages.cwi.nl/~ploeg/papers/zseq.pdf) which gives a very nice overview of the general problem, discusses historical solutions and their tradeoffs, and presents a new solution based on _type-aligned sequences_. Our solution given in the text rewrites binds to the right during [interpretation in the `runFree` function](https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L419), which assumes that the target `Monad` has efficient implementation for right-associated binds.

### Lack of first-class universal quantification in Scala

The `Translate` type we introduced in this chapter is a common idiom for getting around Scala's lack of support for _first-class, universally quantified values_. We'd like for `runFree` to just accept a `forall A . F[A] => G[A]` (this is not valid Scala syntax), but first-class functions and in fact all first-class values in Scala are monomorphic--any type parameters they mention must be fixed to some particular types. To get around this restriction, we defined `Translate[F,G]`:

``` scala
trait Translate[F[_],G[_]] {
  def apply[A](f: F[A]): G[A]
}
```

Although values must be monomorphic, _methods_ in Scala can of course be polymorphic, so we simply introduce a new first-class type containing a polymorphic method. Unfortunately, this means that various polymorphic methods we might have lying around must be explicitly wrapped in `Translate`:

``` scala
def headOption[A](a: List[A]): Option[A]
```

Even though `headOption` is polymorphic, we would need to explicitly wrap it in a `Translate[List,Option]` if we ever needed to use it polymorphically in a first-class way. Another drawback of this approach is that we require separate versions of `Translate` for different "shapes" of the input and output type constructors--for instance, in chapter 14, we define a type `RunnableST`, which would typically just be represented using a universally quantified ordinary value, without needing wrapping in a new type.

### Links

See [Free Monads and Free Monoids](http://blog.higher-order.com/blog/2013/08/20/free-monads-and-free-monoids/) for more information about what "free" means.

## Notes on chapter 14: Local effects and mutable state

The `ST` data type covered in this chapter was first introduced in [Lazy Functional State Threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.3299). The general idea, of using universal quantification to uniquely tag variables and enforce scoping, is also useful in other situations. For instance, we can use the same idea to enforce a 'safe' API for accessing file handles, preventing a file handle from being referenced after it passes out of scope. Here is a sketch of an API:

``` scala
trait Handle[R]

trait SafeIO[S,+A] {
  def flatMap[B](f: A => SafeIO[S,B]): SafeIO[S,B]
  def map[B](f: A => B): SafeIO[S,B]
}

object SafeIO {
  def unit[S,A](a: A): SafeIO[S,A]

  // obtain a file handle
  def handle[S](filename: String): SafeIO[S, Handle[S]]

  // read a number of bytes from a handle, safely
  def read[S](h: Handle[S], n: Int): SafeIO[S, Option[Array[Byte]]]

  // run a 'closed' `SafeIO` computation
  def run[A](io: RunIO[A]): IO[A]
}

/** A 'runnable' `SafeIO` must be universally quantified in `S`. */
trait RunIO[A] { def run[S]: SafeIO[S,A] }
```

By tagging `Handle` values with the scope in which they originate, we prevent running a `SafeIO` whose value incorporates a `Handle`, and we are also prevented from mixing `Handle` values originating in different scopes. Thus, any `Handle` values allocated during `run` may be safely closed when the `IO` action returned from `run` completes.  

In many situations where we might use universal quantification like this to enforce some notion of scoping, we can alternately "invert control". For instance, the need to enforce scoping in both `SafeIO` (and `ST`) comes about because we allow access to an underlying "handle" concept (for `ST`, we have `STRef` and `STArray`, which are handles to mutable references), and we want to ensure that these handles pass out of scope at some delimited location. We can instead choose to work with a different abstraction not based on handles at all. Rather than letting the computation pull data from a `Handle`, we can instead build up a "transducer" that gives abstract instructions for how to transform one input stream to another, without getting access to any underlying handle. For instance, the library we develop in chapter 15 provides access to files but the various consumers of our `Process` type don't deal directly with a `Handle` abstraction.

### Effect systems

Universal quantification is a very simple technique for enforcing effect scoping, and the `ST` type is easy to incorporate as a regular library into many functional languages. There are more sophisticated techniques for tracking effects, called _effect systems_. The general idea here is that we track the _effects_ of a value separate from its type. Effect systems often include natural subtyping of effects, so that, for instance, a pure computation (with no effects) can be passed as an argument without explicit wrapping to a function allowing for the effect of mutation to some variable or region of memory. See for example [Koka](http://www.rise4fun.com/koka/tutorial), [DDC](http://www.haskell.org/haskellwiki/DDC), and [Frank](http://cs.ioc.ee/efftt/mcbride-slides.pdf).

## Notes on chapter 15: Stream processing and incremental I/O

As mentioned in the text, an I/O monad is a kind of lowest common denominator for embedding externally-interpreted effects in a pure language--the model when programming _within_ `IO` is much the same as ordinary imperative programming. Hence, ever since the `IO` monad was first introduced in the early 1990s, functional programmers have been interested in finding more compositional ways of assembling programs that talk to the external world.

For a while, lazy I/O was still quite commonly used, despite its problems. Oleg Kiselyov, a prominent Haskell programmer and researcher, was very vocal in [pointing out the problems with lazy I/O](http://okmij.org/ftp/Haskell/Iteratee/Lazy-vs-correct.txt), and popularized the concept of _iteratees_, which is an early "ancestor" of the library we developed in this chapter. For more background, see [Oleg's page on stream processing](http://okmij.org/ftp/Streams.html). For a gentler exposition to iteratees, also see "Iteratee: Teaching an old fold new tricks" in [Issue 16 of The Monad Reader](http://themonadreader.files.wordpress.com/2010/05/issue16.pdf). [Oleg's site](http://okmij.org/ftp/) is a treasure trove of resources covering various aspects of FP, we highly recommend spending some time there.

In the past 5 years or so, there have been a number of variations developed on the basic idea of iteratees, generally aimed at making simpler to use and/or more expressive libraries. In Haskell, see the [conduit](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview) and [pipes](https://hackage.haskell.org/package/pipes-4.1.2/docs/Pipes-Tutorial.html) packages, as well as Edward Kmett's [machines](https://hackage.haskell.org/package/machines) package, which is the Haskell library which most closely related to the one in this chapter. 

There's been some pushback in the Haskell community that the conduit and pipes packages are overly complicated, and this has led to the development of the [io-streams](http://hackage.haskell.org/package/io-streams-1.0.1.0/docs/System-IO-Streams-Tutorial.html) package. The library is simpler in the sense that it is specialized to `IO` and bakes certain features directly into the basic stream types used, but in our opinion, this library isn't really a full replacement for a library like conduit, pipes, or machines. An important goal of a stream processing library is to allow for pure stream processing logic (often a majority of processing logic) to be defined separately from any I/O. Although having a more convenient abstraction for I/O streams is useful, more general purpose, abstract libraries are still important.

In the Scala world, the [scalaz-stream library](https://github.com/scalaz/scalaz-stream) developed out of work on [machines](https://hackage.haskell.org/package/machines) and the library developed here. Prior to that, there were ports of iteratees in the core [scalaz library](https://github.com/scalaz/scalaz).

### Functional reactive programming ###

Streaming processing and incremental I/O might not seem to have much to do with UI programming, but the problems have similarities. [Functional Reactive Programming](http://en.wikipedia.org/wiki/Functional_reactive_programming) (FRP) originated in the 1990s with work done by [Conal Elliott](http://conal.net/), Paul Hudak, and others (see Elliott's [list of publications](http://conal.net/papers/) and the 1997 paper [Functional Reactive Animation](http://conal.net/papers/icfp97/) by Elliott and Hudak). FRP is often put forward as a solution to the problem of describing interactive UIs in a functional way.

The FRP research has developed somewhat in parallel to the various approaches to streaming I/O mentioned above. According to Elliott, [what uniquely identifies FRP is the use of _continuous time_](http://conal.net/blog/posts/why-program-with-continuous-time), and on providing a simple, precise _denotation_ for the data types and various combinators. An FRP library is based around two types of signals:

* `Behavior[A]`: a time-varying `A` value, having a denotation `Time => A`.
* `Event[A]`: a discrete, time-varying sequence of `A` values, having a denotation of `List (Time, A)`.

These types (and their denotations) are deliberately abstract, which means that implementations are free to expose different sets of primitive combinators, and implementations are free to use very different implementation strategies, depending on what primitives are exposed to users. See [the chapter notes for chapter 9](Chapter-9:-Parser-combinators) for more about this style of design. For instance, the algebra might include the following functions:

* `def foldP[B,A](e: Event[A], z: B)(f: (B,A) => B): Event[B]` for left-folding the sequence of values represented by an `Event`.
* `def sample[A,B](e: Event[A], b: Behavior[B]): Event[B]` for sampling from a continuous behavior whenever an `Event` emits a value

Each of these operations can be given a clear interpretation in terms of the denotations of `Event` and `Behavior`, but implementations may use some more interesting representation of these types to facilitate efficient interpretation.

The FRP use of continuous time is important for much the same reasons that non-strictness is important. We saw in chapter 5 how non-strictness let us write more modular code, by decoupling the description of a computation (which may be infinite) from its evaluation. In the same way, continuous time lets us decouple the description of a time-varying program from any _sampling_ or _discretization_ that occurs at the end of the day when running our program.

In some ways, the denotations for `Event` and `Behavior` are a little _too_ flexible--there is no enforcement of _causality_, for instance--a `Behavior[A] => Behavior[B]` could in priciple let the value of the output behavior at time _t_ depend on the value of the input behavior at time _t + k_, essentially "looking into the future" to determine the value at the present. Elliott [discusses some of these problems in this post](http://conal.net/blog/posts/garbage-collecting-the-semantics-of-frp). The flexibility of the semantic model also means it is not immediately clear what actual algebra should be exposed to the programmer--we clearly want something expressive, but also limited "in the right ways" such that it is possible to implement efficiently. This is a challenging design problem in and of itself.

FRP is a deep area within functional programming. Here are just a few links to learn more:

* [Push-pull FRP](http://conal.net/papers/push-pull-frp/) by Elliott provides a nice, modern introduction to the ideas of FRP and discusses issues in crafting an efficient implementation. We also like [Heinrich Apfelmus' blog](http://apfelmus.nfshost.com/blog.html), which has a number of posts talking about FRP and various implementation issues.
* _Arrowized FRP (AFRP)_ arose in part because of the difficulties in formulating efficient implementations of the FRP model. In AFRP, behaviors and events are not first class, instead we have first-class signal transformers, generally based on some variation of the `Arrow` algebra. See [The Reactive Arcade](http://www.antonycourtney.com/pubs/hw03.pdf), which has an introduction to Yampa, an AFRP system, and also [Causal Commutative Arrows and their Optimization](http://cs-www.cs.yale.edu/c2/images/uploads/ICFP-CCA.pdf), which discusses ways of optimizing AFRP for efficient implementation.
* With both arrowized and traditional FRP, there are questions about the best way to allow for various forms of context-sensitivity and dynamic switching between signals (sometimes called "dynamic event switching"). The need for this can arise in many situations, but, for instance, it can be used for modeling UIs in which the user can add and remove new UI elements by interacting with existing UI elements on the page. In traditional FRP, use of dynamic event switching can make it easy to introduce _time leaks_, due to accidentally retaining the full history of an `Event`--Heinrich Apfelmus [discusses this issue here](http://apfelmus.nfshost.com/blog/2011/05/15-frp-dynamic-event-switching.html). And in AFRP, since the signals themselves are not first class, it isn't immediately obvious what the best way is to represent and interact with a dynamic, changing set of signals (though this issue has been also addressed in the AFRP line of research, see [Antony Courtney's thesis](http://www.antonycourtney.com/pubs/ac-thesis.pdf) pg 123, also the [Reactive Arcade](http://www.antonycourtney.com/pubs/hw03.pdf) and [FRP, continued](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf) papers).

Over time, the term "FRP" has been somewhat diluted, and the term is sometimes incorrectly used to refer to systems with discrete time, and even decidedly non-functional libraries making use of explicit callbacks and side effects!

