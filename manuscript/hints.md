# Hints for exercises

This section contains hints for various exercises in the book, to help you get started in coming up with an answer, or to get you unstuck if you run into trouble.

Not all exercises have hints. If you think of a good hint for an exercise and would like it to be available to other readers, submit a pull request to [the book's GitHub repository.](https://github.com/fpinscala/fpinscala) The electronic version of these hints is available under the `answerkey` directory in that repository.

## Hints for exercises in chapter 2

### Exercise 2.01

You will definitely need a helper method like we did with `factorial`. But think about what information you need at each iteration. You might need two values, one for each of the two numbers you need to calculate the next number. And you know the first two numbers already.

Note that the nth Fibonacci number has a closed form solution (see http://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression). Using that would be cheating; the point here is just to get some practice writing loops with tail-recursive functions.

### Exercise 2.02

You know the array is not sorted as soon as you encounter two adjacent elements for which `gt(first, second)`
returns true (note that equal adjacent elements are in order).

### Exercise 2.03

You have to take an argument of type `A` and return a function of type `B => C`. That function has to take an argument of type `B` and return a value of type `C`. Follow the types.

### Exercise 2.04

You want to return a binary function, so start by taking two arguments. You will have to pass those arguments to `f` one at a time.

### Exercise 2.05

You need to return a new function of type `A => C`. Start by accepting an argument of type `A`. Now follow the types. You have an `A`. What can you do with it? Do you have a function that accepts an `A`?

## Hints for exercises in chapter 3

### Exercise 3.02

Try pattern matching on `l`.

### Exercise 3.04

What should the function do if the `n` argument is 0?
What should it do if the list is empty?
What if the list is not empty and `n` is nonzero?
Consider all of these cases.
Use pattern-matching and recursion.


### Exercise 3.05

What should the function do if the list is empty?
What if it's not empty?
Use pattern-matching and recursion.


### Exercise 3.07

Look at the program trace from the previous example. Based on the trace,
is it possible the function supplied could choose to terminate the recursion early?


### Exercise 3.08

The first step in the trace is `Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))`

### Exercise 3.13

It's possible to do both directions. For your `foldLeft` in terms of `foldRight`,
you must build up, using `foldRight`, some value that you can use to achieve the effect
of `foldLeft`. (It won't be the `B` of the return type necessarily)


### Exercise 3.15

Use `foldRight`.

### Exercise 3.16

Try using `foldRight`. You shouldn't need to resort to an explicitly recursive function.

### Exercise 3.17

Again, try using `foldRight`. You shouldn't need to resort to an explicitly recursive function.

### Exercise 3.18

Again, try using `foldRight`. You shouldn't need to resort to an explicitly recursive function.

### Exercise 3.19

Again, try using `foldRight`!

### Exercise 3.20

You should be able to use a combination of existing functions.

### Exercise 3.28

The signature is `def map[A,B](t: Tree[A])(f: A => B): Tree[B]`.

### Exercise 3.29

The signature is `def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B`. See if you can define this function, then reimplement the functions you've already written for `Tree`.

## Hints for exercises in chapter 4

### Exercise 4.03

Use the `flatMap` and possibly `map` methods.

### Exercise 4.04

Break the list out using pattern-matching where there will be a recursive call to `sequence` in the cons case. Alternatively, use the `foldRight` method to take care of the recursion for you.

### Exercise 4.05

The `traverse` function can be written with explicit recursion or use `foldRight` to do the recursion for you. Implementing `sequence` using `traverse` may be more trivial than you think.

### Exercise 4.06

The `map2` function that we wrote earlier for `Option` will follow the same pattern for `Either`.

### Exercise 4.07

The signature of `traverse` is `def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]]`, and the signature of `sequence` is `def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]]`. In your implementation, you can pattern-match the list and use explicit recursion or use `foldRight` to perform the recursion for you.

## Hints for exercises in chapter 5

### Exercise 5.02

Many `Stream` functions can start by pattern matching on the `Stream` and considering what to do in each of the two cases. This particular function needs to first consider whether it needs to look at the stream at all.

### Exercise 5.04

Use `foldRight`.

### Exercise 5.06

Let `None: Option[A]` be the first argument to `foldRight`. Follow the types from there.

### Exercise 5.14

Try to avoid using explicit recursion. Use `zipAll` and `takeWhile`.

### Exercise 5.15

Try `unfold` with `this` as the starting state. You may want to handle emitting the empty `Stream` at the end as a special case.

### Exercise 5.16

The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

## Hints for exercises in chapter 6

### Exercise 6.02

Use `nonNegativeInt` to generate a random integer between `0` and `Int.MaxValue`, inclusive. Then map that to the range of doubles from 0 to 1.

### Exercise 6.05

This is an application of `map` over `nonNegativeInt` or `nextInt`.

### Exercise 6.06

Start by accepting an RNG. Note that you have a choice in which RNG to pass to which function, and in what order. Think about what you expect the behavior to be, and whether your implementation meets that expectation.

### Exercise 6.07

You need to recursively iterate over the list. Remember that you can use `foldLeft` or `foldRight` instead of writing a recursive definition. You can also reuse the `map2` function you just wrote. As a test case for your implementation, we should expect `sequence(List(unit(1), unit(2), unit(3)))(r)._1` to return `List(1, 2, 3)`.

### Exercise 6.08

The implementation using `flatMap` will be almost identical to the failed one where we tried to use `map`.

### Exercise 6.10

Use the specialized functions for `Rand` as inspiration.

## Hints for exercises in chapter 7

### Exercise 7.01

The function shouldn't require that the two `Par` inputs have the same type.

### Exercise 7.02

What if `run` were backed by a `java.util.concurrent.ExecutorService`? You may want to spend some time looking through the `java.util.concurrent` package to see what other useful things you can find.

### Exercise 7.03

In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating one future, then subtracts that time from the available time allocated for evaluating the other future.

### Exercise 7.05

One possible implementation will be very similar in structure to a function we've implemented previously, for `Option`.

### Exercise 7.08

There is a problem is with fixed size thread pools. What happens if the thread pool is bounded to be of exactly size 1?

### Exercise 7.10

Try adding a second continuation argument to `Future.apply`, which takes an error handler.

## Hints for exercises in chapter 8

### Exercise 8.01

When thinking of properties a function should satisfy, it often helps to consider inputs that have some structure that is easy to describe. A list where all the elements are the same is one simple structure.

### Exercise 8.03

We can refer to the enclosing `Prop` instance with `Prop.this`

### Exercise 8.12

Use the `listOfN` function you wrote before.

### Exercise 8.13

You can use a sized generator.

### Exercise 8.15

You will need to add to the representation of `Gen`. For example, `Gen[Int]` should be capable of generating random integers as well as generating a stream of all the integers from `Int.MinValue` to `Int.MaxValue`. You may want to have the behavior depend on how many test cases were requested.

### Exercise 8.17

Use the `Gen[Par[Int]]` generator from the last exercise.

### Exercise 8.19

If we are just looking at the random case, one way to have the generated `Int` depend on the `String` might be to set the seed of a new random number generator to be equal to the `hashCode` of the given input `String`.

### Exercise 8.22

Use the `Gen[Par[Int]]` generator from the last exercise.

### Exercise 8.25

If we are just looking at the random case, one way to have the generated `Int` depend on the `String` might be to set the seed of a new random number generator to be equal to the `hashCode` of the given input `String`.

## Hints for exercises in chapter 9

### Exercise 9.01

Try mapping over the result of `product`.

### Exercise 9.02

Multiplication of numbers is associative, `a * (b * c) == (a * b) * c`. Is there an analogous property for parsers? What can you say about the relationship between `map` and `product`?



### Exercise 9.06

Given a string of digits, `s`, you can use `s.toInt` to convert that to an `Int`.


### Exercise 9.07

Use `flatMap` and `succeed`.


### Exercise 9.09

For the tokens of your grammar, it's often a good idea to skip any trailing whitespace, to avoid having to deal with whitespace everywhere in your grammar. Try introducing a combinator for this. 

When sequencing parsers with `**`, it's common to want to ignore one of the parsers in the sequence, and you'll probably want to introduce combinators for this.

### Exercise 9.11

Here are two options: we could return the most recent error in the `or` chain, or we could return whichever error occurred after getting furthest into the input string.

### Exercise 9.14

You may want `string` to report the immediate cause of failure (whichever character didn't match), as well as the overall string being parsed.

### Exercise 9.17

Try adding another piece of state to `Location`, `isSliced`. You may want to rename `Location` to `ParseState`, as it's no longer just the location!

### Exercise 9.18

You can add an attribute `otherFailures: List[ParseError]` on `ParseError` itself. This will be a list of parse errors that occurred in other branches of the parser.

## Hints for exercises in chapter 10

### Exercise 10.02

Because we are abstract in the type parameter `A`, we are limited in the number of possible implementations. But there's more than one implementation that meets the monoid laws.

### Exercise 10.03

Again we are limited in the number of ways we can combine values with `op` since it should compose functions of type `A => A` for _any_ choice of `A`. And again there is more than one possible implementation. There is only one possible `zero` though.

### Exercise 10.04

You will need to generate three values of type `A` for testing associativity. Write a new `Gen` combinator for this if necessary.

### Exercise 10.05

You can `map` and then `concatenate`, but that will go over the list twice. Use a single fold instead.

### Exercise 10.06

Notice that the type of the function that is passed to `foldRight` is `(A, B) => B`, which can be curried to `A => (B => B)`. This is a strong hint that we should use the endofunction monoid `B => B` to implement `foldRight`. The implementation of `foldLeft` is then just the dual. Don't worry if these implementations are not very efficient.

### Exercise 10.07

The sequences of lengths 0 and 1 are special cases to consider.

### Exercise 10.08

Think about what a partial answer looks like. If we've only seen some of the elements of a sequence, we need to know if what we have seen so far is ordered. For every new element we see, if the sequence is in fact ordered, it should not fall inside the range of elements seen already.

### Exercise 10.09

Try creating a data type which tracks the _interval_ of the values in a given segment, as well as whether an 'unordered segment' has been found.
When merging the values for two segments, think about how these two pieces of information should be updated.


### Exercise 10.10

A `Stub` should never contain any whitespace.


### Exercise 10.11

You can write default implementations on the `Foldable` trait an then `override` them as necessary.


### Exercise 10.18

Use `mapMergeMonoid` and `intAddition`.

## Hints for exercises in chapter 11

### Exercise 11.01

You have already defined `unit` and `flatMap` for these types. The solution is to simply call them from your `Monad` implementation.

### Exercise 11.02

Since `State` is a binary type constructor, we need to partially apply it with the `S` type argument. Thus, it is not just one monad, but an entire family of monads, one for each type `S`. You need to devise a way of capturing the type `S` in a type-level scope and providing a partially applied `State` type in that scope.

### Exercise 11.03

These implementations should be very similar to implementations from previous chapters, only with more general types, and using the functions on the `Monad` trait. Make use of `unit` and `map2`.

### Exercise 11.04

There is more than one way of writing this function. For example, try starting with a `List[F[A]]` of length `n`.

### Exercise 11.06

You can start by pattern matching on the argument. If the list is empty, our only choice is to return `unit(Nil)`

### Exercise 11.07

Follow the types. There is only one possible implementation.

### Exercise 11.08

Look at the signature of `compose`. What happens if `A` is `Unit`?

### Exercise 11.09

You want to show that these two are equivalent:

flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
compose(compose(f, g), h) == compose(f, compose(g, h))

Rewrite one in terms of the other.

### Exercise 11.12

Follow the types here. Remember that `A` can be _any type at all_, including the type `F[B]` for some type `B`.

### Exercise 11.13

Join is sometimes called "flatten", and `flatMap` "maps and then flattens".

### Exercise 11.14

Rewrite the monad laws stated in terms of `flatMap` by substituting your implementation of `join`.

### Exercise 11.19

What would you expect `getState` to return right after you call `setState`?

### Exercise 11.20

This monad is very similar to the `State` monad, except that it's "read-only". You can "get" but not "set" the `R` value that `flatMap` carries along.

## Hints for exercises in chapter 12

### Exercise 12.02

To implement `map2` in terms of `apply`, try using `f.curried` and following the types.

### Exercise 12.03

Look at your implementation of `map2` in terms of `apply` and try to follow the same pattern.

### Exercise 12.04

Try it yourself in the REPL with some small examples.

### Exercise 12.05

You can write `flatMap` using pattern matching.

### Exercise 12.06

Implement `map2` using pattern matching. If both sides are a failure, try to keep the order of failures consistent.

### Exercise 12.07

Implement `map2` in terms of `flatMap`. Start with each applicative law in turn, then substitute equals for equals and apply the monad laws until you get an equation that is obviously true.

### Exercise 12.08

Follow the types.

### Exercise 12.09

The definition of `map2` is very short. The only things you can do are `map2` and `unit` from the `F` and `G` applicatives. Follow the types.

### Exercise 12.10

You will find this very difficult without an interactive proof assistant like Coq or Agda. If you do decide to take on the challenge, this is the kind of problem that might take someone several days or even a few weeks to think about.

### Exercise 12.12

The standard library lets you treat a `Map` as essentially a list of pairs.

### Exercise 12.13

Follow the types. There is generally only one sensible implementation that typechecks.

### Exercise 12.14

What happens if you call `traverse` with `Applicative[Option]`? Is there an even simpler `Applicative` you could use?

### Exercise 12.16

We need to use a stack. Fortunately a `List` is the same thing as a stack, and we already know how to turn any traversable into a list!

### Exercise 12.17

This implementation is very similar to `toList` except instead of accumulating into a list, we are accumulating into a `B` using the `f` function.

### Exercise 12.19

Follow the types. There is only implementation that typechecks.

### Exercise 12.20

Follow the types. There is only implementation that typechecks.

## Hints for exercises in chapter 13

### Exercise 13.02

Pattern this after the TailRec interpreter we gave in the text earlier

### Exercise 13.04

To define translate, use `runFree` with `Free[Function0,_]` as the target monad. Then use the specialized `runTrampoline` function written earlier.

### Exercise 13.05

Use `Par.async[Either[Throwable,Array[Byte]]] { cb => ... }`. Follow the types - what is the type of `cb` here and how can you use it?

## Hints for exercises in chapter 15

### Exercise 15.03

You'll need to use a local helper function which accepts the current sum and count.

### Exercise 15.11

Use `await`


