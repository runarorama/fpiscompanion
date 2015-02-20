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

