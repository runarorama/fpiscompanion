## Notes on chapter 13: External effects and I/O

Monads were discovered as a way of embedding effectful code into a pure language in the early 1990s--see the 1992 Phillip Wadler paper [The essence of functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps) and also [Imperative functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/imperative/imperative.ps) by Simon Peyton Jones (one of the creators of Haskell) and Wadler.

There are various ways of representing the `Free` data type we made use of here. The traditional formulation would be in terms of just two constructors, `Return` and `Suspend`:

~~~ scala
trait Free[F[_],A]
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F, A]]) extends Free[F,A]
~~~

This type forms a `Monad` given only a `Functor[F]`. See [Free Monads and the Yoneda Lemma](http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/) which talks about the relationship between these various formulations of `Free` and introduces some relevant category theory. 

### Performance of `Free`

The naive, two-constructor representation of `Free` above has quadratic complexity for left-associated sequences of `flatMap` (more standard terminology is "bind" rather than `flatMap`). For example:

~~~ scala
def f(i: Int): Free[F,Int] = ...
val acc: Free[F,Int] = ...

val bad = (0 until N).map(f).foldLeft(acc)(_.map2(_)(_ + _))
~~~

This requires repeated trips to the "end" of the increasingly long chain to attach a further `map2`. The first iteration requires 1 step, the next requires 2 steps, then 3 steps, and so on, up to `N`, leading to quadratic complexity. See [Reflection without remorse](http://homepages.cwi.nl/~ploeg/papers/zseq.pdf) which gives a very nice overview of the general problem, discusses historical solutions and their tradeoffs, and presents a new solution based on _type-aligned sequences_. Our solution given in the text rewrites binds to the right during [interpretation in the `runFree` function](https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L419), which assumes that the target `Monad` has efficient implementation for right-associated binds.

### Lack of first-class universal quantification in Scala

The `Translate` type we introduced in this chapter is a common idiom for getting around Scala's lack of support for _first-class, universally quantified values_. We'd like for `runFree` to just accept a `forall A . F[A] => G[A]` (this is not valid Scala syntax), but first-class functions and in fact all first-class values in Scala are monomorphic--any type parameters they mention must be fixed to some particular types. To get around this restriction, we defined `Translate[F,G]`:

~~~ scala
trait Translate[F[_],G[_]] {
  def apply[A](f: F[A]): G[A]
}
~~~

Although values must be monomorphic, _methods_ in Scala can of course be polymorphic, so we simply introduce a new first-class type containing a polymorphic method. Unfortunately, this means that various polymorphic methods we might have lying around must be explicitly wrapped in `Translate`:

~~~ scala
def headOption[A](a: List[A]): Option[A]
~~~

Even though `headOption` is polymorphic, we would need to explicitly wrap it in a `Translate[List,Option]` if we ever needed to use it polymorphically in a first-class way. Another drawback of this approach is that we require separate versions of `Translate` for different "shapes" of the input and output type constructors--for instance, in chapter 14, we define a type `RunnableST`, which would typically just be represented using a universally quantified ordinary value, without needing wrapping in a new type.

### Links

See [Free Monads and Free Monoids](http://blog.higher-order.com/blog/2013/08/20/free-monads-and-free-monoids/) for more information about what "free" means.

