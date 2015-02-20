## Notes on chapter 7: Purely functional parallelism

FP has a long history of using combinator libraries for expressing parallelism, and there are a lot of variations of the general idea. The main design choices in this sort of library are around how explicit to make the _forking_ and _joining_ of parallel computations. That is, should the API force the programmer to be fully explicit about when parallel tasks are being forked off into a separate logical thread, or should this be done automatically? And similarly, when waiting for the results of multiple logical threads (say, for the implementation of `map2`), should the order of these joins be something the programmer explicitly specifies or chosen by the framework? 

The library we developed in this chapter sits somewhere in the middle--it is explicit about where tasks are forked, but not when tasks are joined (notice that `map2` picks the order it waits for the two tasks whose results it is combining). The join order can be made more explicit. Simon Marlow, one of the [GHC Haskell](http://www.haskell.org/ghc/) developers, discusses this in [Parallel programming in Haskell with explicit futures](http://ghcmutterings.wordpress.com/2010/08/20/parallel-programming-in-haskell-with-explicit-futures/). Also see the full paper, [Seq no more: Better Strategies for Parallel Haskell](http://www.haskell.org/~simonmar/papers/strategies.pdf), which does a nice job of explaining some of the history of approaches for parallelism in Haskell.

Note that because Scala is a strict-by-default language, being more explicit about the join order isn't necessarily as beneficial as in Haskell. That is, we can get away with reasoning about join order much like we think about evaluation in normal strict function application.

This style of library isn't particularly good at expressing _pipeline parallelism_ that's possible when transforming streams of data. For instance, if we have a `Stream[Int]` of 10000 items, and we wish to square each value, then compute a running sum of the squared values, there is a potential for parallelism--as we are squaring values, we can be passing the squared values off to another consumer that is emitting the running sum of the values it receives. We'll be discussing stream processing and pipeline parallelism more in part 4. 

### Notes about map fusion ###

We noted in this chapter that one of our laws for `Par`, sometimes called _map fusion_, can be used as an optimization:

~~~ scala
map(map(y)(g))(f) == map(y)(f compose g)
~~~

That is, rather than spawning a separate parallel computation to compute the second mapping, we can fold it into the first mapping. We mentioned that our representation of `Par` doesn't allow for this, as it's too 'opaque'. If we make `Par` a proper data type and give it constructors that we can pattern match on, then it's easy to implement map fusion:

~~~ scala
trait Par[+A] {
  def map[B](f: A => B): Par[B] = this match {
    case MapPar(p, g) => MapPar(p, g andThen f)
    case _ => MapPar(
  }
case class MapPar[A,+B](par: Par[A], f: A => B) extends Par[B]
~~~

Baking ad hoc optimization rules like this into our data type works, but it can sometimes get unwieldy, and it's not very modular (we don't get to reuse code if there's some other data type needing similar optimizations). There are various ways of factoring out these sorts of optimizations so our core data type (be it `Par` or some other type) stays clean, and the optimization is handled as a separate concern. Edward Kmett has a nice [blog series discussing this approach](http://comonad.com/reader/2011/free-monads-for-less/). Before embarking on that series you'll need to be familiar with the content in part 3 of this book, and you should read [the Haskell appendix](https://github.com/pchiusano/fpinscala/wiki/A-brief-introduction-to-Haskell,-and-why-it-matters) as well.

