## Notes on chapter 14: Local effects and mutable state

The `ST` data type covered in this chapter was first introduced in [Lazy Functional State Threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.3299). The general idea, of using universal quantification to uniquely tag variables and enforce scoping, is also useful in other situations. For instance, we can use the same idea to enforce a 'safe' API for accessing file handles, preventing a file handle from being referenced after it passes out of scope. Here is a sketch of an API:

~~~ scala
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
~~~

By tagging `Handle` values with the scope in which they originate, we prevent running a `SafeIO` whose value incorporates a `Handle`, and we are also prevented from mixing `Handle` values originating in different scopes. Thus, any `Handle` values allocated during `run` may be safely closed when the `IO` action returned from `run` completes.  

In many situations where we might use universal quantification like this to enforce some notion of scoping, we can alternately "invert control". For instance, the need to enforce scoping in both `SafeIO` (and `ST`) comes about because we allow access to an underlying "handle" concept (for `ST`, we have `STRef` and `STArray`, which are handles to mutable references), and we want to ensure that these handles pass out of scope at some delimited location. We can instead choose to work with a different abstraction not based on handles at all. Rather than letting the computation pull data from a `Handle`, we can instead build up a "transducer" that gives abstract instructions for how to transform one input stream to another, without getting access to any underlying handle. For instance, the library we develop in chapter 15 provides access to files but the various consumers of our `Process` type don't deal directly with a `Handle` abstraction.

### Effect systems

Universal quantification is a very simple technique for enforcing effect scoping, and the `ST` type is easy to incorporate as a regular library into many functional languages. There are more sophisticated techniques for tracking effects, called _effect systems_. The general idea here is that we track the _effects_ of a value separate from its type. Effect systems often include natural subtyping of effects, so that, for instance, a pure computation (with no effects) can be passed as an argument without explicit wrapping to a function allowing for the effect of mutation to some variable or region of memory. See for example [Koka](http://www.rise4fun.com/koka/tutorial), [DDC](http://www.haskell.org/haskellwiki/DDC), and [Frank](http://cs.ioc.ee/efftt/mcbride-slides.pdf).

