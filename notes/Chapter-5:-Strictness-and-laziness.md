## Notes on chapter 5: Strictness and laziness

### Non-strictness vs laziness

[The Haskell website](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict) has a good explanation of the difference between _non-strictness_ and _laziness_.

In short, "non-strict" just means "not strict". There are many possible [evaluation strategies](http://en.wikipedia.org/wiki/Evaluation_strategy) one could employ when evaluating a program, and [strict evaluation](http://en.wikipedia.org/wiki/Strict_evaluation) is one of them. [Non-strict evaluation](http://en.wikipedia.org/wiki/Non-strict_evaluation#Non-strict_evaluation) is a _class_ of evaluation strategies, and [lazy evaluation](http://en.wikipedia.org/wiki/Lazy_evaluation) is one non-strict strategy (also known as "call-by-need").

In Scala, non-strict arguments are sometimes called "by name" arguments, in reference to the fact that the evaluation strategy Scala employs for those arguments is [call-by-name](http://en.wikipedia.org/wiki/Call_by_name#Call_by_name). We can turn an argument into call-by-need by caching it in a `lazy val` inside the function:

~~~ scala
def pairIf[A](b: Boolean, x: => A) = {
  lazy val y = x
  if (b) (y, y)
}
~~~

This function will evaluate `x` only once, or never if the boolean `b` is `false`. If we said `(x, x)` instead of `(y, y)`, it would evaluate `x` twice.

The chapter explains that when an expression does not terminate, it is said to evaluate to _bottom_.  At first glance this is counter intuitive because there is a natural tendency to think of infinity as having _no_ bottom.  But the bottom to which the chapter refers is actually the [bottom type](http://en.wikipedia.org/wiki/Bottom_type). See Haskell's definition of [bottom](http://www.haskell.org/haskellwiki/Bottom) for a more thorough description. Scala refers to it as [Nothing](http://www.scala-lang.org/api/current/#scala.runtime.Nothing$), which is at the _bottom_ of the inheritance hierarchy.

### Corecursion and codata

The [Wikipedia article on corecursion](http://en.wikipedia.org/wiki/Corecursion) is a good starting point for understanding the concept.

The article on [Coinduction](http://en.wikipedia.org/wiki/Coinduction) has some further links. Dan Piponi's article ["Data and Codata"](http://blog.sigfpe.com/2007/07/data-and-codata.html) talks about corecursion as "guarded" recursion.

Ralf Hinze's paper ["Reasoning about Codata"](http://www.cs.ox.ac.uk/ralf.hinze/publications/CEFP09.pdf) brings equational reasoning to corecursive programs by employing applicative functors. Hinze's paper will be more comprehensible to readers who have finished part 3 of our book.

### Tying the knot

Non-strictness allows us to create cyclic streams such as:

~~~ scala
val cyclic: Stream[Int] = 0 #:: 1 #:: cyclic
~~~

This may seem like it shouldn't work. The stream is referencing itself in its own tail! But the trick is that the `#::` constructor is non-strict in its second argument. The evaluation of `cyclic` will stop without expanding the expression `1 #:: cyclic`. It's not until somebody takes the `tail` of the `tail` of `cyclic` that the recursive reference is expanded, and again it expands only one element at a time, allowing for an infinite, cyclic stream.

Note that the `cyclic` stream is reusing its own structure. `cyclic.tail.tail` is not a new stream that looks like `cyclic`. It really is the same object as `cyclic` in every sense:

~~~
scala> cyclic.tail.tail eq cyclic
res0: Boolean = true
~~~

This technique is sometimes called "Tying the Knot". For more information see the [Haskell.org article](http://www.haskell.org/haskellwiki/Tying_the_Knot).

However, be careful of creating such structures using the Scala standard library's `Stream`. They can be quite fragile. The reason is that `scala.Stream` is is strict in the head element and it will also [memoize](http://en.wikipedia.org/wiki/Memoization) the computed contents, which can lead to memory leaks.

### Stream.apply

Be careful using Stream.apply, both in the standard library and in the exercises: they are constructed using _repeated parameters_, which are always strict. This means that e.g.

~~~scala
Stream({println("One"); 1}, {println("Two"); 2}, {println("Three"); 3})
~~~ 

will immediately print One, Two, Three. Although the Stream will be constructed lazily, the contents have already been evaluated. 

For truly lazily constructed Streams you can always resort to `#::` (which still evaluates the head value strictly!) or nested cons(..,cons(..,..)) operators in the exercises. 

