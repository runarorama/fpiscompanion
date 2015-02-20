## Notes on chapter 2: Getting started

We assume in this chapter that you have the Scala compiler and interpreter already up and running. See [the documentation page of Scala's website](http://www.scala-lang.org/documentation/) for more details about how to get Scala set up and links to lots of supplementary material about Scala itself.

### Factorial and Fibonacci ###

We also assume some familiarity with the [factorial function](http://en.wikipedia.org/wiki/Factorial) and we give only a brief explanation of the [Fibonacci sequence](http://en.wikipedia.org/wiki/Fibonacci_number).

I'd suggest that when asking for the nth fibonacci number it should be based from 1, not 0 like the current solution. i.e.

~~~ scala
def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int =
      if (n <= 1) prev   //instead of n == 0. Really n at 0 should be undefined but I'm just returning 0 here...
      else loop(n-1, curr, prev + curr)
      
    loop(n, 0, 1)
  }
~~~

### Lambda calculus ###

The notions of first-class and higher-order functions are formalized by the [lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus).

### Parametric polymorphism ###

For more on parametric polymorphism, see [the Wikipedia article.](http://en.wikipedia.org/wiki/Type_variable)

### Parametricity ###

When we can "follow the type" of a function to derive the only possible implementation, we say that the definition is _given by parametricity_. See [the Wikipedia article on parametricity](http://en.wikipedia.org/wiki/Parametricity), and Philip Wadler's paper [Theorems for free!](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html)

### Curry ###

The idea of [Currying](http://en.wikipedia.org/wiki/Currying) is named after the mathematician [Haskell Curry.](http://en.wikipedia.org/wiki/Haskell_Curry) He also discovered one of the most important results in computer science, the [Curry-Howard isomorphism](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) which says that _a program is a logical proof, and the hypothesis that it proves is its type_.

### Function composition ###

Function composition in functional programming is closely related to [function composition in mathematics.](http://en.wikipedia.org/wiki/Function_composition)

## FAQ

#### Are tail calls optimized if the `@annotation.tailrec` annotation isn't there?
They are still optimized, but the compiler won't warn you if it can't do the tail call optimization. 

#### Is there a list of other annotation types somewhere? 
See the [Scaladoc for the Annotation class](http://www.scala-lang.org/api/current/index.html#scala.annotation.Annotation), and expand the 'known subclasses section'. 

#### Is the common style to define loops using local function, rather than a (private) standalone function? 
Yes, this is much more common. There's no need to pollute the namespace with helper functions you aren't expecting to be called by anyone. 

#### Is `a || go(x)` considered a tail call? What about `a && go(x)`?
Yes
