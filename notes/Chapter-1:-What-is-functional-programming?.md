## Notes on chapter 1: What is functional programming?

See the Wikipedia articles on [functional programming](http://en.wikipedia.org/wiki/Functional_programming) and [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_%28computer_science%29).

### Why functional programming matters 

In this chapter we highlight some of the benefits of functional programming. A classic article that gives some justification for FP is John Hughes's [Why Functional Programming Matters](http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf).

### Referential transparency and side effects 

We introduce functional programming as programming with _referentially transparent_ expressions. Our definition of referential transparency in the chapter is a little bit simplistic, but it serves our purposes for the time being. We discuss a more expanded notion of referential transparency in chapter 14 on "local effects".

A subtlety in the definition of RT given in the chapter is the meaning of "without affecting the meaning". What is the _meaning_ of a program? To answer this, we need to consider the program _with regard to_ some evaluator, or in the context of some other program. That is, the meaning of a program depends very much on how we interpret or evaluate it, and whether some effect of evaluation is to be considered a _side_ effect depends on the observer. For example, the fact that memory allocations occur as a side effect of data construction is not something we usually care to track or are even able to observe on the JVM. So ultimately what we consider to break referential transparency depends very much on what we can or care to observe.

See for example [What purity is and isn't](http://blog.higher-order.com/blog/2012/09/13/what-purity-is-and-isnt/) for a discussion of purity with regard to an evaluator, and [Is network I/O always worth tracking?](http://pchiusano.github.io/2014-05-21/what-effects-are-worth-tracking.html) for a more philosophical take on what constitutes a side effect.

