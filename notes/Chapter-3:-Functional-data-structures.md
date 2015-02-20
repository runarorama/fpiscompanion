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

* [The Algebra of Algebraic Data Types](http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/) by Chris Taylor.
* [Species and Functors and Types, Oh My!](http://www.cis.upenn.edu/~byorgey/papers/species-pearl.pdf) by Brent Yorgey
* [Clowns to the left of me, jokers to the right](http://personal.cis.strath.ac.uk/~conor/Dissect.pdf) by Conor McBride

### Zippers ###

Since an algebraic data type is a type-level function involving sums and products, we can take the derivative of such a function, yielding a data structure called a [zipper](http://en.wikipedia.org/wiki/Zipper_%28data_structure%29). The zipper for a given structure is like the original structure, but with a movable "focus" or pointer into the structure. This can be used to insert, remove, and modify elements under the focus.

For example, a [list zipper](http://eed3si9n.com/learning-scalaz/Zipper.html) consists of one element under focus together with two lists: one enumerating all the elements to the left of the focus, and another enumerating all the elements to the right of the focus. The focus can me moved left or right (like a zipper on a piece of clothing) and elements will move through the focus. The element under focus can then be removed or modified, and we can insert a new element by consing onto the lists to its left or right.

### Type inference ###

Scala's type system is complicated by the presence of path-dependent types and subtyping. As a result Scala has only very limited, _local_ type inference.

Whereas other programming languages like Haskell or ML may have some species of [Hindley-Milner](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type inference, Scala has what's called "flow-based" inference. That is, type information "flows" from arguments to results. Given the types of the arguments, Scala is able to infer the result type of a function, unless that function is recursive. This also goes for values that are not functions, which can be considered 0-argument functions for the purpose of this discussion. 

We gain type inference benefits from grouping arguments into two argument lists, as in `xs.foldRight(0)(_ + _)`. Type information flows from the first argument list to the second when inferring type arguments to a function call. Note that no inference benefit can be gained from adding more than two argument lists to a function. When inferring the type arguments to a function call, Scala's typer does not consult any argument lists beyond the first.

See the [Scala Language Specification](http://www.scala-lang.org/docu/files/ScalaReference.pdf) for more information on Scala's type inference. Specifically sections 4.6.4 (Method Return Type Inference), 6.26.4 (Local Type Inference), and 8.3 (Type Parameter Inference In Patterns).
## <a id="s4"/> Links

* [Object-Oriented Programming Versus
Abstract Data Types](http://www.cs.utexas.edu/users/wcook/papers/OOPvsADT/CookOOPvsADT90.pdf)


### FAQ

#### Why do you declare `Nil` as a `case object` instead of a `case class` within the definition of our functional `sealed trait List`?

`case object` is more appropriate because `Nil` is a singleton. We can still use pattern matching in this case. However, there won't be a companion object with `apply`, `unapply`, etc. `case class Nil` will actually cause an error because case classes require an explicit parameter list:

~~~
<scala> case class Nil
<console>:1: error: case classes without a parameter list are not allowed;
use either case objects or case classes with an explicit `()' as a parameter list.
case class Nil
              ^
~~~
