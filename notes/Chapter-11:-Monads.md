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

See [Rúnar's article, "More on monoids and monads"](http://apocalisp.wordpress.com/2010/07/21/more-on-monoids-and-monads/) for more information about this connection.

#### Kleisli categories

Both monoids and monads form _categories_. In fact, we can see a category as a _generalized monoid_. Observe that with a monoid `M`, we can view each element of the monoid as a function of type `M => M`. For example, in the `Int` monoid with addition, these elements are `(_ + 0)`, `(_ + 1)`, `(_ + (-1))`, etc. Then the composition of these functions is the operation of the monoid. We can generalize this notion to consider not just the type `M`, but all types (`A`, `B`, `C`, etc.) and functions not just of type `M => M`, but `A => B` for any types `A` and `B`. Then ordinary function composition is an associative operation, with an identity element which is the `identity` function that just returns its argument. This more general notion does not form a monoid, but a _category_ which is more general. Specifically, it's the _category of Scala types with function composition_. Even more generally, whenever we have _arrows_ (a generalized notion of functions) whose composition is associative and has an identity element, we have a category.

A monad `F` can be described by what's called a [_Kleisli category_](http://en.wikipedia.org/wiki/Kleisli_category). The objects of this category are the ordinary Scala types, but the arrows are _Kleisli arrows_. That is, every arrow in this category is not of the general form `A => B`, but of the more specific form `A => F[B]`. The composition of these arrows is _Kleisli composition_ (given by the `compose` combinator in the chapter) and the identity for Kleisli composition is monadic `unit`. Every monad forms a Kleisli category in this way.

#### A monad is a monoid in a category of endofunctors

A monad is also a kind of monoid. If we think of a type like `(M, M) => M` as `M² => M` (taking 2 `M`s or the product of `M` with itself), and `M` as `1 => M` (where `1` is the `Unit` type), then we can think of a type like `F[F[A]] => F[A]` as `F²[A] => F[A]` or just `F² ~> F` (where `~>` denotes a [natural transformation](http://en.wikipedia.org/wiki/Natural_transformation)) and `A => F[A]` as `1[A] => F[A]` (where `1` is the identity functor) or just `1 ~> F`:


|             | `zero`/`unit`   | `op`/`join`|
--------------|-----------------|------------|
| `Monoid[M]` | `1 => M`        | `M² => M`  |
| `Monad[F]`  | `1 ~> F`        | `F² ~> F`  |

It's now clear that these are the same kind of thing, except `Monoid[M]` is operating in a category where the objects are Scala types and the arrows are Scala functions, and `Monad[F]` is operating in a category where the objects are Scala functors and the arrows are natural transformations.

See this StackOverflow question and its answers: http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem.

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

We say that `F` is _left adjoint_ to `G`, written `F ⊣ G` in mathematical notation.

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

