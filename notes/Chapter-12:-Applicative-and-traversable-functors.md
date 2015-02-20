## Notes on chapter 12: Applicative and traversable functors

### The cost of power

There is a tradeoff between applicative APIs and monadic ones. Monadic APIs are strictly more powerful and flexible, but the cost is a certain loss of algebraic reasoning.

The difference is easy to demonstrate in theory, but takes some experience to fully appreciate in practice.

Consider composition in a monad, via `compose` (Kleisli composition):

~~~ scala
val foo: A => F[B] = ???
val bar: B => F[C] = ???
val baz: A => F[C] = bar compose foo
~~~

There is no way that the implementation of the `compose` function in the `Monad[F]` instance can inspect the values `foo` and `bar`. They are functions, so the only way to "see inside" them is to give them arguments. The values of type `F[B]` and `F[C]` respectively are not determined until the composite function _runs_.

Contrast this with combining values with `map2`:

~~~ scala
val quux: F[A] = ???
val corge: F[B] = ???
val grault: F[C] = map2(quux, corge)(f)
~~~

Here the implementation of `map2` can actually look at the values `quux` and `corge`, and take different paths depending on what they are. For instance, it might rewrite them to a normal form for improved efficiency. If `F` is something like `Future`, it might decide to start immediately evaluating them on different threads. If the data type `F` is applicative but _not a monad_, then the implementation has this flexibility universally. There is then never any chance that an expression in `F` is going to involve functions of the form `A => F[B]` that it can't see inside of.

The lesson here is that power and flexibility in the interface often restricts power and flexibility in the implementation. And a more restricted interface often gives the implementation more options.

See [this StackOverflow question](http://stackoverflow.com/questions/7861903/what-are-the-benefits-of-applicative-parsing-over-monadic-parsing) for a discussion of the issue with regard to parsers.

See also the end of the note below on "Applicative laws", for an example of the loss of algebraic reasoning that comes with making an API monadic rather than applicative.

### Applicative laws

In the chapter, we decided to present the `Applicative` laws in terms of `map2`. We find that this works pretty well pedagogically. We used the following laws:

   * Left identity: `map2(unit(()), fa)((_,a) => a) == fa`
   * Right identity: `map2(fa, unit(()))((a,_) => a) == fa`
   * Associativity: `product(product(fa, fb),fc) == map(product(fa, product(fb, fc)))(assoc)`
   * Naturality: `map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))`

But there are other ways to state the laws for `Applicative`. Commonly the laws for applicative are stated in terms of `apply`, which is sometimes called _idiomatic function application_ (where the "idiom" is `F`):

~~~ scala
def apply[A,B](ff: F[A => B], fa: F[A]): F[B] =
  map2(ff, fa)(_(_))
~~~

The laws for `apply` are _identity_, _homomorphism_, _interchange_, and _composition_.

#### Identity law

The identity law for `apply` is stated as:

~~~ scala
apply(unit(id), v) == v
~~~

That is, `unit` of the identity function is an identity for `apply`.

#### Homomorphism law

The homomorphism law for `apply` is stated as:

~~~ scala
apply(unit(f), unit(x)) == unit(f(x))
~~~

In other words, idiomatic function application on `unit`s is the same as the `unit` of regular function application. In more precise words, `unit` is a homomorphism from `A` to `F[A]` with regard to function application.

#### Interchange law

The interchange law for `apply` is stated as:

~~~ scala
apply(u, unit(y)) == apply(unit(_(y)), u)
~~~

This law is essentially saying that `unit` is not allowed to carry an effect with regard to any implementation of our applicative functor. If one argument to `apply` is a `unit`, then the other can appear in either position. In other words, it should not matter when we evaluate a `unit`.

#### Composition

The composition law for `apply` is stated as:

~~~ scala
apply(u, apply(v, w)) == apply(apply(apply(unit(f => g => f compose g), u), v), w)
~~~

This is saying that applying `v` to `w` and then applying `u` to that is the same as applying composition to `u`, then `v`, and then applying the composite function to `w`. Intuitively it's saying the same as:

~~~ scala
u(v(w)) == (u compose v)(w)
~~~

We might state this law simply as: "function composition in an applicative functor works in the obvious way."

#### Applicative normal form

The applicative laws taken together can be seen as saying that we can rewrite any expression involving `unit` or `apply` (and therefore by extension `map2`), into a normal form having one of the following shapes:

~~~ scala
pure(x)          // for some x
map(x)(f)        // for some x and f
map2(x, y)(f)    // for some x, y, and f
map3(x, y, z)(f) // for some x, y, z, and f
// etc.
~~~

Where `f`, `x`, `y`, and `z` do not involve the `Applicative` primitives at all. That is, every expression in an applicative functor `A` can be seen as lifting some pure function `f` over a number of arguments in `A`.

Note that this reasoning is lost when the applicative happens to be a monad and the expressions involve `flatMap`. The applicative laws amount to saying that the arguments to `map`, `map2`, `map3`, etc can be reasoned about independently, and an expression like `flatMap(x)(f)` explicitly introduces a dependency (so that the result of `f` depends on `x`). See the note above on "The cost of power".

### Applicatives in Scalaz

[The Scalaz library](http://github.com/scalaz/scalaz) provides an [`Applicative` trait.](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Applicative). In this trait, `map2` et al are called `lift2`, `lift3`, and so on.

The `scalaz.syntax.applicative` object supplies implicit syntax for applicatives to lift a function of arbitrary arity:

~~~ scala
(x |@| y |@| z)(f)
~~~

This is equivalent to `lift3(x, y, z)(f)`.

### Traversable functors

For further reading on traversable functors, see:

[The Essence of the Iterator Pattern](http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator), by Jeremy Gibbons and Bruno Oliveira. Published in _Mathematically-Structured Functional Programming_, 2006.

[Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html), by Conor McBride and Ross Paterson. Published in _Journal of Functional Programming_, 2008.

[An Investigation of the Laws of Traversals](http://arxiv.org/pdf/1202.2919), by Mauro Jaskelioff and Ondrej Rypacek, published in _Mathematically-Structured Functional Programming_, 2012.

#### Laws of traversable functors

`Traverse[T[_]]` has two laws. There are many ways to state them, but here is one:

  * Identity law: `sequence[Id,A](xs)` = `xs`. That is, traversing in the identity applicative (`type Id[X] = X`) has no effect.
  * Fusion law: `sequence[({type f[x] = F[G[x]]})#f, A](xs)` = `map(sequence[F,G[A]](xs))(sequence[G,A])`. That is, traversal in `F[_]` followed by traversal in `G[_]` can be fused into one traversal in the composite applicative `F[G[_]]`.

### Monad transformers

A _monad transformer_ is a data type that composes a particular monad with any other monad, giving us a composite monad that shares the behavior of both.

There is no general way of composing monads. Therefore we have to have a specific transformer for each monad.

For example, [`OptionT`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.OptionT) is a monad transformer that adds the behavior of `Option` to any other monad. The type `OptionT[M, A]` behaves like the composite monad `M[Option[_]]`. Its `flatMap` method binds over both the `M` and the `Option` inside, saving us from having to do the gymanstics of binding over both.

[Scalaz](http://github.com/scalaz/scalaz) provides many more monad transformers, including `StateT`, `WriterT`, `EitherT`, and `ReaderT` (also known as `Kleisli`).

### Links

* [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)

* [The essence of form abstraction](http://groups.inf.ed.ac.uk/links/formlets/) talks about writing compositional web forms using applicative functors.

* Brent Yorgey's [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) is a great resource on `Monad`, `Applicative`, `Traverse` and other type classes.

