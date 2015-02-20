## Notes on chapter 10: Monoids

### Monoids in category theory

In [category theory](http://en.wikipedia.org/wiki/Category_theory), a [monoid](http://en.wikipedia.org/wiki/Monoid_(category_theory)) is a [category](http://en.wikipedia.org/wiki/Category_(mathematics)) with one object.

A _category_ is a purely algebraic structure consisting of "objects" and "arrows" between them, much like a directed graph with nodes and edges between them. A category will have objects like `A`, `B`, `C`, etc. and arrows between objects. Importantly, arrows _compose_. Given an arrow `f` from `A` to `B`, and another arrow `g` from `B` to `C`, their composition is an arrow from `A` to `C`. There is also an identity arrow from every object to itself.

For example, Scala forms a category where the objects are Scala types and the arrows are Scala functions. There's another category where the objects are Scala types and the arrows are subtype relationships.

Arrows in a category compose according to certain laws. Composition has to obey an _identity law_ such that any arrow `f` composed with the identity arrow is just `f`. That is, the identity arrow is _an identity with regard to composition_. Composition also has to obey an _associative law_. Denoting the composition operator as `*`, the associative law says that `(f * g) * h` should be the same as `f * (g * h)`. That is, it doesn't matter whether we compose on the left or the right first. This should remind you of the monoid laws!

A `Monoid[M]`, then, is just a category where the only object is the type `M`, and the arrows are the values of type `M`. The identity arrow is the identity element of the monoid, and arrow composition is the monoid's binary operation.

It can be a little difficult to see how the values of a type like `Int` can be _arrows_. Take for instance the monoid formed by integers with addition. Then the integer `5`, say, can be seen as an _operation_ that _adds 5_ to another integer. And `0` can be seen as an operation that leaves an integer alone.

See Rúnar's article [On Monoids](http://apocalisp.wordpress.com/2010/06/14/on-monoids/) for some of the deeper connections.

### The canonicity of a Scala monoid

In Scala, it's possible to have multiple `Monoid` instances associated with a type. For example, for the type `Int`, we can have a `Monoid[Int]` that uses addition with `0`, and another `Monoid[Int]` that uses multiplication with `1`.

This can lead to certain problems since we cannot count on a `Monoid` instance being _canonical_ in any way. To illustrate this problem, consider a "suspended" computation like the following:

~~~ scala
case class Suspended(acc: Int, m: Monoid[Int], remaining: List[Int])
~~~

This represents an addition that is "in flight" in some sense. It's an accumulated value so far, represented by `acc`, a monoid `m` that was used to accumulate `acc`, and a list of `remaining` elements to add to the accumulation using the monoid.

Now, if we have _two_ values of type `Suspended`, how would we add them together? We have no idea whether the two monoids are the same. And when it comes time to add the two `acc` values, which monoid should we use? There's no way of inspecting the monoids (since they are just functions) to see if they are equivalent. So we have to make an arbitrary guess, or just give up.

[Monoids in Haskell](http://www.haskell.org/haskellwiki/Monoid) work a bit differently. There can only ever be one `Monoid` instance for a given type in Haskell. This is because `Monoid` is a [type class](https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes). Instances of Haskell _type classes_ are not first-class values like they are in Scala. They are implicitly passed and never explicitly referenced. They do not have types per se, but [appear as constraints on types](http://www.haskell.org/tutorial/classes.html).

In Haskell, there is only one abstract monoidal operation and one zero. They are both polymorphic, and their signatures are:

~~~ haskell
mappend :: Monoid m => m -> m -> m
mempty :: Monoid m => m
~~~

This reads: "for any type `m`, if `m` is a monoid, `mappend` takes an `m` and another `m` and returns an `m`", and "`mempty` is a value of any type `m`, given that `m` is a monoid."

How do we then represent types that are monoids in more than one way, for example `Int` with addition as one monoid and multiplication as another? The answer is that we make each monoid a [newtype](http://www.haskell.org/haskellwiki/Newtype):

~~~ haskell
newtype Product = Product { getProduct :: Int }
newtype Sum = Sum { getSum :: Int }
~~~

A newtype in Haskell is a lot like a case class in Scala, except that newtypes can only have exactly one field, and they have _no runtime representation_ other than the underlying type of that field. It's a purely type-level construct, a kind of tagged type.

Using this mechanism, a product and a sum are actually different types, even though the underlying value is an `Int` in both cases. This way every type has its own canonical monoid, and values accumulated in one monoid can never be confused with values accumulated in another. But we can always convert between them if we need to.

[The Scalaz library](https://github.com/scalaz/scalaz) takes the same approach, where there is only one canonical monoid per type. However, since Scala doesn't have type constraints, the canonicity of monoids is more of a convention than something enforced by the type system. And since Scala doesn't have newtypes, we use [phantom types](http://www.haskell.org/haskellwiki/Phantom_type) to add tags to the underlying types. This is done with [`scalaz.Tag`](http://docs.typelevel.org/api/scalaz/stable/7.1.0-M3/doc/#scalaz.Tags$), which uses a couple of type aliases:

~~~ scala
type Tagged[A] = AnyRef { type T = A }
type @@[A,B] = A with Tagged[B]
~~~

Now the type `Int @@ Product`, for example, is just the type `Int`, but "tagged" with `Product` to make it explicit that `Monoid[Int @@ Product]` is distinct from `Monoid[Int @@ Sum]`. The types `Product` and `Sum` themselves are just empty traits with no members whatsoever.

Tim Perrett wrote a [blog post](http://timperrett.com/2012/06/15/unboxed-new-types-within-scalaz7/) detailing how tagged types work in Scalaz.

The great benefit of canonicity is that Scalaz can build a monoid instance for you just from the type. For example, if you have a `Map[Int, Map[String, (Int, Boolean)]]`, Scalaz can figure out a canonical composite monoid for this type:

~~~ scala
import scalaz._
import Scalaz._
import Tags._

val x = Map(1 -> Map("Foo" -> (1, Conjunction(false))))
val y = Map(1 -> Map("Foo" -> (2, Conjunction(true))))

val z = x |+| y
~~~

The value of `z` will be `Map(1 -> Map("Foo" -> (3, Conjunction(false))))`. Here, `Conjunction` is a "newtype" for `Boolean` to indicate that we want the "conjunction" monoid (`Boolean` with `&&` as the op and `true` as the identity element). There's a corresponding `Disjunction` for `||` with `false`. At the present time, Scalaz's default monoid for `Int` is addition, which is why we get `3` in `z`.

The syntax `x |+| y` works for any monoid. It works because there is an implicit class on the `Scalaz._` import. Be warned though, the `Scalaz._` import brings a lot of implicits into scope. You can import just the monoid syntax with `import scalaz.syntax.monoid._`.

### Monoid coproducts

If `A` and `B` are monoids, then `(A,B)` is a monoid, called their _product_. But what about _coproducts_? Is there a composite monoid where we can have _either_ an `A` or a `B`? Yes there is. See [Rúnar's blog post on monoid coproducts](http://blog.higher-order.com/blog/2014/03/19/monoid-morphisms-products-coproducts/) for the details.

### Links

[On Monoids, by Rúnar](http://apocalisp.wordpress.com/2010/06/14/on-monoids/) -- Provides some deeper insight on the relationship between monoids and lists, and looks at them from a category theory perspective.
