# A brief introduction to Haskell, and why it matters

[Haskell](http://www.haskell.org/haskellwiki/Haskell) is in some ways a nicer language for functional programming than Scala, and if you are serious about learning more FP, we recommend learning it. We recommend this even if you continue to program predominantly in Scala. Why? Many of the key ideas and structures covered in this book (for instance, most of parts 3 and 4) originated with research or work using Haskell. Throughout the chapter notes, we have provided links to further reading--many of those references use Haskell, and knowing Haskell at least well enough to 'read' it will let you tap into the enormous well of ideas about FP that exist in the Haskell community. Our goal here is not to make you a Haskell expert, but to give enough of an introduction to the language that you can learn from the various resources (papers, blog posts, and code) about FP that use Haskell.

## About Haskell

Haskell is a purely functional language. Unlike Scala, there is no escape hatch--if you want to write imperative code in Haskell, you are forced to use the techniques discussed in part 4--the `IO` and `ST` monads, and/or a higher-level library built atop these data types, like the streaming library we discussed in chapter 15. (Technically, Haskell does include some functions, like `unsafePerformIO`, that allow you to subvert its purity. But these functions are used extremely rarely, as they don't mix well with Haskell's laziness.) This lack of escape hatch has been one reason why many discoveries of how to write functional programs have come from the Haskell community--in Haskell, the question, "how do I express this program using pure functions?" is not merely an intellectual exercise, it is a prerequisite to getting anything done!

Notably, Haskell is also _lazy by default_. As we discussed in chapter 5, Scala (and most other languages) by default evaluates function arguments _before_ calling the function. In Haskell, arguments are by default passed unevaluated and data constructors are also lazy. This is a very different model of computation than a strict language, and reasoning about performance in Haskell is different than in a strict language. There are benefits to the Haskell model (we discussed some of these benefits in chapter 5), though if you do start Haskell programming, _take the time to fully understand Haskell's evaluation model and incorporate it into your thinking_.

_Note:_ To be a bit more precise, the Haskell standard does not technically dictate that all functions be call-by-need (lazy) and the compiler/runtime is free to do more strict evaluation so long as this does not affect program behavior. As a general rule, though, call-by-need evaluation provides a reasonable mental model for how Haskell programs are executed.

We won't talk too much more about Haskell's laziness in this appendix, but follow some of the links at the end for more information.

## Haskell syntax

Let's start by looking at a simple `"Hello world!"` program, which we might place in a file, `Main.hs`: 

~~~ Haskell
module Main where   -- A comment, starting with `--` 

-- Equivalent to `def main: IO[Unit] = ...`
main :: IO ()
main = putStr "Hello world!!"
~~~ 

We can compile this using the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/) (GHC), which comes bundled with the [Haskell Platform](http://www.haskell.org/platform/). 

~~~ 
> ghc --make Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
> ./Main
Hello world!!
~~~ 

Module declarations start with the keyword `module`, then are followed by a module name (here `Main`, but `Foo.Bar.Baz` and `Data.List` are also valid module names), then the keyword `where`. Like Java, module names must follow directory structure, so `Foo.Bar` must live in a file called `Bar.hs` inside the `Foo/` directory.
  
The entry point of a Haskell program is the `main` function. In Haskell, type signatures _precede_ the declaration of the value. So `main :: IO ()` is a type signature, preceding the definition of `main = putStr "Hello world!!"` on the subsequent line. We pronounce the `::` symbol as 'has type'.

Since Haskell is pure, it uses an `IO` type to represent computations that may interact with the outside world. Unlike Scala, `IO` is built into the language--you do not need to write your own `IO` type like we did in chapter 13. And there are a couple other differences in how we write type signatures--the `Unit` type from Scala is written as `()` in Haskell. (Like Scala, it has a single value, written `()`) And _type application_ uses juxtaposition, rather than square brackets. For comparison:

* `IO[Unit]` (Scala) vs `IO ()` (Haskell)
* `Map[String,Double]` vs `Map String Double`
* `Map[String,Map[K,V]]` vs `Map String (Map k v)`: In Haskell, type variables must begin with a lowercase letter. Names starting with uppercase are reserved for concrete types and data constructor names. 

***
__Modules with explicit export lists__: All values and types declared inside a Haskell module are public by default. If you wish to make some of these private, you can declare the module with an explicit _export list_, and simply avoid mentioning any private functions you don't want others to use. For example:

~~~ Haskell
module TopSecret (f1, f2) where

f1 :: Int
f1 = f2 + magicValue

f2 = 97
magicValue = 42 
~~~ 

Since `magicValue` is not exported, it will not be visible to users of this module. See the [Haskell wikibook](http://en.wikibooks.org/wiki/Haskell/Modules#Exporting) for more information about importing and exporting.
***



Type application is left-associative, so `Foo x y z` is the same as `((Foo x) y) z`. Unlike Scala, type constructors in Haskell are _curried_ by default. We do not need to do anything special to partially apply a type constructor, we just leave off some of the arguments (recall the type-lambda trick we introduced in chapter 11 when giving the `Monad` instance for `State`.) We'll see how this comes in handy later, when giving the `Monad` instance for `State`.

Let's look at a couple function definitions: 

~~~ Haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n = go n 1
  where go 0 acc = acc
        go n acc = go (n-1) (n * acc)

factorial'' :: Int -> Int
factorial'' n = 
  let go 0 acc = acc
      go n acc = go (n-1) (n * acc)
  in go n 1
~~~ 

A few things about this code:

* Function types are written using `->` instead of `=>` as in Scala. 
* Identifier names in Haskell can contain the `'` symbol (as well as `_`, letters, and numbers, though data types must start with a capital letter and other identifiers cannot start with a number or `'`). 
* The syntax for _function application_ is simple juxtaposition, with arguments separated by spaces, as in `go n 1` (vs `go(n, 1)` in Scala). Function application is left-associative and binds tighter than any other operation, so `f x y+1` would actually be parsed as `(f x y) + 1`, and we can use parentheses to obtain a desired grouping.
* We are also making use of a local function to write our loop in both `factorial'` and `factorial''`, much like we'd do in Scala, though unlike Scala, _all tail calls are optimized_. Syntactically, we can place local definition(s) _after_ the expression that references it, using a `where`-clause (used in `factorial'`) or we can place the local definitions _before_ the expression referencing it, using a `let` expression (used in `factorial''`).
* These examples demonstrate some simple pattern matching in Haskell, matching on the numeric literal `0`. We can write a function's name several times, supplying a pattern for each argument. Patterns are matched top to bottom, as in Scala. We'll talk more about Haskell's pattern matching in the next section.

_Note:_ Haskell determines where blocks begin and end based on indentation. Certain keywords (like `let` and `where`) introduce a _layout block_. Read [all about the details here](http://en.wikibooks.org/wiki/Haskell/Indentation).

Lastly, we note that unlike Scala, Haskell does not have a distinction between `val`, `def`, `lazy val`, and `var`. All declarations use the same syntax, a symbol name, followed by any arguments or patterns, followed by an `=` sign, followed by the body of the definition. 

### Data types and higher order functions ###

Unlike Scala, algebraic data types in Haskell are not represented using subtypes. Instead, we declare an ADT just by listing its data constructors using the following syntax:

~~~ Haskell
data List a = Nil | Cons a (List a)

data Option a -- alternate style
  = None 
  | Some a
~~~ 

An ADT is introduced with the keyword `data`, followed by the name of the type, followed by a list of type parameter names separated by spaces, followed by an `=`, then a list of data constructors separated by `|`. Each data constructor must start with an uppercase letter (exception: constructors starting with `:` are allowed, and are useful for infix 'operator' data constructors), and as with function application we just separate the arguments of the data constructor by spaces, using parentheses for grouping. Note that data constructor arguments are unnamed, we give only their types. There is a limited form of named accessors in Haskell; see the sidebar below.

Rather than having `Nil` 'extend' `List[Nothing]`, Haskell has proper _universally quantified types_. The type of the value `Nil` is `forall a . List a`, which means that we can treat `Nil` as a `List Int`, a `List String`, and so on, for any choice of `a`.

_Note:_ Haskell also includes a feature called generalized algebraic data types (GADTs), which have a slightly different syntax. GADTs are quite powerful and come up a lot when designing embedded domain specific languages. See the [Haskell Wikibook](http://en.wikibooks.org/wiki/Haskell/GADT) for more information. 

As mentioned above, data constructors in Haskell are lazy by default. We could use the `List` data type above to construct an infinite sequence, much like we did for `Stream` in chapter 5.

Pattern matching mirrors the syntax of the data constructors as in `headOption`, which returns the first element of a `List a`, if one exists.

~~~ Haskell
headOption :: List a -> Option a
headOption Nil = None
headOption (Cons h t) = Some h
~~~ 

Pattern matching forces evaluation of the expression being matched (here, the argument to `headOption`), though the expression is only forced to a depth needed to resolve which branch to take. Thus, `headOption` works fine for an infinite list.

***
__Haskell record syntax:__ If you want to name the arguments to a data constructor, you can just write functions, for instance (note these are partial functions which generate a pattern match error at runtime if given `Nil`):

~~~ Haskell
head (Cons h _) = h 
tail (Cons _ t) = t 
~~~ 

Haskell provides some syntax sugar which generates these functions for you: 

~~~ Haskell
data List a = Nil | Cons { head :: a, tail :: List a }
~~~ 

This is the same `List` type but the `{ head :: a, tail :: List a }` instructs the compiler to generate the `head` and `tail` accessors. Because this simple scheme puts accessors into the module namespace, you cannot have two data constructors in the same module with a same-named argument. There are various proposals to improve on this.

_Note:_ Haskell does not support ad hoc name overloading like Scala.
***

Note that the Haskell standard library contains an implementation of `Option`, although the type is called `Maybe`, and its constructors are called `Nothing` and `Just` instead of `None` and `Some`:

~~~ Haskell
data Maybe a = Nothing | Just a
~~~ 

There is also a data type, `Either` in Haskell, with the same constructors as Scala:

~~~ Haskell
data Either a b = Left a | Right b
~~~ 

#### List syntax and higher order functions ####

Lists in Haskell come built in and have some special syntax. We write the type `List Int` as `[Int]`, we write `Nil` as `[]`, and `Cons h t` as `h:t`. The `:` operator is right-associative, so `1:2:[]` is equal to `1:(2:[])`. And we can write list literals using the syntax `[1,2,3,4]`, `["a","b"]`. 

Let's look at a couple familiar higher-order list functions, which will also give us an excuse to discuss some more bits of Haskell syntax:

~~~ Haskell
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f z xs = case xs of 
  [] -> z
  h:t -> f h (foldRight f z t)

-- signature not needed (see note below)
takeWhile f xs = 
  foldRight (\h t -> if f h then h : t else []) [] xs 

takeWhile' f = 
  foldRight (\h t -> if f h then h : t else []) []
~~~ 

_Note:_ In the Haskell standard library, `foldRight` is called `foldr` and `foldLeft` is called `foldl`. 

_Note:_ Haskell is fully inferred, unlike Scala, so type annotations are generally not needed anywhere, not even for top-level function declarations (though it is considered good style to include them). 

This snippet again shows a few new things. Let's start by looking at the type signature. What's with all the `->` symbols? Functions in Haskell are curried by default. Although we can write `(Int,Int) -> Int`, for the type of a function that expects a pair of integers and returns an `Int`, we almost always work with curried functions in Haskell and the language has special support for interpreting curried function application efficiently. The `->` operator is right-associative, so the type `(a -> b -> b) -> b -> List a -> b` can be read the same way you'd read the Scala signature:

~~~ Scala
def foldRight[A,B](f: (A,B) => B, z: B, xs: List[A]): B
~~~ 

Function literal syntax is a bit different in Haskell as well. In Scala, we'd write `(h,t) => ...`, in Haskell, we write `\h t -> ...`. The parameters to a lambda function mirrors the syntax for function application, with parameters separated by spaces. Following the parameter list is the `->`, then the body of the lambda. In this case, the function literal we are passing to `foldRight` uses an `if` expression, which uses the syntax `if <expr> then <expr> else <expr>`. 

Moving on, we see an alternate syntax for pattern matching the `case ... of` syntax. This is analogous to Scala's `match` expression, and grammatically it can be used anywhere an expression is expected--it's not tied to a symbol delaration like we saw earlier for `factorial`.

Lastly, the definition of `takeWhile'` shows a very common idiom in Haskell. Because functions are curried by default, we get a very lightweight syntax for left-to-right partial application--we simply leave off some of the arguments! In the case of `takeWhile'`, we are leaving off one of its parameters (the list parameter), since all we are doing with it is passing it along to the `foldRight` function. This is called _eta reduction_. More generally, the following three definitions are all equivalent:

~~~ Haskell
f x y = g x y
f x = g x -- eta reduction
f = g -- another eta reduction
~~~ 

The [Haskell Wiki](http://www.haskell.org/haskellwiki/Eta_conversion) has a nice further explanation. 

Generally, function parameters are ordered to maximize the usefulness of left-to-right partial application, which is usually the _opposite_ order of typical OO. 

_Note:_ In Haskell, you'll often see expressions like `sum (filter isEven (map f xs))` written as: `sum . filter isEven . map f $ xs`. What's going on here? Well, `f . g` is equivalent to `f.compose(g)` in Scala (recall that `f.compose(g)(x) == f(g(x))`). And `f $ x` is equivalent to `f x`, but the `$` has very low precedence and is just used to avoid having to introduce a level of parentheses. This reads "backwards" from the typical OO chain of function calls, where we'd write `xs.map(f).filter(isEven).sum`.

### Type classes, monads, and do notation ###

`Monad`, `Applicative`, `Functor`, and all the other structures we discussed in part 3 exist in Haskell's standard library. The way they are encoded and used works differently than in Scala. Let's start by looking at what is called the _class_ definition for `Functor`: 

~~~ Haskell
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
~~~ 

This is equivalent to the following _interface_ definition in Scala: 

~~~ Scala
trait Functor[F[_]] {
  def fmap[A,B](f: A => B): F[A] => F[B]
}
~~~ 

`Functor` is called a _type class_ (or 'typeclass') in Haskell. The name comes from the the fact that it defines a _class_ of types, not in the OO sense of the term 'class', but in the more general, mathematical sense of a collection of objects whose members can be described in some formal way.. 

Let's now look at how we declare an instance of `Functor`:

~~~ Haskell
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)
~~~ 

Whereas in Scala, we'd write: 

~~~ Scala
val OptionFunctor = new Functor[Option] {
  def fmap[A,B](f: A => B): Option[A] => Option[B] = 
    oa => oa match {
      case None => None
      case Some(a) => Some(f(a))
    }
}
~~~ 

Notice that in Haskell, we aren't naming the instance at all. We simply state "here is the `Functor` instance for the type constructor `Maybe`". We'll see why this is in a minute.

The way we write code which is polymorphic in the choice of `Functor` works differently in Haskell. In Scala, we might accept the `Functor` instance as an ordinary function parameter, possibly `implicit`. (We might also just place the definition within the `Functor` `trait`.) 

~~~ Scala
def strength[F[_],A,B](
    a: A, fb: F[B])(implicit F: Functor[F]): F[(A,B)] =
  F.fmap((b: B) => (a,b))(fb)
~~~ 

In Haskell, instances are not first-class values as they are in Scala. Instead, we accept `Functor` as a _constraint_:

~~~ Haskell
strength :: Functor f => a -> f b -> f (a, b)
strength a fb = fmap (\b -> (a,b)) fb
-- Or, eta reduced:
-- strength a = fmap (\b -> (a,b)) 
~~~ 

Notice the call to `fmap` in the implementation doesn't say which `Functor` instance it is referring to! Instances are unnamed and the correct instance will be chosen based on type--for instance, when calling `strength 1 ["a", "b", "c"]`, the `Functor` for list will be supplied automatically. Although Haskell frees the programmer from having to explicitly pass instances around in the program, because instances are not first-class values and are selected based on _type_, we are limited to supplying at most one instance for a given type. For instance, we can imagine multiple `Monoid` instances on `Int`, one for addition, and one for multiplication. If `Monoid` is formulated as a `class`, we can't give both instances, as they overlap:
  
~~~ Haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid Int where
  mempty = 0
  mappend = (+) -- Syntax for referencing an infix operator

-- Not legal! Overlapping instance
instance Monoid Int where
  mempty = 1
  mappend = (*)
~~~ 

_Note:_ No one particularly likes the names `mempty` and `mappend`, as they sound a little too specific to lists, but these are the names used in the Haskell standard library definition of `Monoid`.

If both these instances were defined, we would have no way of distinguishing which one was intended at the call site of a function like `foldMap`:

~~~ Haskell
foldMap :: Monoid m => (a -> m) -> [a] -> m -- only takes two arguments, no way to specify the instance!
foldMap f = foldr mappend mempty . map f
~~~ 

_Note_: Haskell's restriction on overlapping instances does come with another benefit. If we have two `Map k v` values (Haskell's ordered map implementation), it is safe to assume that both maps were ordered using the same `Ord k` instance (`Ord` is Haskell's typeclass for specifying ordering). This is not in general a safe assumption to make in Scala, as the maps may have been built by two different pieces of code, each of which locally used a different instance of `Ordering` (the Scala trait for specifying ordering).

In situations where we might otherwise define multiple overlapping instances, we can do one of two things. We can switch to using ordinary values to represent the class. For `Monoid`, we can create a type with a single data constructor, containing the identity and the operation:

_Note:_ By convention, data types with a single constructor usually give the constructor the same name as the type. 

~~~ Haskell
data Monoid a = Monoid a (a -> a -> a)
~~~ 

Or we can provide a wrapper type for `Int`, simply so Haskell's type system can use this to select the appropriate `Monoid`:

~~~ Haskell
newtype Sum = Sum Int
newtype Product = Product Int

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a + b)

instance Monoid Product where
  mempty = Product 1
  mappend (Product a) (Product b) = Product (a * b)
~~~ 

Note that `newtype Sum = Sum Int` behaves just like `data Sum = Sum Int`, but its runtime representation is the same as whatever is inside the newtype, in this case `Int`.

Let's look at the class definition for `Monad`, and the `Monad` instance for `Maybe` and `State`. This introduces a few new things:

~~~ Haskell
class Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Monad Maybe where 
  return = Just

  Nothing >>= _ = Nothing
  Just x >>= f = f x

data State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s) 
  a >>= f = State $ \s0 -> case runState a s0 of
    (ar, s1) -> runState (f ar) s1 
~~~ 

In Haskell, `unit` and `flatMap` are named `return` and `>>=` (pronounced 'bind'). Notice that we didn't need to construct a type lambda like we did in chapter 11, we just partially applied the `State` type constructor by leaving off its second parameter. 

Haskell also includes an analogue of for-comprehension syntax, called `do`-notation. Here's an example:

~~~ Haskell
main :: IO () 
main = do 
  putStr "Echo-inator v2.0" 
  s <- readLine
  putStr s
~~~ 

This is equivalent to: 

~~~ Haskell
main = 
  putStr "Echo-inator v2.0" >>= \_ -> -- As in Scala, we use '_' for an ignored function parameter
  readLine >>= \s -> 
  putStr s
~~~ 

Unlike Scala's `for` comprehensions, we aren't required to end with a `yield`, and we don't have to extract a value from each monadic expression in the `do` block. Here's the equivalent Scala:

~~~ Scala
def main = for {
  _ <- putStr("Echo-inator v2.0")
  s <- readLine
  _ <- putStr(s)
} yield ()
~~~ 

## Conclusion ##

This appendix has only scratched the surface of Haskell, but should provide you with a foundation for reading papers, blog posts, and other resources that use Haskell. We recommend the [Haskell Wikibook](http://en.wikibooks.org/wiki/Haskell) as a good reference for learning more of the details of Haskell's syntax.

