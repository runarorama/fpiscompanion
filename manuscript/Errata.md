# Errata

This is a list of post-publication errata for the first published version of *Functional Programming in Scala*. To submit new errata please [open an issue](https://github.com/fpinscala/fpinscala/issues) in the book's GitHub repository.

_Pg xviii in About this Book_: URL is incorrectly listed as https://github.com/fpinscala/fpinscla (note missing 'a'), should be https://github.com/fpinscala/fpinscala

_Pg 7_: In `throw new Exception`, `Exception` should not be bolded, in the code listing at the top of the page.

_Pg 10_: URL in footnote is incorrectly listed as https://github.com/pchiusano/fpinscala, should be https://github.com/fpinscala/fpinscala

_Pg 55_: `employeesByName.get("Joe")` should be `lookupByName("Joe")` at the top of the page.

_Pg 82_: `Int.MaxValue` is incorrectly capitalized as `Int.maxValue` in the exercise prompt for `6.1`

_Pg 86_: Definition of `nonNegativeLessThan` at bottom of page incorrectly reuses `rng` variable for recursive call. Definition should be: 

``` scala
def nonNegativeLessThan(n: Int): Rand[Int] = { rng => 
  val (i, rng2) = nonNegativeInt(rng)
  val mod = i % n
  if (i + (n-1) - mod >= 0)
    (mod, rng2)
  else nonNegativeLessThan(n)(rng2)
}
```

_Pg 74_: Figure contains a typo, "strean elements" instead of "stream elements"

_Pg 150_: Listing 9.1 has a gratuitous nonstrict function argument in the definition of `|` in `ParserOps`. The definition should be:

``` scala
case class ParserOps[A](p: Parser[A]) {
  def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2) 
  def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
}
```

_Pg 150_: Examples given for `listOfN` are incorrect. Should be:

``` scala
run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab","ab","cad"))
run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad","ab","ab"))
run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab","ab","ab"))
```

_Pg 184_: Exercise 10.13 incorrectly defines `Leaf` as a `case object`. It should be `case class`:

``` scala
case class Leaf[A](value: A) extends Tree[A]
```

_Pg 187_: Definition of signature for `map` on `Option` has incorrect return type, should be:

``` scala
def map[A,B](oa: Option[A])(f: A => B): Option[B]
```

_Pg 238_: Definition of `printLine` has a typo, an extra `Return` constructor, should be:

``` scala
def printLine(s: String): IO[Unit] = Suspend(() => println(s))
```

_Pg 240_: REPL session has a typo, should be:

``` scala
val g = List.fill(100000)(f).foldLeft(f) {
  (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
}
```

Note: we could write a little helper function to make this nicer: 

``` scala
def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }

val g = List.fill(100000)(f).foldLeft(f) {
  (a, b) => x => suspend { a(x).flatMap(b) }
}
```

_Pg 241_: `TrailRec` should be `TailRec` at the top of the page.

_Pg 285_: Code snippet has an incorrectly named type parameter, currently reads:

``` scala
case class Await[A,O](
req: Is[I]#f[A], recv: Either[Throwable,A] => Process[Is[I]#f,O]
) extends Process[Is[I]#f,R] // `R` should be `O`
```

Should read:

``` scala
case class Await[A,O](
req: Is[I]#f[A], recv: Either[Throwable,A] => Process[Is[I]#f,O]
) extends Process[Is[I]#f,O]
```

_Pg 262_: List 14.3 `STArray.apply` has a typo, should be:

``` scala
object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })
}
```

_Pg 275_: `Await` in the definition of `loop` is incorrect. Should be `await`.

