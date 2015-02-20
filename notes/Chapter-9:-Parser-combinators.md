## Notes on chapter 9: Parser combinators

### Different approaches to functional design

There are many different approaches to functional design. The approach we take in this chapter we are calling "algebraic design" to emphasize the fact that we are mainly concerned with the abstract data types and laws of our API. The actual representation is a later concern, and we choose a representation that best facilitates our algebra. This perspective is common to [Category Theory](http://en.wikipedia.org/wiki/Category_theory), where mathematical objects (or a _type_ in the more concretely programming-related case) are defined solely by their relationship to other objects, not their "internal" structure.

[Conal Elliott](http://conal.net/) advocates for an approach he calls [denotational design](http://conal.net/papers/type-class-morphisms), also see [this nice exposition by Luke Palmer](http://lukepalmer.wordpress.com/2008/07/18/semantic-design/). In this style of design, the choice of a _precise meaning_ or _denotation_ (see [denotational semantics](http://en.wikipedia.org/wiki/Denotational_semantics)) for the data types in our library guides the design process. This denotation is not (necessarily) similar to the actual concrete representation we ultimately select when implementing our library--that happens later. The denotation is instead a hopefully simple, precise mathematical object that lets us understand what the data types of our library and their various operations _mean_ (in terms of how they transform these denotations). In this style of design, we begin with some initial idea of a denotation for our data type, and refine this denotation in response to the operations we wish to support.

There is no "right" answer in approaches to the design of software, and the higher your perspective, the more the lines between different approaches blur.

### Design issues in parser combinator libraries

Parser combinator libraries are very common in the FP world, and there are a lot of different variations. A few of the key design issues:

_Error reporting_: Generating good error messages is a delicate problem, and is easy to get wrong. The overall error-reporting strategy developed in this chapter is most similar to [Parsec](http://legacy.cs.uu.nl/daan/parsec.html), a widely used parsing library in Haskell, though Parsec does not support the _nested_ error messages we developed here. A key factor for good error reporting is having a clear model for _when backtracking is allowed to occur_ and giving the programmer the ability to control this when specifying the grammar. We chose to adopt Parsec's convention that all parsers commit by default if they consume at least one character, and that the `attempt` combinator "undoes" this commit. Another choice is to _not_ have parsers commit by default, and introduce a separate `commit` combinator (which is still "undone" by `attempt`). This has the same expressiveness but we find it is not usually the best default since most grammars require little lookahead. Interestingly, the grammar ends up looking almost the same with the introduction of an explicit `commit`: the leaf-level `token` parser is usually wrapped in `commit`, and higher levels of the grammar that require more lookahead use `attempt`.

_Input handling:_ Is the input type fixed to be `String` or can parsers be polymorphic over any sequence of tokens? The library developed in this chapter chose `String` for simplicity and speed. Separate tokenizers are not as commonly used with parser combinators--the main reason to use a separate tokenizer would be for speed, which is usually addressed by building a few fast low-level primitive like `regex` (which checks whether a regular expression matches a prefix of the input string). 

Having the input type be an arbitrary `Seq[A]` results in a library that could in principle be used for other sorts of computations (for instance, we can "parse" a sum from a `Seq[Int]`). There are usually better ways of doing these other sorts of tasks (see the discussion of stream processing in part 4 of the book) and we have found that there isn't much advantage in generalizing the library we have here to other input types. 

We can define a similar set of combinators for parsing _binary_ formats. Binary formats rarely have to deal with lookahead, ambiguity, and backtracking, and have simpler error reporting needs, so often a more specialized binary parsing library is sufficient. Some examples of binary parsing libraries are [Data.Binary](http://code.haskell.org/binary/) in Haskell, which has inspired similar libraries in other languages, for instance [scodec](https://github.com/scodec/scodec) in Scala.

You may be interested to explore generalizing or adapting the library we wrote enough to handle binary parsing as well (to get good performance, you may want to try using the [specialized annotation](http://www.scala-notes.org/2011/04/specializing-for-primitive-types/) to avoid the overhead of boxing and unboxing that would otherwise occur). 

_Streaming parsing and push vs. pull_: It is possible to make a parser combinator library that can operate on huge inputs, larger than what is possible or desirable to load fully into memory. Related to this is is the ability to produce results in a streaming fashion--for instance, as the raw bytes of an HTTP request are being read, the parse result is being constructed. The two general approaches here are to allow the parser to _pull_ more input from its input _source_, and inverting control and _pushing_ chunks of input to our parsers (this is the approach taken by the popular Haskell library [attoparsec](http://hackage.haskell.org/packages/archive/attoparsec/0.10.2.0/doc/html/Data-Attoparsec-ByteString.html)), which may report they are finished, failed, or requiring more input after each chunk. We will discuss this more in part 4 when discussing stream processing. 

_Handling of ambiguity and left-recursion_: The parsers developed in this chapter scan input from left to right and use a left-biased `or` combinator which selects the "leftmost" parse if the grammar is ambiguous--these are called [LL parsers](http://en.wikipedia.org/wiki/LL_parser). This is a design choice. There are other ways to handle ambiguity in the grammar--in [GLR parsing](http://en.wikipedia.org/wiki/GLR_parser), roughly, the `or` combinator explores both branches simultaneously and the parser can return multiple results if the input is ambiguous. (See [Daniel Spiewak's series on GLR parsing](http://www.codecommit.com/blog/scala/unveiling-the-mysteries-of-gll-part-1) also his [paper](http://www.cs.uwm.edu/~dspiewak/papers/generalized-parser-combinators.pdf)) GLR parsing is more complicated, there are difficulties around supporting good error messages, and we generally find that its extra capabilities are not needed in most parsing situations.

Related to this, the parser combinators developed here cannot directly handle _left-recursion_, for instance:

~~~ scala
def expr = (int or double) or (
           expr ** "+" ** expr ) or (
           "(" ** expr ** ")" )
~~~

This will fail with a stack overflow error, since we will keep recursing into `expr`. The invariant that prevents this is that all branches of a parser must consume at least one character before recursing. In this example, we could do this by simply factoring the grammar a bit differently:  

~~~ scala
def leaf = int or double or ("(" ** expr ** ")")
def expr = leaf.sep("+")
~~~

Where `sep` is a combinator that parses a list of elements, ignoring some delimiter ("separator") that comes between each element (it can be implemented in terms of `many`). This same basic approach can be used to handle operator precedence--for instance, if `leaf` were a parser that included binary expressions involving multiplication, this would make the precedence of multiplication higher than that of addition. Of course, it is possible to write combinators to abstract out patterns like this. It is also possible to write a monolithic combinator that uses a [more efficient (but specialized) algorithm](http://en.wikipedia.org/wiki/Shunting-yard_algorithm) to build operator parsers.

### Other interesting areas to explore related to parsing

* _Monoidal and incremental parsing_: This deals with the much harder problem of being able to incrementally reparse input in the presences of inserts and deletes, like what might be nice to support in a text-editor or IDE. See [Edward Kmett's articles on this](http://comonad.com/reader/2009/iteratees-parsec-and-monoid/) as a starting point.
* _Purely applicative parsers_: If we stopped before adding `flatMap`, we would still have a useful library--context-sensitivity is not often required, and there are some interesting tricks that can be played here. See this [stack overflow answer](http://stackoverflow.com/questions/7861903/what-are-the-benefits-of-applicative-parsing-over-monadic-parsing) for a discussion and links to more reading.

### Single pass parsing

Lastly, we mentioned that the JSON parser we wrote in this chapter was rather dumb in that it only constructed a parse tree. This is unfortunate--we will typically then have to traverse this parse tree to extract some meaningful type from the parse tree after the fact. We could instead build up whatever type we wish _as we go_. The general idea is to parameterize our grammar on functions that describe what to do at each level of the parse tree:
  
~~~ scala
def array[A,B](value: Parser[A])(f: List[A] => B): Parser[B] = 
  "[" *> value.sep(",").map(f) <* "]"
  // `*>` ignores the left parse result, in this case the `"["`, 
  // and `<*` ignores the right parse result, in this case the `"]"`.
~~~

This parses a JSON array, but rather than building a `JArray`, immediately converts the result to `B` using the given function, where `B` might be some more meaningful type. The same strategy can be applied for the rest of the grammar.
