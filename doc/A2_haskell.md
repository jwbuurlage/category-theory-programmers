\appendix

\chapter{Short introduction to Haskell}

Here we will give an introduction to programming using Haskell. It will not be an extensive introduction, in fact it will be very brief. However, studying this section should be enough to allow you to follow along with the rest of the text even if you have no experience with Haskell. You are encouraged to look for additional material online, see also the references at the end of this section. You are assumed to have access to the Glasgow Haskell Compiler (GHC) and its interactive REPL GHCi.

To follow along, open `ghci` and play around with the code snippets that we provide.

We will dicuss te topics suggested by the NICTA Haskell course^[https://github.com/NICTA/course].

**Values and assignment**

A value can be assigned to a variable as follows:
```haskell
let x = 'a'
let y = 3
let xs = [1,2,3]
let f x = x * x
let g x y = x * y
```
We note that these variables are only valid inside an expression, using a:
```haskell
let [variable = value] in [expression]
```
syntax, but you can also use this style of variable definition inside `ghci`.

**Type signatures**

In GHCi, you can see the type of the variable using:
```haskell
:t x -- x :: Char
:t y -- y :: Num a => a
:t xs -- g :: Num t => [t]
:t f -- f :: Num a => a -> a
:t g -- g :: Num a => a -> a -> a
```
Here `::` means "has the type of".

The `->` in a type is right associative, i.e.
```haskell
a -> a -> a == a -> (a -> a)
```
and so on. You can read this as 'for an `a`, we get a function from `a` to `a`'.

**Functions are values**

Functions can be used as arguments to other (higher order) functions. E.g.
```haskell
:t (2*) -- Num a => a -> a
map :: (a -> b) -> [a] -> [b]
map (2*) xs -- [2,4,6]
```
Here we *map* a function over a list.

**Functions take arguments**

On thing to notice about the `map` example, is that it although it is a function that technically takes a single argument (and produces a function from a list to a list), it can also be viewed as a function of two arguments. We will not explicitely distinguish between these two views.

We can also make anonymous 'lambda' functions:
```haskell
map (\x -> x * x) xs -- [1,4,9]
```
The backslash is inteded to look like a $\lambda$.

**Functions can be composed**

In Haskell there are three alternative ways of composing functions (to prevent overuse of parenthesis):
```haskell
g(f 123)
g $ f 123
(g . f) 123
```
Here, `$` makes sure that all the functions on the right have been evaluated before statements on the left come in to play.

**Infix operators**

An operator starts with a non-alphanumeric character, e.g. `+`, `++`, `>>=`, `:` are all operators, and they use *infix* notation by default. For example:
```haskell
1 + 2 -- 3
[1,2] ++ [3,4] -- [1,2,3,4]
1 : [2,3] -- [1,2,3]
```
To use them with *prefix* notation, we surround them with parenthesis:
```haskell
(+) 1 2 -- 3
```
Any function (which by default uses prefix notation) can be used infix as well using backticks:
```haskell
let f x y = x * x + y * y
2 `f` 3 -- 13
```
this can make code significantly more clear when defining e.g. operations that act on multiple lists, sets, or maps.

**Polymorphism**

We already saw the type signature of `map`:
```haskell
map :: (a -> b) -> [a] -> [b]
```
This is an example of a polymorphic function, it is defined for any type `a` and `b`. We refer to these 'wildcard types' as *type variables*. These always start with a lowercase letter.

**Data types**

To work with custom data structures, we create new *data types*. These are declared as follows:
```haskell
data DataTypeName a b = Zero | One a | One' b | Both a b
```
A data type is declared using the `data` keyword, and the *type constructor* is given a name (here `DataTypeName`). A data type depends on a number of type variables, here `a` and `b`. After the `=` sign, there are zero or more *data constructors*, here `Zero`, `One`, `One'`, and `Both`, each depending on one or more of the type variables of the type constructor and separated by a pipe `|`.

Data constructors can be used for *constructing* a value of the data type, or for pattern-matching on values of the data type (i.e. retrieve which constructor was used to construct the given value).

**Type classes**

Type classes are a way to have *ad-hoc polymorphism* in Haskell, while the ordinary polymorphic functions discussed before are *parametric*. This means that we can have different behaviour for different types. Type classes are introduced as follows:
```haskell
class Eq a where
  (==) :: a -> a -> Bool
```
Here, we state that in order for a type `a` to be part of the *type class* `Eq`, we have to implement an equality function with the given signature. We can then restrict functions definitions to only work on types in this type class in the following manner:
```haskell
(!=) :: Eq a => a -> a -> Bool
x != y = not (x == y)
```

**Monoids, Functors, Applicative and Alternative**

Here we give a whirlwind tour of some interesting type classes used in Haskell, the majority of the category theory that we will discuss will explain the mathematical background and uses of these typeclasses in detail, here we summarize the resulting classes as a reference. Feel free to skip or skim them, and to come back after studying the material presented in later chapters.

_Monoid_

Many types have one (or even multiple) _monoidal_ structure, which means that it is possible to combine two elements to a single element, and that this way of combining has some special (but common) properties.
```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m -- infix operator alias: <>
```
The implementations depend on the type `m`, but when implementing a Monoid instance, it is the task of the implementor to adher to the following laws:
```haskell
-- forall x, y, z :: m
x <> mempty == x -- identity
mempty <> x == x
(x <> y) <> z == x <> (y <> z) -- associativity
```

For example, the following are all possible Monoid instances (given as `(m, mempty, mappend)`):

- `(Int, 0, (+))`
- `(Int, 1, (*))`
- `(Int32, minBound, max)`
- `(Int32, maxBound, min)`
- `(String, "", (++))`
- `(Maybe, Nothing, (<|))`, here `(<|)` denotes the binary function that yields the left-most non-`Nothing` value if anything (obviously there is also a right-most equivalent `(|>)`).

and so on.

_Functor_

A functor can take an 'ordinary' function, and apply it to a `context`. This context can be a list, the result of a computation that may have failed, a value from input/output and so on. You can also view the functor itself as the context.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b -- infix operator alias: <$>
```
Again, each instance should satisfy certain laws. For `Functor`, these are:
```haskell
-- forall f, g :: a -> b
fmap id == id
fmap (f . g) == fmap f . fmap g
```
For example, the `List` functor is implemented as:
```haskell
instance Functor [] where
    fmap = map
```
Or the 'composition' functor:
```haskell
instance Functor ((->) c) where
    -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
    fmap = (.)
```

_Applicative_

The most obvious use of applicative is to lift functions of multiple arguments into a context. If we have a function of multiple arguments like:
```haskell
g :: a -> b -> c
```
Then we can't just list it into a functor (context), since we would obtain:
```haskell
fmap g :: f a -> f (b -> c)
```
If we compose it with a function that has the signature
```haskell
apply :: f (b -> c) -> f b -> f c
```
then we obtain:
```haskell
apply . fmap g :: f a -> f b -> f c
```
If we implement `apply`, then we can lift functions with an arbitrary number of arguments (by iteratively calling `apply` after `fmap`). A functor with `apply` is called an _applicative functor_, and the corresponding type class is:
```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    ap :: f (a -> b) -> f a -> f b -- infix operator alias: <*>
```
we see that additionally, `pure` is introduced as a way to put any value into an applicative context. Any applicative instance has to satisfy the following laws:
```haskell
-- forall v, w :: a; x, y, z :: f a; g :: a -> b
pure id <*> x = x -- identity
pure (.) <*> x <*> y <*> z = x <*> (y <*> z) -- composition
pure g <*> pure v = pure (g v) -- homomorphism
y <*> pure v = pure ($ y) <*> v -- interchange
```

_Alternative_

Now that we have introduced some terminology, we can introduce Alternative functors as _giving an applicative context a monoidal structure_.
```haskell
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
```

For example, for `Maybe` we can say:
```haskell
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> right = right
    left <|> _ = left
```
Or for `List` we have the standard concatenation monoid:
```haskell
instance Alternative [] where
    empty = []
    (<|>) = (++)
```

_Monads_

A functor lets you lift functions to the functorial context. An applicative functor lets you untangle functions caught in a context (this can be e.g. an artifact of currying functions of multiple arguments) to functions in the functorial context. Another useful operation is to compose functions whose _result lives inside the context_, and this is done through bind `>>=` (with its flipped cousin `=<<`).

To illustrate the similarities between the typeclasses `Functor => Applicative => Monad`:
```haskell
(<$>) :: (a -> b)   -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: a -> f b   -> f a -> f b
```
For us, the interesting part of the definition is:
```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```
The default implementation of `return` is to fall back on `pure` from applicative. The bind operation has to satisfy the following laws:
```haskell
-- forall v :: a; x :: m a; k :: a -> m b, h :: b -> m c
return v >>= k = k v
x >>= return = x
m >>= (\y -> k y >>= h) = (m >>= k) >>= h
```
Thus bind takes a monadic value, and shoves it in a function expecting a non-monadic value (or it can bypass this function completely). A _very_ common usage of bind is the following.

```haskell
x :: m a
x >>= (\a -> {- some expression involving a -})
```
which we can understand to mean that we _bind_ the name `a` to whatever is inside the monadic value `x`, and then we can reuse it in the expressions that follow. In fact, this is so common that Haskell has convenient syntactic sugar for this pattern called `do`-notation. This notation is recursively desugared according to the following rules (taken from Stephen Diehl's "What I wish I knew when learning Haskell"):
```haskell
do { a <- f; m }  ~>  f >>= \a -> do { m }
do { f; m }  ~>  f >> do { m }
do { m }  ~>  m
```
Curly braces and semicolons are usually omitted. For example, the following two snippets show the sugared and desugared version of `do`-notation:
```haskell
do
  a <- f
  b <- g
  c <- h
  return (a, b, c)

f >>= \a ->
  g >>= \b ->
    h >>= \c ->
      return (a, b, c)
```

Monads can be used to do all kinds of things that are otherwise relatively hard to do in a purely functional language such as Haskell:

- Input/output
- Data structures
- State
- Exceptions
- Logging
- Continuations (co-routines)
- Concurrency
- Random number generation
- ...

_Folds_

Folds^[A _fold_ is also known as _reduce_ or _accumulate_ in other languages] are an example of a _recursion scheme_. You could say that (generalized) folds in functional languages play a similar role to `for`, `while`, ... statements in imperative languages.  There are two main higher-order functions for _folds_ in Haskell:
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldr :: (a -> b -> b) -> b -> [a] -> b
```
Here, `foldl` associates to the left, and `foldr` associates to the right. This means:
```haskell
foldl (+) 0 [1, 2, 3]
-- ~> ((1 + 2) + 3)
foldr (+) 0 [1, 2, 3]
-- ~> (1 + (2 + 3))
```
E.g. `foldr` can be implemented as:
```haskell
foldr f x xs = case xs of
    [] -> x
    (y:ys) -> y `f` (foldr f x ys)
```

_Foldable_

Lists are not the only data structure that can be folded. A more general signature of `foldr` would be:
```haskell
foldr :: (a -> b -> b) -> b -> t a -> b
```
where `t` is some _foldable data structure_. There is a type class, with the following core functions:
```haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m
```
Only one of these two functions has to be implemented, the one can be retrieved from the other. Here, `foldMap` maps each element of a foldable data structure into a monoid, and then uses the operation and identity of the monoid to fold. There is also a general `fold` method for each `Foldable`:
```haskell
fold :: Monoid m => t m -> m
-- e.g. for list, if `x, y, z :: m`:
fold [x, y, z]
-- ~> x <> y <> z <> mempty
```
The more 'natural' fold in Haskell is `foldr`, to understand why we should look at the difference between `cons`-lists and `snoc`-lists:
```haskell
List a  = Empty  | Cons a (List a) -- 'cons'
List' a = Empty' | Snoc (List a) a -- 'snoc'
```
The standard Haskell lists `[a]` are `cons`-lists, they are built _from the back of the list_. The reason `foldr` is more natural for this type of list is that the recursion structure follows the structure of the list it self:
```haskell
h = foldr (~) e
-- h [x, y, z] is equal to:
h (x : (y : (z : [])))
--   |    |    | |
--   v    v    v v
--(x ~ (y ~ (z ~ e)))
```
This can be summarized by saying that a `foldr` _deconstructs_ the list, it uses the shape of the construction of the list to obtain a value. The `(:)` operation gets replaced by the binary operation `(~)`, while the empty list (base case) is replaced by the _accumulator_ `e`.

As a special case, since the value constructors for a list are just functions, we can obtain the identity operation on lists as a fold:
```haskell
id :: [a] -> [a]
id == foldr (:) []
`````

\section*{References}

If you want to learn Haskell, the following resources are helpful as a first step:

- 5 minute tutorial to get an idea:
    * <https://tryhaskell.org/>
- The wiki book on Haskell is quite good:
    * <https://en.wikibooks.org/wiki/Haskell>
- There is an excellent accessible Haskell book coming out soon, but it can be found already:
    * <http://haskellbook.com/>
- A cult-classic Haskell book:
    * <http://learnyouahaskell.com/chapters>
- If you are looking to do exercises, there is a guide to different courses available here:
    * <https://github.com/bitemyapp/learnhaskell>
- A handy search engine for library functions is Hoogle:
    * <https://www.haskell.org/hoogle/>
- Advanced topics for Haskell:
    * <http://dev.stephendiehl.com/hask/>


