\part{Exercises}

\chapter*{Parser}

_This exercise is based on the parser exercises of (1) and the blog post series of evaluating DSLs (spoilers in the article!) (2)._

**Description**

The goal of this exercise is to parse, and evaluate expressions such as:
```haskell
"((x + 3) * (y + 5))"
"(((x + 3) * (y + 5)) * 5)"
"(x + y)"
...
```
it is up to you to define precisely the rules of this language, and to enforce (or not) the use of parentheses.

**Preliminaries**

Assume that we have the following definitions:

```haskell
type Id = String

data OperatorType = Add | Multiply
  deriving (Show, Eq)

data Expression =
  Constant Int
  | Variable Id
  | BinaryOperation OperatorType (Expression, Expression)

data Parser a = Parser { runParser :: String -> Maybe (a, String) }
```

**A) Parser**

1. Implement:
```haskell
charParser :: Char -> Parser Char
```
2. Implement:
```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = ...
-- such that
charParser c = satisfy (== c)
```
Useful predicates on characters are `isAlpha, isAlphaNum, isDigit, isSpace`, and are found in the `Data.Char` library.
3. Implement:
```haskell
intParser :: Parser Int
```
(possibly) useful library functions are:
```haskell
null :: [a] -> Bool
read :: Read a => String -> a -- specialize to Int
span :: (a -> Bool) -> [a] -> ([a], [a])
```
4. Provide instances for `Parser` for the following type classes:
    - `Functor`: (`<$>`) _given a function `a -> b` and a parser for `a`, return a parser for `b`._
    - `Applicative`: (`<*>`) _given a parser for a function `a -> b` and a parser for `a`, return a parser for `b`._
    - `Alternative`: (`<|>`) _given parsers for `a` and `b`, try to parse `a`; if and only if it fails, try to parse `b`._

    _Hint: Use the corresponding instances of `Maybe`. It is also a good exercise to implement these instances for `Maybe` yourself_.
5. Implement:
```haskell
oneOrMore :: Parser a -> Parser [a]
zeroOrMore :: Parser a -> Parser [a]
```
Use the alternative instance of `Parser`. In fact, these functions are already implemented for you in the Alternative type class as `many` and `some` respectfully.

    _Hint: implement both in terms of the other. For example, `oneOrMore` can be seen as parse one, then parse zero or more._
6. Implement
```haskell
spaces :: Parser String
```
that parses zero or more whitespace characters (use `isSpace`).
7. Implement
```haskell
idParser :: Parser Id
```
A valid identifier is (in most language) a string that starts with an alpha character, followed by zero or more alphanumeric characters (remember to use the character predicates available!).
8. Implement
```haskell
operatorParser :: Parser OperatorType
```
9. Combine the different parsers that you have made to make an expression parser:
```haskell
expressionParser :: Parser Expression
```
It may be useful for debugging to implement `show` for `Expression`:
    ```haskell
    instance Show Expression where
        -- show :: Expression -> String
        show expr = ...
    ```
Also look at the functions `(*>)` and `(<*)` for `Applicative` instances, which ignore the result of a computation but keep the side effect (use this to ignore whitespace).

**B) Evaluation**

We define the `Environment` as a map that holds (integer) values for variables.
```haskell
type Environment = Map Id Int
```
`Map` is found in the `Data.Map` library. See the documentation for usage.

1. Implement:
```haskell
evaluate :: Environment -> Expression -> Maybe Int
```
2. Implement:
```haskell
optimize :: Expression -> Expression
```
for example, `(0 + x)` can be replaced with just `x`, and `(1 + 3)` can just be evaluated to produce `4`, and so on (think of other optimizations).
3. Implement:
```haskell
partial :: Environment -> Expression -> Expression
```
that replaces all the variables in the epxression with those that have values in the environment, and leaves the others intact.
4. Observe that you can implement `evaluate` in terms of `partial` followed by `optimize`, and do this.
5. Make a function:
```haskell
dependencies :: Expression -> [Id]
```
returning the variables that occur in expression. Use the `Data.Set` library along with the functions `singleton, union, empty, toList`.

6. Use `dependencies` to improve your error messages by implementing a function
```haskell
result :: Expression -> Either String Int
```
That returns the result of an expression, or a string containing an error message along with the dependencies that are missing.

**C) Monadic parser**

1. Write the `Monad` instance of `Parser`.
2. Observe that `do`-notation for the Parser reads very naturally:
    ```haskell
    threeInts :: Parser [Int]
    threeInts = do
      x <- parseOneInt
      y <- parseOneInt
      z <- parseOneInt
      return [x, y, z]
      where
        parseOneInt = spaces *> intParser
    ```

---

*We will revisit our parser when we talk about __catamorphisms__.*

**References**

- (1): <http://cis.upenn.edu/~cis194/spring13/lectures.html>
- (2): <https://deque.blog/2017/01/17/catamorph-your-dsl-introduction/>

\chapter*{Monads}


**A) IO: Hangman**

_Taken from (1)_

**Description**

The goal is to make an interactive 'hangman' game in Haskell, so that:
```
./hangman
Enter a secret word: *******
Try to guess: h
h______
Try to guess: ha
ha___a_
Try to guess: hang
hang_an
Try to guess: hangman
You win!!!
```

**Preliminaries**

Assume that we have the following definitions:

```haskell
...
```

1. Implement:
    ...

**B) State: Simulating Risk battles**

_Taken from (2)_

**Description**

Simulate Risk

**Preliminaries**

Assume that we have the following definitions:

```haskell
...
```

**References**

- (1): <http://www.haskellbook.com>
- (2): CIS194

\chapter*{Folds}

For this exercise it is good to hide the fold operations from the Prelude so that you can implement them yourself.
```haskell
import Prelude hiding (foldr, foldl)
```

**A) Lists**

1. Implement the functions:
    ```haskell
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldr :: (a -> b -> b) -> b -> [a] -> b
    ```
2. Implement the following functions on lists using a fold:
```haskell
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
length :: Num b => [a] -> b
and :: [Bool] -> Bool
or :: [Bool] -> Bool
elem :: Eq a => a -> [a] -> Bool
min :: (Bounded a, Ord a) => [a] -> a
max :: (Bounded a, Ord a) => [a] -> a
all :: (a -> Bool) -> [a] -> Bool
any :: (a -> Bool) -> [a] -> Bool
concat :: [[a]] -> [a]
reverse :: [a] -> [a]
filter :: (a -> Bool) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
```
3. use foldMap to implement the following functions
```haskell
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
concat :: [[a]] -> [a]
asString :: Show a => [a] -> String
```

**B) Folds over either, maybe and binary trees**

1. Implement:
```haskell
foldm :: (a -> b -> b) -> b -> Maybe a -> b
folde :: (a -> b -> b) -> b -> Either c a -> b
```
Other useful 'fold-like' functions of `Maybe` and `Either` are
```haskell
maybe :: (a -> b) -> b -> Maybe a -> b
either :: (a -> c) -> (b -> d) -> Either a b -> Either c d
```
Implement them. They are also in the prelude.
2. Define a binary tree as:
```haskell
data Tree a = Node (Tree a) a (Tree a) | Leaf
```
and implement the function
```haskell
foldt :: (a -> b -> b) -> b -> Tree a -> b
```
using this implement e.g.
```haskell
sumTree :: Num a => Tree a -> a
productTree :: Num a => Tree a -> a
```

**C) Peano numbers**

_Modelled after section 1.2 of 'Algebra of programming' from Bird and de Moor._

Natural numbers can be represented a la Peano as:
```haskell
data Nat = Zero | Succ Nat
```
Or mathematically, we can say that any natural number can be represented recursively as $k = 0 \sqcup (n + 1)$, where $n$ is again a natural number.
Using this notation we can write a typical recursion formula:
$$f(m) = \begin{cases}
  c & \text{ if } m = 0 \\
  h(f(n)) & \text{ if } m = n + 1
  \end{cases}$$
or in code:
```haskell
f :: Nat -> b
f Zero = c
f (Succ x) = h (f x)
```
Here, `f` is completely determined by the functions `h :: b -> b` and `c :: b`.

1. Write a function `foldn` that encapsulates this pattern:
```haskell
foldn :: (b -> b) -> b -> Nat -> b
-- to see the similarity with foldr, write it as
-- foldn :: (() -> b -> b) -> b -> Nat -> b
-- (the discrepency is because `:k Nat = *`)
```
2. Implement the following functions using foldn
```haskell
sum :: Nat -> Nat -> Nat
product :: Nat -> Nat -> Nat
exponential :: Nat -> Nat -> Nat
factorial :: Nat -> Nat
fibonacci :: Nat -> Nat
```
    It may be convenient during testing to make some aliases for numbers:
```haskell
zero = Zero
one = Succ Zero
two = Succ one
three = Succ two
```
    and to implement `instance Show Nat where ...`.

    _Hint:_ Use `(Num, Num)` as `b` for `fact` and `fib`, and compose with `snd`.
3. Using `foldn`, implement:
```haskell
square :: Nat -> Nat
-- `last p n` returns the last natural number <= n that satisfies p
last :: (Nat -> Bool) -> Nat -> Nat
```
4. The Ackermann function is defined as:
$$A(m, n) =
\begin{cases}
n+1 & \mbox{if } m = 0 \\
A(m-1, 1) & \mbox{if } m > 0 \mbox{ and } n = 0 \\
A(m-1, A(m, n-1)) & \mbox{if } m > 0 \mbox{ and } n > 0.
\end{cases}$$
    Implement `curry ack` using `foldn`, where `ack :: (Nat, Nat) -> Nat`.

**D) Finding distinct elements of a list**

_From the Data61 Haskell course_

Recall the `State` monad, defined as `data State s a = State { runState :: s -> (a, s) }`.
```haskell
import qualified Data.Set as S
```
1. Implement a function
```haskell
filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
```
that takes a list, and an _effectful predicate_, and produces an effectful list containing only those elements satisfying the predicate.
2. Using `filtering`, with `State (Set a) Bool` as the `Applicative f`, implement:
```haskell
distinct :: Ord a => [a] -> [a]
```

\chapter*{Catamorphisms}

In this exercise we are going to play with catamorphisms and least fixed points.

I suggest that for each part you make a new Haskell file with on the top e.g.:
```haskell
{-# LANGUAGE ..., ... #-}

module CataList where

import Fix

...
```
Use `import` statements to resolve dependencies. This will prevent name collisions.

**A) The Fix type class**

Use the following GHC extension:
```haskell
{-# LANGUAGE RankNTypes #-}
```

As mentioned in the chapter on $F$-algebras, there are two (equivalent) ways to define least fixed points in Haskell, these are:
```haskell
data Fix f = Fix { unFix :: f (Fix f) }
data Fix' f = Fix' { unFix' :: forall a. (f a -> a) -> a }
```
1. Write a function `cata` that converts an $F$-algebra to a catamorphism:
```haskell
cata :: Functor f => (f a -> a) -> (Fix f) -> a
```

2. Write the isomorphisms between `Fix` and `Fix'`, i.e.:
```haskell
iso :: Functor f => Fix f -> Fix' f
invIso :: Functor f => Fix' f -> Fix f
```
_Hint_: `Fix'` is also called `flipCata`.

Note that the answers are described in the text, if you need help.

**B) Catamorph your lists**

_References:_

- <http://comonad.com/reader/2013/algebras-of-applicatives/>
- <https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/>

To define a list in the way described in Example \ref{exa:list_initial_algebra}, we write:
```haskell
data ListF a b = Nil | Cons a b
```
here $a$ is the fixed set $A$, and $b$ represents $X$. We want to find the least fixed point, we make an alias:
```haskell
type List a = Fix (ListF a)
```
read as: the least fixed point of the endofunctor `ListF a` (which, as we have seen, is just the usual description of a list).

1. Write functions that make constructing a list in this least fixed point description easier:
    ```haskell
    nil :: List a
    (<:>) :: a -> List a -> List a
    -- We want the cons function to be right associative
    infixr 5 <:>
    ```
2. Make a functor instance for `ListF a`:
    ```haskell
    instance Functor (ListF a) where
        ...
    ```
3. Given:
    ```haskell
    type Algebra f a = f a -> a
    -- an example list to work with
    testCase :: Fix (ListF Int)
    testCase = 2 <:> 3 <:> 4 <:> nil
    ```
    define functions:
    ```haskell
    sum' :: Algebra (ListF Int) Int
    square' :: Algebra (ListF Int) (List Int)
    ```
    And observe that you only have to define local transformations, and can let `cata` take care of the recursive structure:
    ```haskell
    main = do
        print $ (cata sum') testCase
        print $ (cata sum') $ (cata square') testCase
    ```
    In essence, you are writing the ingredients of a fold, but there is no specific reference to any fold or even to any list in `cata`. We abuse the fact that the recursive structure is encoded in the definition of the functor.

**C) Catamorph your expressions**

_Reference:_

- <https://deque.blog/2017/01/20/catamorph-your-dsl-deep-dive/>

Similar to lists, we can define our expression functor as:
```haskell
data ExprF b = Cst Int | Add (b, b)
type Expr = Fix ExprF
```
Corresponding to the endofunctor:
$$F(X) = \mathrm{Int}_{32} + X \times X.$$
Here, $\mathrm{Int}_{32}$ represents finite 32-bit integers, and `Expr` is the least fixed point of this functor.

1. Write convenience functions:
```haskell
cst :: Int -> Expr
add :: (Expr, Expr) -> Expr
```
2. Give the functor instance for `ExprF`:
    ```haskell
    instance Functor ExprF where
        ...
    ```
3. Implement:
    ```haskell
    eval :: Expr -> Int
    render :: Expr -> String
    ```
    Use `cata` and an algebra, i.e.:
    ```haskell
    function = cata algebra where
        algebra ...
    ```

4. Implement:
    ```haskell
    leftUnit :: ExprF Expr -> Expr
    rightUnit :: ExprF Expr -> Expr
    ```
    that optimize away additions with zero.
5. Implement:
    ```haskell
    comp :: (ExprF Expr -> Expr) ->
        (ExprF Expr -> Expr) ->
        (ExprF Expr -> Expr)
    ```
    that composes two algebras with the same carrier as the initial algebra, like `leftUnit` and `rightUnit`.
6. Implement
    ```haskell
    optimize :: Expr -> Expr
    ```
    using `comp` of `leftUnit` and `rightUnit`

**D) Modularize your catamorphed expressions**

_Reference:_

- "W. Swierstra; Data types a la carte" <http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf>

If we want to add e.g. multiplication to our little expression system defined above, we have to not only change the definition of `ExprF`, but also of all algebras that we defined after that. This problem has been summarized as follows:

> _The goal is to define a data type by cases, where one can add new cases to the data type_
> _and new functions over the data type, without recompiling existing code, and while retaining static type safety_
-- Dubbed the 'expression problem' by Phil Wadler in 1998

and is the subject of the functional pearl referenced above.

In this exercise we will implement the ideas given in that paper. The following GHC extensions are needed:
```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
```

First, instead of
```haskell
data Expr' b = Val' Int | Add' b b
```
like above, we will express the different components of the coproduct in our functor independently, as in:
```haskell
data Val e = Val Int
data Add e = Add e e
```
Note that `Val` does not depend on `e`, but is seen as a functor of `e` so that it is on the same level as the other parts of the coproduct (it is seen as a constant functor).

From the paper:

> The big challenge, of course, is to combine the ValExpr and AddExpr
> types somehow. The key idea is to combine expressions by taking the
> coproduct of their signatures

Here, ValExpr and AddExpr are defined as the least fixed points of the respective functors.

We do that using:
```haskell
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 5 :+:
```
1. Expressions now have the following signature:
    ```haskell
    addExample :: Fix (Val :+: Add)
    ```
    here, `Val :+: Add` represents the functor that we called `Expr'` before. Try to define a simple expression, like `2 + 3` using this system, and observe how incredibly clumsy this is. Later we will define some _smart constructors_.
2. Implement the following instances:
    ```haskell
    instance Functor Val where
        ...
    instance Functor Add where
        ...
    instance (Functor f, Functor g) => Functor (f :+: g) where
        ...
    ```
3. Now we are going to define a way to evaluate expressions, we do this by defining a new typeclass, effectively saying how to evaluate an algebra for each part of the coproduct that defines our final endofunctor.
    ```haskell
    class Functor f => Eval f where
        evalAlg :: f Int -> Int
    ```
    Implement:
    ```haskell
    instance Eval Val where
        ...
    instance Eval Add where
        ...
    instance (Eval f, Eval g) => Eval (f :+: g) where
        ...
    ```
    Finally, implement:
    ```haskell
    eval :: Eval f => Fix f -> Int
    ```
    that evaluates an expression (catamorph the algebra!)
4. From the paper:

    > The definition of addExample illustrates how messy expressions can easily
    > become. In this section, we remedy the situation by introducing smart
    > constructors for addition and values.

    to this end, we first define the following type class (which can look quite magical at first):
    ```haskell
    class (Functor sub, Functor sup) => sub :<: sup where
        inj :: sub a -> sup a
    ```
    you should read this as: `sub` can be used to construct a value for `sup`. In a way, the least fixed point for `sub` is a subset of the least fixed point for `sup`. For example, `sub` can be a term in `sup` if the latter is a coproduct.
    Implement:
    ```haskell
    instance Functor f => f :<: f where
        ...
    instance (Functor f, Functor g) => f :<: (f :+: g) where
        ...
    instance (Functor f, Functor g, Functor h, f :<: g) =>
        f :<: (h :+: g) where
        ...
    ```
    The astute Haskeller will note that there is some overlap in the second and third definitions. There is however no ambiguity as long as expressions involving `:+:` use no explicit parentheses.
    Implement also:
    ```haskell
    inject :: (g :<: f) => g (Fix f) -> Fix f
    ```
    to perform the injection in a least fixed point representation
5. Implement smart constructors:
    ```haskell
    val :: (Val :<: f) => Int -> Fix f
    (<+>) :: (Add :<: f) => Fix f -> Fix f -> Fix f
    -- make + left associative
    infixl 6 <+>
    ```
    Now we can construct expressions as:
    ```haskell
    expression :: Fix (Add :+: Val)
    expression = (val 30000) <+> (val 200)
    ```
6. We went through all this pain to end up with what the previous exercise already allowed us to do! Let us show the advantage of this system by adding support for multiplication. Implement the gaps in:
    ```haskell
    data Mul x = Mul x x

    instance Functor Mul where
        ...

    instance Eval Mul where
        ...

    (<#>) :: (Mul :<: f) => Fix f -> Fix f -> Fix f
    ...

    -- multiplication should bind tighter than addition
    infixl 7 <#>

    expression2 :: Fix (Val :+: Add :+: Mul)
    expression2 = (val 30000) <+> (val 200) <#> (val 300)
    ```
    Note that we did not have to touch any previous code!
7. We can also extend functionality beyond evaluating, again without retouching (and even without recompiling) previous code. Fill in the gaps of this pretty printer:
    ```haskell
    class Functor f => Render f where
        render :: Render g => f (Fix g) -> String

    pretty :: Render f => Fix f -> String
    ...

    instance Render Val where
        ...

    instance Render Add where
        ...

    instance Render Mul where
        ...

    instance (Render f, Render g) => Render (f :+: g) where
        ...
    ```
