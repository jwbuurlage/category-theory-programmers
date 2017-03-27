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

**D) Catamorphisms**

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


