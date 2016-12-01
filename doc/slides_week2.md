---
numbersections: false
documentclass: memoir
classoption: extrafontsizes
classoption: 17pt
header-includes:
    - \usepackage{tikz-cd}
    - \usepackage{amsthm}
    - \usepackage{epigraph}
    - \setlength{\epigraphwidth}{0.8\textwidth}
    - \newtheoremstyle{custom}
        {0.5 cm}
        {0.5 cm}
        {}
        {}
        {\bfseries}
        {.}
        {.5em}
        {}
    - \theoremstyle{custom}
    - \newtheorem{theorem}{Thm}
    - \newtheorem{definition}{Def}
    - \newtheorem{example}{Ex}
    - \setlength{\columnsep}{1cm}
    - \usepackage[margin=1.5cm,landscape,twocolumn]{geometry}
    - \setlength{\parindent}{0em}
    - \setlength{\parskip}{0.5cm}
---

**Category of types**

\begin{figure}[h]
\centering
\begin{tikzcd}
\texttt{Int} \arrow[d] \arrow[r] \arrow[rd] & \texttt{Float} \arrow[dl]\\
\texttt{[Int]} \arrow[r] & \texttt{String}
\end{tikzcd}
\end{figure}

*Types are sets*

- `Integer` $\mapsto \mathbb{Z}$
- `Real` $\mapsto \mathbb{R}$
- `Char` $\mapsto \{ \ldots, a, b, c, \ldots, A, B, C, \ldots \} = S$
- `String` $\mapsto S^* = \{ (), (a, b), (c, w, i), \ldots \}$
- `(Integer, Real)` $\mapsto \mathbb{Z} \times \mathbb{R}$

Programming constructs and mathematical operations, e.g.:

- `struct` (POD): cartesian products
- `union` (tagged, `std::variant`): disjoint unions

We work with **Type** $\cong$ **Set**, maybe set it to **Hask** later.

\newpage

**Haskell**

More accurately models a category than other languages

- All functions are *pure*, no side effects.
- Identity function, composition are all built in:
```haskell
-- identity
id :: a -> a
-- composition
h = f . g
```
- `void` in C corresponds to $\{ * \}$, not $\emptyset$! There is no type corresponding to $\emptyset$ in C. In Haskell there is `Void` and even the function:
```haskell
absurd :: Void -> a
```
(that can never be called!)

- The singleton set $\{ * \}$ is denoted by the type `()` in Haskell, the single value for the type is denoted the same. This allows:
```haskell
-- a function without arguments
f :: () -> Int
f () = 0
-- calling the function
b <- f()
```
- All 'unions' are tagged
- For **Hask** all types a single value for 'crashing' is added: $\perp$ or *bottom*. Technicality which we will ignore (and that causes a lot of trouble formally)

What are the initial objects and terminal objects in the *category of types*?

**Words**

\begin{definition}
For a set $X$, \emph{words in $X$}, are finite tuples of elements of $X$:
$$(x_1), (x_1, x_2, x_3), (), \ldots$$
If we want to construct a \emph{word functor}:
$$T: \mathbf{Set} \to \mathbf{Set}$$
then $T$ would send
\begin{align*}
T&: X \mapsto X^*\\
 &: (f: X \to Y) \mapsto (Tf: X^* \to Y^*)
\end{align*}
For this second option, we have an obvious candidate for the precise function, let $f: X \to Y$ be some map, then $Tf$ maps a word in $X$ gets to a word in $Y$ in the following way:
$$Tf(x_1, x_2, ... x_n) = (f(x_1), f(x_2),, \ldots, f(x_n)).$$
\end{definition}

Translating from **Set** to **Type**, this functor corresponds to 'lists of objects of a certain type':

- C++: `T` $\to$ `std::vector<T>`
- NumPy: `dtype` $\to$ 'numpy array with member `dtype`'

**Type classes and constructors**

Type constructor is a function that *constructs a type from a type*.

A type constructor is combined with *value constructors*.

```haskell
data Bool = True | False
data Either a b = Left a | Right b
```

Can be differentiated between using *pattern matching*.

A *type class* is a family of *types* that have a *common interface*, e.g.:

```haskell
class Eq a where
  (==) :: a -> a -> Bool

class Functor F where
    fmap :: (a -> b) -> (F a -> F b)
```

**List functor**

```haskell
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x t) =
        Cons (f x) (fmap f t)
```

In Haskell: `[]` is used for the functor `List`, as well as for the value constructor `Nil`, and `Cons` is written as `:` (infix):

```haskell
x = 1 : 2 : [] -- => [1, 2] :: [Int]
```

**Maybe functor**

Useful for exceptions!

- C++: `std::optional`
- Python: e.g.
```python
def fn(a):
        if (a >= 0)
            return sqrt(a)
        return None
```
- Haskell:

```haskell
data Maybe = Nothing | Just a

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
```

**Natural transformations**

\begin{definition}
A \textbf{natural transformation} $\mu$ between two functors $F, G: \mathcal{C} \to \mathcal{D}$ is a family of morphisms:
$$\mu = \{ \mu_a: Fa \to Fb~|~a \in \mathcal{C} \},$$
indexed by objects in $\mathcal{C}$, so that for all morphisms $f: a \to b$ the diagram

\begin{figure}[h]
\centering
\begin{tikzcd}
Fa \arrow[r, "\mu_a"] \arrow[d, "Ff"] & Ga \arrow[d, "Gf"] \\
Fb \arrow[r, "\mu_b"] & Gb
\end{tikzcd}
\end{figure}

commutes. This diagram is called the \emph{naturality square}.  We write $\mu: F \Rightarrow G$, and call $\mu_a$ \emph{the component of $\mu$ at $a$}.
\end{definition}

We can \emph{compose} natural transformations, turning the set of functors from $\mathcal{C} \to \mathcal{D}$ into a category. Let $\mu: F \Rightarrow G$ and $\nu: G \Rightarrow H$, then we have $\nu \circ \mu: F \Rightarrow H$ defined by (in components):
$$(\nu \circ \mu)_a = \nu_a \circ \mu_a.$$
Where the composition of the rhs is simply composition in $\mathcal{D}$.

**Polymorphic functions**

Parametric (vs ad-hoc) polymorphism:

- A _parametric polymorphic_ function should work (uniformly) for *all types*.
- No overloading or (partial) specialization.
- 'Substituting in the type', not changing the body of the function.

```haskell
head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = x
```

This is a polymorphic function, but also a *natural transformation between the list functor and the maybe functor*.

Naturality condition *guaranteed because of parametric polymorphism*:

```haskell
fmap f . head = head . fmap f
```

- lhs: fmap for Maybe
- rhs: fmap for List

- `[x,y,z, ...] -> x -> f(x)`
- `[x,y,z, ...] -> [f(x), f(y), f(z), ...] -> f(x)`
- Naturality condition gives compiler tools to optimize.

