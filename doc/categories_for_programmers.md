% Category Theory for Programmers
% Jan-Willem Buurlage
% November 21, 2016

---
numbersections: true
documentclass: book
classoption: oneside
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
    - \newtheorem{theorem}{Theorem}
    - \newtheorem{definition}{Definition}
    - \newtheorem{example}{Example}
    - \usepackage{geometry}
    - \geometry{margin=4cm}
---

This document contains notes for a small-scale seminar on category theory in the context of (functional) programming, organized at CWI. The goal of the seminar is to gain familiarity with concepts of category theory that apply (in a broad sense) to the field of functional programming. It could be an idea to have an associated (toy) project that examplifies the concepts that are discussed.

Although the main focus will be on the mathematics, examples should be made in Haskell to illustrate how to apply the concepts, and possibly examples in other languages as well (such as Python and C++).

# Categories

## Core definitions

We start with giving the definition of a category:

> **Definition**: A *category* $\mathcal{C} = (O, A)$ is a set $O$ of \emph{objects} and $A$ of \emph{arrows} between these objects, along with a notion of \emph{composition $\circ$ of arrows} and a notion of an identity arrow $\text{id}_a$ for each object $a \in O$.

> The composition operation and identity arrow should satisfy the following laws:

> - *Composition*: If $f: a \to b$ and $g: b \to c$ then $g \circ f: a \to c$.

\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[r, "f"] \arrow[rr, "g \circ f", bend right=50] & b \arrow[r, "g"]  & c
\end{tikzcd}
\end{figure}

> - *Composition with identity arrows*:  If $f: x \to a$ and $g: a \to x$ where $x$ is arbitrary, then:
> $$ \text{id}_a \circ f = f,~g \circ \text{id}_a = g.$$


\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, "g"', bend right=20] & x \arrow[l, "f"', bend right=20]
\end{tikzcd}
\end{figure}

> - *Associativity*: If $f: a \to b$, $g: b \to c$ and $h: c \to d$ then:
> $$(h \circ g) \circ f = h \circ (g \circ f).$$
> This is the same as saying that the following diagram commutes:

\begin{figure}[h]
\centering
\begin{tikzcd}[sep=huge]
a \arrow[r, "f"] \arrow[d, "h \circ g \circ f"'] \arrow[rd, crossing over, near start, "g \circ f"] & b  \arrow[d, "g"] \arrow[ld, crossing over, near end, "h \circ g"] \\
d & c \arrow[l, "h"]
\end{tikzcd}
\end{figure}
> Saying a diagram commutes means that for all pairs of vertices $a'$ and $b'$ all paths from between them are equivalent (i.e. correspond to the same arrow of the category).

If $f: a \to b$, then we say that $a$ is the *domain* and $b$ is the *codomain* of $b$. It is also written as:
$$\text{dom}(f) = a,~\text{cod}(f) = b.$$
The composition $g \circ f$ is only defined on arrows $f$ and $g$ if the domain of $g$ is equal to the codomain of $f$.

We will write for objects and arrows respectively simply $a \in \mathcal{C}$ and $f \in \mathcal{C}$, instead of $a \in O$ and $f \in A$.

**Examples:**

Some examples of familiar categories:

| Name     | Objects            | Arrows                 |
|----------|--------------------|------------------------|
| **Set**  | sets               | maps                   |
| **Top**  | topological spaces | continuous functions   |
| **Vect** | vector spaces      | linear transformations |
| **Grp**  | groups             | group homomorphisms    |

In all these cases, arrows correspond to functions, although this is by no means required. All these categories correspond to objects from mathematics, along with *structure preserving maps*. **Set** will also play a role when we discuss the category **Hask** when we start talking about concrete applications to Haskell.

There are also a number of very simple examples of categories:

- **0**, the empty category $O \equiv A \equiv \emptyset$.
- **1**, the category with a single element and (identity) arrow:

\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"]
\end{tikzcd}
\end{figure}

- **2**, the category with a two elements and a single arrow between these elements

\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, "f"] & b \arrow[loop right, "\text{id}_b"]
\end{tikzcd}
\end{figure}

Another example of a category is a \emph{monoid}, which is a specific kind of category with a single object. A monoid is a set $M$ with a associative binary operation $(\cdot): S \times S \to S$ and a unit element (indeed, a group without necessarily having inverse elements, or a \emph{semi-group with unit}).

This corresponds to a category $\mathcal{C}(M)$ where:

- There is a single object (for which we simply write $M$)
- There are arrows $s: M \to M$ for each element $s \in M$.
- Composition is given by the binary operation of the monoid: $s_1 \circ s_2 \equiv s_1 \cdot s_2$.

## Functors

A functor is a map between categories. This means it sends objects to objects, and arrows to arrows.

> **Definition**: A *functor* $T$ between categories $\mathcal{C}$ and $\mathcal{D}$ consists of two functions (both denoted simply by $T$):
>
> - An *object function* that maps objects $a \in \mathcal{C}$: $a \mapsto Ta \in \mathcal{D}$
> - An *arrow function* that assigns to each arrow $f: a \to b$ in $\mathcal{C}$ an arrow $Tf: Ta \to Tb$ in $\mathcal{D}$, such that:
> $$T(\text{id}_a) = \text{id}_{Ta},~T(g \circ f) = Tg \circ Tf.$$

A functor is a very powerful concept, since intuitively it allows you to translate between different branches of mathematics! They also play an important role in functional programming. Where among many other things, they are useful for defining the type of _containers_.

Functors can be composed, and this allows one to define a category of categories\footnote{Actually, there is some technicalities to be worked out and the resulting category consists of 'small categories' only.}, where the arrows are functors.

**Examples**

* The identity functor: $\text{id}_{\mathcal{C}}: \mathcal{C} \to \mathcal{C}$ is defined as:
\begin{align*}
\text{id}_{\mathcal{C}}:~&a \mapsto a\\
&f \mapsto f
 \end{align*}

* The constant functor $\Delta_d: \mathcal{C} \to \mathcal{D}$ for fixed $d \in \mathcal{D}$:
\begin{align*}
\Delta_{d}:~&a \mapsto d\\
&f \mapsto \text{id}_d
 \end{align*}

* The 'power-set functor': $\mathcal{P}:$ **Set** $\to$ **Set** sends subsets to their image under maps. Let $A, B \in$ **Set**, $f: A \to B$ and $S \subset A$:
\begin{align*}
\mathcal{P}A &= \mathcal{P}(A),\\
\mathcal{P}f&: \mathcal{P}(A) \to \mathcal{P}(B),~S \mapsto f(S)
\end{align*}

## Special objects, arrows and functors

\subsection*{Special objects}

For objects, we distuinguish two special kinds:

\begin{definition}
An object $x \in \mathcal{C}$ is \textbf{terminal} if for all $a \in \mathcal{C}$ there is exactly one arrow $a \to x$. Similarly, it is \textbf{initial} if there is exactly one arrow $x \to a$ to all objects.
\end{definition}


\begin{figure}[h]
\centering
\begin{tikzcd}[sep=tiny]
  &a \arrow[dr]&  \\
i\arrow[ur] \arrow[rr] \arrow[dr] & & t\\
  &b \arrow[ur] &
\end{tikzcd}
\end{figure}
Here, $i$ is initial, and $t$ is terminal.

\subsection*{Special arrows}

There are a number of special kind of arrows:

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is a \textbf{monomorphism} (or simply mono), if for all objects $x$ and all arrows $g, h: x \to a$ and $g \neq h$ we have:
$$g \circ f \neq h \circ f.$$
\end{definition}

To put this into perspective, we show that in the category \textbf{Set} monomorphisms correspond to injective functions;

\begin{theorem}
In \textbf{Set} a map $f$ is mono if and only if it is an injection.
\end{theorem}

\begin{proof}
Let $f: A \to B$. Suppose $f$ is injective, and let $g, h: X \to A$. If $g \neq h$, then $g(x) \neq h(x)$ for some $x$. But since $f$ is injective, we have $f(g(x)) \neq f(h(x))$, and hence $h \circ f \neq h \circ f$, thus $f$ is mono.

For the contrary, suppose $f$ is mono. Let $\{ * \}$ be the set with a single element. Then for $x \in A$ we have an arrow $\{ * \} \to A$ corresponding to the constant function $\tilde{x}(*) = x$, then $f \circ \tilde{x}(*) = f(x)$. Let $x \neq y$. Since $f$ is mono, $(f \circ \tilde{x})(*) \neq (f \circ \tilde{y})(*)$, and hence $f(x) \neq f(y)$, thus $f$ is an injection.\qedhere
\end{proof}

There is also an generalization of the notion of \emph{surjections}.

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is a \textbf{epimorphism} (or simply epi), if for all objects $x$ and all arrows $g, h: b \to x$ we have:
$$g \circ f = h \circ f \implies g = h.$$
\end{definition}

Finally, we introduce the notion of an 'invertible arrow'.

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is an \textbf{isomorphism} if there exists an arrow $g: b \to a$ so that:
$$g \circ f = \text{id}_a~\text{ and }~f \circ g = \text{id}_b.$$
\end{definition}

(Introduce split, sections, retractions here? It is already a lot). In set, epi and mono imply iso. This however does not hold for general categories!

\subsection*{Special functors}

Finally, we turn our attention to special kinds of functors. For this we first introduce the notion of a _hom-set_ of $a$ and $b$, the set\footnote{Here we assume that this collection is a set, or that the category is so-called \emph{locally small}} of all arrows from $a$ to $b$:
$$\text{Hom}_\mathcal{C}(a, b) = \{ f \in \mathcal{C}~|~f: a \to b \}.$$

\begin{definition}
A functor $F: \mathcal{C} \to \mathcal{D}$ is \textbf{full} if for all pairs $a, b \in \mathcal{C}$ the induced function:
\begin{align*}
F:~\text{Hom}_\mathcal{C}(a, b) &\to \text{Hom}_\mathcal{C}(Fa, Fb),\\
   f &\mapsto Ff
\end{align*}
is a surjection. It is called \textbf{faithful} if it is an injection.
\end{definition}

When after applying $F$ an arrow $Ff$ or an object $Fa$ has a certain property(i.e. being initial, terminal or epi, mono), it is implied that $f$ (or $a$) had this property, then we say the \emph{$F$ \textbf{reflects} the property}.

This allows for statements such as this:

\begin{theorem}
A faithful functor reflects epis and monos.
\end{theorem}

\begin{proof}
As an example we will prove it for a $Ff$ that is mono. Let $f: a \to b$ such that $Ff$ is mono, and let $h,g: x \to a$ such that $h \neq g$.

\begin{figure}[h]
\centering
\begin{tikzcd}[sep=huge]
x \arrow[dr, "F"] \arrow[r, bend right=20, "g"'] \arrow[r, bend left=20, "h"]& a \arrow[dr, "F"]  \arrow[r, "f"] & b \arrow[dr, "F"]  &\\
& Fx \arrow[r, bend right=20, "Fg"'] \arrow[r, bend left=20, "Fh"]& Fa \arrow[r, "Ff"] & Fb\\
\end{tikzcd}
\end{figure}

Since $g \neq h$ and $F$ is faithful, we have $Fg \neq Fh$. This implies, because $Ff$ is mono, that $Ff \circ Fg \neq Ff \circ Fh$, and since $F$ is a functor we have $F(f \circ g) \neq F(f \circ h)$, implying $f \circ g \neq f \circ h$, and hence $f$ is mono.

\qedhere
\end{proof}

## Natural transformations

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

## References:

- 1.1 -- 1.4 and 1.8 of Mac Lane
- 1.1, 1.2, 2.1, 3.1 and 3.2 of Asperti and Longo
- 2.1, 2.7, 2.8, 3.1, 4.2, 4.3 of Barr and Wells
- 1.1, 1.7, 1.10 of the 'Category Theory for Programmers' blog by Bartosz Milewski (best to study after reading Chapter 2)

# Types and functions: a category for programmers

\epigraph{""A monad is a monoid in the category of endofunctors, what's the problem?"}{\emph{James Iry jokes about Haskell in his blog post A Brief, Incomplete, and Mostly Wrong History of Programming Languages}}

To establish a link between functional programming and category theory, we need to find a category that is applicable. Observe that a _type_ in a programming language, corresponds to a _set_ in mathematics. Indeed, the type `int` in C based languages, corresponds to some finite set of numbers, the type `char` to a set of letters like `'a'`, `'z'` and `'$'`, and the type `bool` is a set of two elements (`true` and `false`). This category, the category of types, turns out to be a very fruitful way to look at programming.

Why do we want to look at types? Programming safety and correctness. In this part we will hopefully give an idea of how category theory applies to programming, but we will not go into to much detail yet, this is saved for later parts.

We will take as our model for the category of types the category **Set**. Recall that the elements of **Set** are sets, and the arrows correspond to maps. There is a major issue to address here: Mathematical maps and functions in a computer program are not identical (bottom value $\perp$). We may come back to this, but for now we consider **Set**, although we will refer to the category **Hask** in the following (it is enough to think of it as **Set**).

In Haskell, we can express that an object has a certain type:

```haskell
    a :: Integer
```

In C++ we would write:

```cpp
    int a;
```

To define a function $f: A \to B$ from type $A$ to type $B$ in Haskell:

```haskell
    f :: A -> B
```

To compose:

```haskell
    g :: B -> C
    h = f . g
```

This means that `h` is a function `h :: A -> C`! Note how easy it is to compose functions in Haskell. Compare how this would be in C++, if we were to take two polymorphic functions in C++ and compose them:

```cpp
    template <typename F, typename G>
    auto operator*(G g, F f) {
        return [&](auto x) { return g(f(x)); };
    }


    int main() {
        auto f = [](int x) -> float { return ...; };
        auto g = [](float y) -> int { return ...; };

        std::cout << (g * f)(5) << "\n";
    }
```

We need some additional operations to truly turn it into a category. It is easy to define the identity arrow in Haskell (at once for all types):

```haskell
    id :: A -> A
    id a = a
```

in fact, this is part of the core standard library of Haskell (the Prelude) that gets loaded by default. Ignoring reference types and e.g. `const` specifiers, we can write in C++:

```cpp
    template <typename T>
    T id(T x) {
        return x;
    }
```

There is one issue we have glared over; in mathematics all functions are *pure*: they will always give the same output for the same input. This is not always the case for computer programs, using IO functions, returning the current date, using a global variable are all examples of impure operations that are common in programming. In Haskell, *all functions are pure*, and this is a requirement that allows us to make the mapping to the category **Set**. The mechanism that allows Haskell programs to still do useful things is powered by *Monads*, which we will discuss later.

Although many of the things we will consider can apply to other languages (such as Python and C++), there is a strong reason why people consider often consider Haskell as an example in the context of category theory and programming; it originates in academia and therefore takes care to model the language more accurately. For example, since we take as our model the category **Set**, there should be a type that corresponds to the empty set $\emptyset$. In C / C++, the obvious candidate would be `void` for this set, but consider a function definition:
```cpp
void f() { ... };
```
This can be seen as a function from `void -> void`. We can call this function using `f()`, but what does it mean to call a function? We always invoke a function for an argument, so `void` actually corresponds to the set with a single element! Note that C functions that return void either do nothing useful (i.e. discard their arguments), or are impure. Indeed, even using a pointer argument to return a value indirectly modifies a 'global state'!  In Haskell, the type corresponding to the *singleton set* (and its single value) is denoted with `()`. Meaning that if we have a function:
```haskell
f :: () -> Int
```
we can invoke it as `f()`! Instead, the type `Void` corresponds to the empty set, and there can never be a value of this type. There is even a (unique) polymorphic (in the return type!) function that takes `Void` added to the prelude:
```haskell
absurd :: Void -> a
```
You may be tempted to discard the type `Void` as something that is only used by academics to make the type system 'complete', but there are a number of legitimate uses for `Void`. An example is *Continuation passing style*, or CPS, where functions do not return a value, but pass control over to another function:
```haskell
type Continuation a = a -> Void
```
In other words, a continuation is a function that *never returns*, which can be used to manipulate control flows (in a type-safe manner).

Recall that an initial object has exactly one arrow to each other object, and a terminal object has exactly one arrow coming from each other object. These objects are unique up to isomorphism. In the category of types, they correspond to `Void` and `()` respectively. 

To summarize this introduction, in the category of 'computer programs', types are objects, and *pure* functions between these types are arrows. Next, we consider how we can apply some of the concepts we have seen, such as functors and natural transformations, to this category.

## Containers as functors

When we consider functors in the category of types, the first question is 'to what category?'. Here, we will almost exclusively talk about functors from **Hask** to itself, i.e. _endofunctors_.

Endofunctors in **Hask** map types to types, and functions to functions. There are many examples of functors in programming. Let us first consider the concept of _lists of objects_, i.e. arrays or vectors. In C++ a list would be written as:
```cpp
std::vector<T> xs;
```
or in Python we would have;
```python
>>> import numpy as np
>>> a = np.array([1,2,3], dtype='int')
>>> type(a)
<class 'numpy.ndarray'>
>>> a.dtype
dtype('int64')
```
Note here that the true type of the numpy array is hidden inside the object, meaning its the responsiblity of the program to make sure that the types of operations match! The reason that we consider `numpy` arrays is that normal 'lists' in Python are actually _tuples_, which we will discuss when we talk about products and coproducts.

Let us consider the mathematical way of expressing this:

\begin{example}
Lists of some type are more generally called \textbf{words over some alphabet} (i.e. a set) $X$, and we denote the set of all finite words of elements in $X$ as $X^*$. Elements in $X^*$ look like:
$$(x_1, x_2, x_3)$$
$$(x_1)$$
$$()$$
These are all examples of \emph{words} in $X$ (where the last example corresponds to the empty word). If we want to construct a \emph{word functor} $T$, then $T$ would then have the signature:
\begin{align*}
T&: X \to X^*\\
 &: (f: X \to Y) \mapsto (Tf: X^* \to Y^*)
\end{align*}
For this second option, we have an obvious candidate for the precise function, let $f: X \to Y$ be some map, then $Tf$ maps a word in $X$ gets to a word in $Y$ in the following way:
$$Tf(x_1, x_2, x_3, ... x_n) = (f(x_1), f(x_2), f(x_3), \ldots, f(x_n)).$$
\end{example}

**Type classes en type constructors**

We will express this idea in Haskell, but before we can do this we first have to consider type classes and -constructors. A _type constructor_ is a 'function' (on types, not an arrow) that creates a type out of a type. A _type constructor_ can have multiple _value constructors_, and these constructors can be differentiated between using something called _pattern matching_ which we will see later. As an example, consider `Bool`.
```haskell
data Bool = True | False
```
Here, we define the type constructor `Bool` as the resulting type corresponding to the _value_ given by the value constructors `True` and `False`, which both are nullary constructors (that take no argument as types!). Normally however, type constructors take one or multiple types for their value constructors:
```haskell
data Either = Left a | Right b
```
Here, the type constructor either hold either a value of type `a` or of type `b`, corresponding to the value constructors `Left` and `Right`. We will revisit this idea (and `Either`) when talk about products and coproducts.

A type class is a _common interface for types_. It defines a family of types that support the same operations. For example, a type class for objects that support equality is defined as:
```haskell
class Eq a where
    (==) :: a -> a -> Bool
```
If we want to express the concept^[In C++, type constructors are referred to as _concepts_, and they have been a long time coming (but are not yet in the standard)] _functor_ using a typeclass, we have to state that it can send types to types, and that it sends functions between two types to functions with the appropriate signature, i.e.:
```haskell
class Functor F where
    fmap :: (a -> b) -> F a -> F b
```
This says that `F` is a functor, if there is a function `fmap` that takes a function `f :: a -> b` and maps it to a function `fmap f :: F a -> F b`. Note that we dont explicitely have to state that `F` sends types to types, because this can be induced from the fact that we use `F a` where the compiler expects a type.

\subsection*{The List functor}

The _list functor_ in Haskell is denoted with `[]`, and a list of type `a` is denoted `[a]` (which is syntactic sugar, normally the type would be `[] a`).

Let us try to define this functor from the ground up. If we would write `List` instead of `[]`, then first we have to define what a list is. We can define this as follows:
```haskell
data List a = Nil | Cons a (List a)
```
Here the type constructor has two possible ways of constructing (partitioning the possible values of the type):  a list of `a`s is either empty (corresponding to the _constructor_ `Nil`), or that it is the concatenation (corresponding to the _constructor_ `Cons`) of an object of type `a` with another list of `a`s. Note that this is a recursive definition!

Next we define the `fmap` corresponding to our `List` functor (i.e. how it maps functions). The corresponding definition to the map described for the _word functor_ is:
```haskell
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x t) = Cons (f x) (fmap f t)
```
If a list is empty, then we get te empty set, otherwise we map the indivual values in the list recursively using the given `f`. In `C++` this `fmap` functor roughly corresponds to `std::transform`, while for Python the closest thing would be the `map` function. With these two definitions, `List` is a functor! We could check the that it satisfies the requirements.

As mentioned, `List` is implemented in the standard library as `[]`, and `Cons` is written as `:`, while the empty set is written also as `[]`. This allows you to write:
```haskell
x = 1 : 2 : [] -- this results in `[1, 2] :: [Int]`!
```

\subsection*{The Maybe functor}

As a simpler example, consider a type that either has no value or it has a value corresponding to some type `a`. In Haskell, this is called `Maybe`, while in C++ this is called `std::optional`, in Python the same idea could be achieved using:
```python
def fn(a):
    if (a >= 0)
        return sqrt(a)
    return None
```
This function returns `None` (corresponding to 'no value') if we provide 'invalid input'. This functor can be defined as:
```haskell
data Maybe = Nothing | Just a
```
And to turn it into a functor, we define `fmap`:
```haskell
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
```

## Polymorphic functions as natural transformations

Now that we view type constructors as functors, we can consider natural transformations between type constructors. If we let `a` be a type, then a natural transformation `alpha` would be something that maps between `F a` and `G a`, where `F` and `G` are type constructors:
```haskell
alpha :: F a -> G a
```
Note that implicitely we talk about the component of `alpha` at `a`, since this function is _polymorphic_ the right component gets picked by the compiler. For example, say we have a list `[a]`, and we want to obtain the first element of this list. If the list is empty, then there is no such element, otherwise we obtain an `a`; i.e. the result is a `Maybe a`:
```haskell
head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = x
```
Here, we have a natural transformation between the `List` and the `Maybe` functor!

\subsection*{Parametric polymorphism and ad-hoc polymorphism}

In C++, a template does not have to be defined for all types, i.e. we can write:
```cpp
template <typename T>
T f(T a);

template <>
int f(int a) { return 2 * a; }

template <>
float f(float a) { return 2.0f * a; }
```
Here, e.g. `f<int>(1)` would yield `2`, while `f<char>('a')` would result in a compilation error.

In Haskell, this is not allowed, polymorphic functions must work for _all types_, this is called parametric polymorphism. Specializing function definitions is done using type classes^[in C++ this would be done using overloading and (partial) template specialization]. This has an important consequence (or perhaps, it is the underlying reason): a parametric polymorphic function satisfies automatically the naturality conditions.

The corresponding diagram is:
\begin{figure}[h]
\centering
\begin{tikzcd}
\texttt{F a} \arrow[r, "\texttt{alpha}"] \arrow[d, "\texttt{fmap f :: F a -> F b}"'] & \texttt{G a} \arrow[d, "\texttt{fmap f :: G a -> G b}"]\\
\texttt{F b} \arrow[r, "\texttt{alpha}"'] & \texttt{G b}
\end{tikzcd}
\end{figure}

Here the left `fmap` works for `F`, while the right `fmap` corresponds to `G`, and the top `alpha` is implicitely the component at `a`, while the bottom one is the component at `b`. What would have to show, is that automatically

\texttt{fmap f . alpha = alpha . fmap f}

This can be shown in a very general context, and it has to do with the fact that the 'bodies'for `f`, `fmap` and `alpha` are the same for all types. We will show this in an upcoming part when we discuss _free theorems_.

Let us revisit our `head :: [a] -> Maybe a` example, and consider the naturality condition here. It says that:

```haskell
fmap f . head = head . fmap f
```
Here, the fmap on the lhs corresonds to the `Maybe` functor, while on the rhs it corresponds to the `[]` functor. The lhs can b e read like this; take the first element of the list, then apply f on it. The rhs can be read as "apply the function f to the enitre list, then take the first element". The result is the same; the funtion f applied to the head of the list (if any). But for the rhs we apply the function `f` for each element in the list, while on the lhs we only apply it to the head. Because of the constraint on polymorphic function, the compiler knows that the result is equal and can choose which one to use!

**References:**

- 1.2, 1.7 of the 'Category Theory for Programmers' blog by Bartosz Milewski

# Products, Co-products and Algebraic Datatypes

Duality, the 'opposite category'. Initial and terminal objects as 'co-concepts'.

Universal construction

Product and co-product

Product and co-products applied to types

Bifunctorality and `Either`

**References:**

- 3.1, 3.3 (partially) and 3.4 (partially) of Mac Lane
- 1.5, 1.6 and 1.8 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- Chapter 5 of Barr and Wells
- Catsters products and co-products

# Monads

**References:**

- 6.1 and parts of 6.3 and 6.4 of Mac Lane
- Blogs..
- Catsters

# Function types (and Curry-Howard Isomorphism?)

**References:**

- 1.9 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 6.1 of Barr and Wells

# Reader and writer monads, Kleisli categories

# Yonedda's Lemma

# Limits and co-limits

# 'Theorems for free!'

# 'Fast and loose reasoning is morally correct'

# Lenses; Adjunctions and profunctors

# F-algebras:

Recursion, co-algebras and streams

# Denotational semantics

# Homotopy type theory

# Quantum computations?

(Bert Jacobs)

# Ideas

Section on 'modularity':

> *Bartosz Milewski*: "... Elegant code creates chunks that are just the right size and come in just the right number for our mental digestive system to assimilate them. So what are the right chunks for the composition of programs? Their surface area has to increase slower than their volume ... The surface area is the information we need in order to compose chunks. The volume is the information we need in order to implement them ...  Category theory is extreme in the sense that it actively discourages us from looking inside the objects ... The moment you have to dig into the implementation of the object in order to understand how to compose it with other objects, you’ve lost the advantages of your programming paradigm."

# Literature

## Blogs
1. *Bartosz Milewski*: "Category Theory for Programmers", a blog post series that gives an excellent overview of interesting topics. <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>

## Papers
1. About **Hask**: <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf>
2. Free theorems: <http://ttic.uchicago.edu/~dreyer/course/papers/wadler.pdf> (also Reynold: <http://www.cse.chalmers.se/edu/year/2010/course/DAT140_Types/Reynolds_typesabpara.pdf>).
3. Recursion as initial objects in F-algebra: <http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt>

## Books
1. Conceptual Mathematics: A first introduction to categories.
2. Maclane, Category Theory for the working mathematician
3. Barr and Wells, Category Theory for Computer Scientists
