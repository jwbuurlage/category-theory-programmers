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
    - \usepackage{float}
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
    - \newtheorem{proposition}{Proposition}
    - \newtheorem{example}{Example}
    - \numberwithin{theorem}{chapter}
    - \numberwithin{definition}{chapter}
    - \numberwithin{example}{chapter}
    - \numberwithin{proposition}{chapter}
    - \usepackage{geometry}
    - \geometry{margin=4cm}
---

This document contains notes for a small-scale seminar on category theory in the context of (functional) programming, organized at CWI. The goal of the seminar is to gain familiarity with concepts of category theory that apply (in a broad sense) to the field of functional programming. It could be an idea to have an associated (toy) project that examplifies the concepts that are discussed.

Although the main focus will be on the mathematics, examples should be made in Haskell to illustrate how to apply the concepts, and possibly examples in other languages as well (such as Python and C++).

# Categories

## Core definitions

We start with giving the definition of a category:

\begin{definition}
A \textbf{category} $\mathcal{C} = (O, A, \circ)$ consists of:
\begin{itemize}
\item a collection $O$ of \emph{objects}, written $a,b,\ldots \in O$.
\item a collection $A$ of \emph{arrows} written $f,g,\ldots \in A$ between these objects, e.g. $f: a \to b$.
\item a notion of \emph{composition} $f \circ g$ of arrows.
\item an identity arrow $\text{id}_a$ for each object $a \in O$.
\end{itemize}
The composition operation and identity arrow should satisfy the following laws:

\begin{itemize}
\item \emph{Composition}: If $f: a \to b$ and $g: b \to c$ then $g \circ f: a \to c$.

\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[r, "f"] \arrow[rr, "g \circ f", bend right=50] & b \arrow[r, "g"]  & c
\end{tikzcd}
\end{figure}

\item \emph{Composition with identity arrows}:  If $f: x \to a$ and $g: a \to x$ where $x$ is arbitrary, then:
$$ \text{id}_a \circ f = f,~g \circ \text{id}_a = g.$$


\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, "g"', bend right=20] & x \arrow[l, "f"', bend right=20]
\end{tikzcd}
\end{figure}

\item \emph{Associativity}: If $f: a \to b$, $g: b \to c$ and $h: c \to d$ then:
$$(h \circ g) \circ f = h \circ (g \circ f).$$
This is the same as saying that the following diagram commutes:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=huge]
a \arrow[r, "f"] \arrow[d, "h \circ g \circ f"'] \arrow[rd, crossing over, near start, "g \circ f"] & b  \arrow[d, "g"] \arrow[ld, crossing over, near end, "h \circ g"] \\
d & c \arrow[l, "h"]
\end{tikzcd}
\end{figure}
Saying a diagram commutes means that for all pairs of vertices $a'$ and $b'$ all paths from between them are equivalent (i.e. correspond to the same arrow of the category).
\end{itemize}

\end{definition}

If $f: a \to b$, then we say that $a$ is the *domain* and $b$ is the *codomain* of $b$. It is also written as:
$$\text{dom}(f) = a,~\text{cod}(f) = b.$$
The composition $g \circ f$ is only defined on arrows $f$ and $g$ if the domain of $g$ is equal to the codomain of $f$.

We will write for objects and arrows respectively simply $a \in \mathcal{C}$ and $f \in \mathcal{C}$, instead of $a \in O$ and $f \in A$.

\subsection*{Examples of categories}

Some examples of familiar categories:

| Name     | Objects            | Arrows                 |
|----------|--------------------|------------------------|
| **Set**  | sets               | maps                   |
| **Top**  | topological spaces | continuous functions   |
| **Vect** | vector spaces      | linear transformations |
| **Grp**  | groups             | group homomorphisms    |

In all these cases, arrows correspond to functions, although this is by no means required. All these categories correspond to objects from mathematics, along with *structure preserving maps*. **Set** will also play a role when we discuss the category **Hask** when we start talking about concrete applications to Haskell.

There are also a number of simple examples of categories:

- **0**, the empty category $O = A \equiv \emptyset$.
- **1**, the category with a single element and (identity) arrow:

\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"]
\end{tikzcd}
\end{figure}

- **2**, the category with a two elements and a single arrow between these elements

\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, "f"] & b \arrow[loop right, "\text{id}_b"]
\end{tikzcd}
\end{figure}

- $\rightrightarrows$: the category with two elements and two parallel arrows between these elements:

\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, shift left] \arrow[r, shift right]  & b \arrow[loop right, "\text{id}_b"]
\end{tikzcd}
\end{figure}

From now on we will sometimes omit the identity arrows when drawing categories.

- Another example of a category is a \emph{monoid category}, which is a specific kind of category with a single object.
    \begin{definition}
    A \emph{monoid} $(M, \cdot, e)$ consists of:
    \begin{itemize}
    \item a set $M$
    \item an associative binary operation $(\cdot): M \times M \to M$
    \item a unit element w.r.t $(\cdot)$, i.e. $\forall_m~e \cdot m = m$
    \end{itemize}
    Indeed, it is a group structure without requirement of inverse elements. It is also called a \emph{semi-group with unit})
    \end{definition}
    This corresponds to a category $\mathcal{C}(M)$ where:
    - There is a single object (for which we simply write $M$)
    - There are arrows $m: M \to M$ for each element $m \in M$.
    - Composition is given by the binary operation of the monoid: $m_1 \circ m_2 \equiv m_1 \cdot m_2$.
    - The identity arrow $\text{id}_M$ is equal to $e$, the unit of the monoid.


- We can also consider natural numbers $\mathbb{N}_{> 0}$, with arrows going from each number to its multiples.

\begin{figure}[H]
\centering
\begin{tikzcd}
1 \arrow[r, bend right=0] \arrow[rr, bend right=10] \arrow[rrr, bend right=20] \arrow[rrrr, bend right=30] \arrow[rrrrr, bend right=40] & 2 \arrow[rr, bend left=20, "\times 2"] \arrow[rrrr, bend left=30, "\times 3"] & 3 \arrow[rrr, bend left=20, "\times 2"] & 4 & 5 & 6 \ldots
\end{tikzcd}
\end{figure}

- A partially ordered set (poset): a binary relation $\leq$ over a set $S$ s.t. for $a,b,c \in S$:

    - $a \leq a$
    - $a \leq b,~b \leq a \implies a = b$
    - $a \leq b,~b \leq c \implies a \leq c$

    also corresponds to a category.

## Functors

A functor is a map between categories. This means it sends objects to objects, and arrows to arrows.

**Definition**: A *functor* $T$ between categories $\mathcal{C}$ and $\mathcal{D}$ consists of two functions (both denoted simply by $T$):

- An *object function* that maps objects $a \in \mathcal{C}$: $a \mapsto Ta \in \mathcal{D}$
- An *arrow function* that assigns to each arrow $f: a \to b$ in $\mathcal{C}$ an arrow $Tf: Ta \to Tb$ in $\mathcal{D}$, such that:
$$T(\text{id}_a) = \text{id}_{Ta},~T(g \circ f) = Tg \circ Tf.$$

A functor is a very powerful concept, since it allows you to translate between different branches of mathematics! They also play an important role in functional programming. Where among many other things, they are useful for defining the _container types_ or more generally _type constructors_.

Functors can be composed, and this allows one to define a category of categories\footnote{Actually, there are some technicalities to be worked out and the resulting category consists of 'small categories' only.} **Cat**, where the arrows are functors.

\subsection*{Examples of functors}

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

* The \emph{power-set functor}: $\mathcal{P}:$ **Set** $\to$ **Set** sends subsets to their image under maps. Let $A, B \in$ **Set**, $f: A \to B$ and $S \subset A$:
\begin{align*}
\mathcal{P}A &= \mathcal{P}(A),\\
\mathcal{P}f&: \mathcal{P}(A) \to \mathcal{P}(B),~S \mapsto f(S)
\end{align*}

* From many categories representing 'sets with added structure' (groups, vector spaces, rings, topological spaces, ...) there is a *forgetful functor* going to **Set**, where objects are sent to their underlying sets.

    There is also a forgetful functor $F: \mathbf{Cat} \to \mathbf{Graph}$, sending each category to the graph defined by its objects and arrows.

* Dual-set functor
\begin{align*}
*&: \textbf{Vect} \to \textbf{Vect}\\
&: W \mapsto W^*\\
&: (f: V \to W) \mapsto (f^*: W^* \to V^*)
\end{align*}
This is an example of a \emph{contravariant functor} (a functor from \textbf{Vect} to $\textbf{Vect}^{\text{op}}$, the category with reversed arrows and composition rules.

## Special objects, arrows and functors

\subsection*{Special objects}

For objects, we distuinguish two special kinds:

\begin{definition}
An object $x \in \mathcal{C}$ is \textbf{terminal} if for all $a \in \mathcal{C}$ there is exactly one arrow $a \to x$. Similarly, it is \textbf{initial} if there is exactly one arrow $x \to a$ to all objects.
\end{definition}


\begin{figure}[H]
\centering
\begin{tikzcd}
  &a \arrow[dr]&  \\
i\arrow[ur] \arrow[rr, shift left] \arrow[dr] & & t \arrow[ll, shift left]\\
  &b \arrow[ur] &
\end{tikzcd}
\end{figure}
Here, $i$ is initial, and $t$ is terminal.

\subsection*{Special arrows}

There are a number of special kind of arrows:

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is a \textbf{monomorphism} (or simply mono), if for all objects $x$ and all arrows $g, h: x \to a$ and $g \neq h$ we have:
$$f \circ g \neq f \circ h.$$
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

In set, **epi** and **mono** imply **iso**. This however does not hold for general categories!

\subsection*{Special functors}

Lastly, we turn our attention to special kinds of functors. For this we first introduce the notion of a _hom-set_ of $a$ and $b$, the set\footnote{Here we assume that this collection is a set, or that the category is so-called \emph{locally small}} of all arrows from $a$ to $b$:
$$\text{Hom}_\mathcal{C}(a, b) = \{ f \in \mathcal{C}~|~f: a \to b \}.$$

\begin{definition}
A functor $F: \mathcal{C} \to \mathcal{D}$ is \textbf{full} if for all pairs $a, b \in \mathcal{C}$ the induced function:
\begin{align*}
F:~\text{Hom}_\mathcal{C}(a, b) &\to \text{Hom}_\mathcal{D}(Fa, Fb),\\
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

\begin{figure}[H]
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

\begin{figure}[H]
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

There is one issue we have glared over; in mathematics all functions are *pure*: they will always give the same output for the same input. This is not always the case for computer programs, using IO functions, returning the current date, using a global variable are all examples of impure operations that are common in programming. In Haskell, *all functions are pure*, and this is a requirement that allows us to make the mapping to the category **Set**. The mechanism that allows Haskell programs to still do useful things is powered by *monads*, which we will discuss later.

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

**Type classes and type constructors**

We will express this idea in Haskell, but before we can do this we first have to consider type classes and -constructors. A _type constructor_ is a 'function' (on types, not an arrow) that creates a type out of a type. A _type constructor_ can have multiple _value constructors_, and these constructors can be differentiated between using something called _pattern matching_ which we will see later. As an example, consider `Bool`.
```haskell
data Bool = True | False
```
Here, we define the type constructor `Bool` as the resulting type corresponding to the _value_ given by the value constructors `True` and `False`, which both are nullary constructors (that take no argument as types!). Normally however, type constructors take one or multiple types for their value constructors:
```haskell
data Either a b = Left a | Right b
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
If a list is empty, then we get the empty set, otherwise we map the indivual values in the list recursively using the given `f`. In `C++` this `fmap` functor roughly corresponds to `std::transform`, while for Python the closest thing would be the `map` function. With these two definitions, `List` is a functor! We could check the that it satisfies the requirements.

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
\begin{figure}[H]
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

## Duality and products of objects

\subsection*{Duality}

For any category, we can defined the category with all arrows (and composition) reversed.

\begin{definition}
The \emph{opposite category} $\mathcal{C}^{\text{op}}$ of a category $\mathcal{C}$ is the category with:

\begin{itemize}
\item The same objects as $\mathcal{C}$.
\item For all arrows $f: a \to b$ in $\mathcal{C}$, there is an arrow $f^{\text{op}}: b \to a$
\item The composition of $f^{\text{op}} : a \to b$ and $g^{\text{op}}: b \to c$ is given by:
$$g^{\text{op}} \circ f^{\text{op}} = (f \circ g)^{\text{op}}$$
\end{itemize}
\end{definition}

The opposite category is very useful, because many concepts defined in the original category have 'dual notions' in the opposite category. Clearly, for example, an *initial object in $\mathcal{C}$ is a terminal object in $\mathcal{C}^{\text{op}}$*. Similarly, an arrow that is *mono in $\mathcal{C}$ is epi in $\mathcal{C}^{\text{op}}$*. This is called **duality**, and provides so-called 'co-' notions of constructs, as well as 'co-' versions of theorems.

Whenever defining something it always make sense to see what this means in the opposite category, giving you a lot of free information. For example, we showed that faithful functors reflects mono's. Looking at the dual category, we immediately have that it also reflects epi's!

\subsection*{Products}

Initial objects and terminal objects have a so-called *universal property*, they are the object so that for all other objects there is a *unique morphism to the objects*. A more involved example of such a universal property is the *notion of a product of objects*. The categorical product is a unifying definition for many 'products' encountered in mathematics, such as the cartesian product, product group, products of topological spaces, and so on.

\begin{definition}
Let $\mathcal{C}$ be a category, and let $a, b \in \mathcal{C}$ be objects in $\mathcal{C}$. A \emph{product} of $a$ and $b$ is an object $a \times b \in \mathcal{C}$ along with two arrows $p_1: a \times b \to a$ and $p_2: a \times b \to b$ (the \emph{projections}) so that for all objects $c \in \mathcal{C}$ and arrows $f: c \to a$ and $g: c \to b$ there exists a unique morphism $q: c \to a \times b$ that makes the following diagram commute:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& c \arrow[ddl, "f"'] \arrow[ddr, "g"] \arrow[d, "q"] & \\
& a \times b \arrow[dl, "p_1"] \arrow[dr, "p_2"'] & \\
a & & b
\end{tikzcd}
\end{figure}
\end{definition}

In this case, the (unique) arrows $q$ are what gives the product a *universal mapping property*. If a product exists, it is unique op to unique isomorphism.

We say that the functions $f$ and $g$ *factors* through $a \times b$, or that $a \times b$ *factorizes* $f$ and $g$. The reason for this name is clear when making the analogy with numbers. Consider:
$$f = p_1 \circ q,~g = p_2 \circ q.$$
For an example with numbers:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& 2 \arrow[dl, "\times 4"'] \arrow[dr, "\times 8"] \arrow[d, "\times 2"] & \\
8 & 4 \arrow[l, "\times 2"] \arrow[r, "\times 4"'] & 16
\end{tikzcd}
\end{figure}
$$4 = 2 \times 2,~8 = 4 \times 2.$$
This seems to indicate that in 'some category related to numbers' (in fact, precisely the category of natural numbers with their multiples, that we gave as an example in the first chapter), the product would correspond to the gcd!


\begin{example}
Let us consider the product of objects in \textbf{Set}. Consider two sets $A, B$. We have a clear candidate for a product; the cartesian product $A \times B$. Given any element (or \emph{pair}) $(a, b) \in A \times B$, the projections $p_1, p_2$ send it to $a$ and $b$ respectively. Is this indeed a product?

Let $V$ be any other set, with arrows (functions) $f$ to $A$ and $g$ to $B$. Can we construct a (unique) arrow $q$ to $A \times B$?

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& V \arrow[ddl, "f"'] \arrow[ddr, "g"] \arrow[d, "q"] & \\
& A \times B \arrow[dl, "p_1"] \arrow[dr, "p_2"'] & \\
A & & B
\end{tikzcd}
\end{figure}

Consider any element $v \in V$. It gets mapped to $f(v) \in A$, and $g(v) \in B$. Let $q: v \mapsto (f(v), g(v))$, then $(p_1 \circ f)(v) = f(v)$, and thus $p_1 \circ f = f$. Similarly $p_2 \circ g = g$.

Indeed, we have constructed an arrow that makes the above diagram commute. It is also clear that this is the \emph{only arrow} that satisfies this, so we conclude that $A \times B$ is the product of $A$ and $B$ in the category \textbf{Set}. Another example of a product of sets would be $B \times A$, which is cannonically isomorphic to $A \times B$ (the isomorphism corresponds to 'swapping' the elements, which is its own inverse).
\end{example}

For a completely different example, we consider the category corresponding to a poset.

\begin{example}
Let us consider the product of objects in the category corresponding to some poset $P$. Consider two elements $x, y \in P$. A product $z \equiv x \times y$ would be equiped with two arrows $z \to x$ and $z \to y$, which means $z \leq x$ and $z \leq y$. Furthermore, for any element $w$ with arrows to $x, y$ (i.e. $w \leq x$ and $w \leq y$), there has to be an arrow $q: w \to z$ (i.e. $w \leq z$). This is the same as saying that, in addition to $z \leq x$ and $z \leq y$, we have for all elements $w$ of the poset:
$$w \leq x \text{ and } w \leq y \implies w \leq z$$
This means that $z$ is the "largest element that is smaller or equal to $x$ and $y$", also called the \emph{infimum} of $x$ and $y$.
\end{example}

\subsection*{Coproducts}

Let us revisit the idea of *duality*. What would be the dual-notion of the product? Let us take the product diagram, and reverse the arrows:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& c  & \\
& a + b\arrow[u, "q"] & \\
a \arrow[uur, "f"] \arrow[ur, "p_1"'] & & b \arrow[uul, "g"']\arrow[ul, "p_2"]
\end{tikzcd}
\end{figure}

This already very suggestives, we have arrows going from objects $a, b$ into the coproduct (written 'a+b', we will see why soon), and from this coproduct arrows going to arbitrary target objects $c$. The arrows $a \to a + b$ and $b \to a + b$ already look kind of like an inclusion. Let us see what happens when we apply duality to the product definition, and change some names.

\begin{definition}
Let $\mathcal{C}$ be a category, and let $a, b \in \mathcal{C}$ be objects in $\mathcal{C}$. A \emph{coproduct} of $a$ and $b$ is an object $a + b \in \mathcal{C}$ along with two arrows $i_1: a + b \leftarrow a$ and $i_2: a + b \leftarrow b$ (the \emph{inclusions}) so that for all objects $c \in \mathcal{C}$ and arrows $f: c \leftarrow a$ and $g: c \leftarrow b$ there exists a unique morphism $q: c \leftarrow a + b$ that makes the following diagram commute:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& c  & \\
& a + b\arrow[u, "q"] & \\
a \arrow[uur, "f"] \arrow[ur, "i_1"'] & & b \arrow[uul, "g"']\arrow[ul, "i_2"]
\end{tikzcd}
\end{figure}
\end{definition}
Note that this is precisely the definition of the product, with all arrows reversed and the projections renamed to $i_1$ and $i_2$.

Because of the properties that we will show discover, the coproduct is also called the *sum*. Note that this dual notion is fundamentally different. Let us see what it means for the category **Set**:

\begin{example}
Consider two sets $A, B$. When looking at the diagram for the coproduct, we see that we need to find some kind of set in which elements of $A$ and $B$ are represented but completely independent; since $c$ is now the target of the functions we want to factor through $a + b$.

This describes the \emph{union} of $A$ and $B$, but only if the two are disjoint since in the intersection of $A$ and $B$ we would not know whether $q$ should represent $f$ or $g$. This is easily solved by looking at the disjoint union, which has a nice representation:
$$A + B \equiv \{ (a, 0)~|~a \in A \} \cup \{ (b, 1)~|~b \in B \}.$$

It is clear what $i_1$ and $i_2$ are. Let $V$ be any other set, with arrows (functions) $f: A \to V$ and $g: B \to V$.

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
& V  & \\
& A + B\arrow[u, "q"] & \\
A \arrow[uur, "f"] \arrow[ur, "i_1"'] & & B \arrow[uul, "g"']\arrow[ul, "i_2"]
\end{tikzcd}
\end{figure}

Consider any element $a \in A$. It gets mapped to $f(a) \in V$, and to $i_1(a) = (a, 0)$ in $A + B$. Then we should set $q(a, 0) \equiv f(a)$, and similarly we should set $q(b, 1) \equiv g(b)$. This already defines $q$ uniquely and completely, so we conclude that the disjunt union is indeed the coproduct in the category \textbf{Set}.
\end{example}

We note there that the coproduct (and product) of two objects, generalizes also to products of more than 2 objects (by simply adding more maps $i_1, i_2, i_3 \ldots$).

## Algebraic data types

Let us apply the product (and coproduct) concepts to the category of types. Since we already saw what these constructs mean for sets, namely the cartesian product and the disjoint union respectively, it should be clear what this means for types.

Given a type $a$ and a type $b$, the product corresponds to a *pair*, written `(a, b)` in Haskell. We could implement this ourselves using simply:
```haskell
data Pair a b = Pair a b
```
Here, we give the unique value constructor the same name as its type constructor. In C this would correspond roughly to a `struct` (more specifically a POD data type), although a `Record` in Haskell corresponds more precisely to a `struct`. Note for this to make sense, the product type should (and is) be defined for  more than 2 elements.

In C++ this is known as a `std::pair` (or a `std::tuple` for n-ary products). However, its implementation (and also usage) is awkward and convoluted. Functional programming (and product/coproduct types) is not yet a first-class citizen in C++.

The coproduct (or _sum type_ corresponds to a value that has either type $a$, or type $b$. This is implemented as the `Either` datatype:
```haskell
data Either a b = Left a | Right b
```
Here, the two value constructors take an element of type `a`, or an element of type `b` respectively. In C and C++ this would correspond roughly to a union\footnote{In C++17 there will be a standardized 'tagged union' `std::variant` that more accurately models the coproduct}, except that it is *tagged*.

A sum type means choosing an alternative between types, while the product type is a combination of the types. Let us look at some more examples:

- In C, an `enum` represents a fixed number of alternative constants. In Haskell, this would correspond to the sum type of multiple 0-ary value constructors (implicitely the finite sum type of the type `()` with itself):
```haskell
    data Enum = One | Two | Three
```

- A node of a binary tree of type `a` has a sum type: it is either `()` (representing a leaf), or it is the product type of:
    - Tree on the left
    - `a` for the value of the node
    - Tree on the right

    or in Haskell:
```haskell
data Tree a = Leaf | Node (Tree a) a (Tree a)
```

- Using the product and sum types, we can turn the type system into a semi-ring, where we define:
    - $0$ = `Void`
    - $1$ = `()`
    - $a + b$ = `Either a b = Left a | Right b`
    - $a \times b$ = `(a, b)`

    Let us check that $0$ really works as $0$. What happens when we add `Void` to a type:
```haskell
Either a Void = Left a | Right Void
```
    We can never get a value for void, so the only thing we can do is to construct `Either a Void` with a value of type `a`, which means:
    $$a + 0 = a.$$
    Similarly, if we have a product with `Void`, we can never instantiate a pair (because there is no value for `Void`), so the corresponding product type is again `Void`:
    $$a \times 0 = 0.$$
    Although this is all a bit of a stretch, this analogy has some interesting properties, and we can do some real algebra with our types and try to interpret the results. Consider again the list type:
```haskell
List a = Empty | Cons a (List a)
```
    In our 'semi-ring', writing $x$ for `List a`, this would look like the expression:
    $$x = 1 + a \times x$$
    This is unsolvable, but we can try to iteratively substitute $x$ into the right hand side:
    \begin{align*}
        x &= 1 + a \times (1 + a x)\\
          &= 1 + a + a^2 x\\
          &= 1 + a + a^2 (1 + a x)\\
          &= 1 + a + a^2 + a^3 (1 + ax) \\
          &= \ldots
     \end{align*}
     Which can be read as 'a list is either empty, or it has one element of type `a`, or it has two elements of type `a`, etc.
     Although this is mostly an entertaining (and, depending on your view, an overly complicated) way of looking at types, a similar correspondence from types to logical operations forms the basis of the Curry-Howard isomorphismthat connects type theory to logic in a very fundamental way.

## Bi-functors

\begin{definition}
Given two categories $\mathcal{C}, \mathcal{D}$ their product category $\mathcal{C} \times \mathcal{D}$ is given by:
\begin{itemize}
\item The objects are pairs $(c, d)$ where $c \in \mathcal{C}$ and $d \in \mathcal{D}$.
\item The arrows are pairs of arrows, $(f, g): (c, d) \to (c', d')$ for $f: c \to c'$ in $\mathcal{C}$ and $g: d \to d'$ in $\mathcal{D}$.
\item The identity arrow for $(c, d)$ is the pair $(\text{id}_c, \text{id}_d)$.
\item Composition of arrows happens per component, i.e.\ when $f, g$ in $\mathcal{C}$ and $h, k \in \mathcal{D}$:
$$(f, h) \circ (g, k) \equiv (f \circ g, h \circ k)$$
\end{itemize}
\end{definition}

Note that alternatively we could define this as the product of objects in the category \textbf{Cat}.

This brings us to the concept of a bifunctor, which can be seen as a 'functor of two arguments'.

\begin{definition}
Let $\mathcal{C}, \mathcal{D}, \mathcal{E}$ be categories. A bifunctor is a functor:
$$F: \mathcal{C} \times \mathcal{D} \to \mathcal{E}.$$
\end{definition}

When is something a bifunctor. Given any candidate functor $\mathcal{C} \times \mathcal{D} \to \mathcal{E}$, it is enough to show that fixing one argument (an object, setting the mapped arrow to the identity arrow at that object) result in a functor from one of the components to $\mathcal{E}$. This is summarized in the following proposition:

\begin{proposition}
\label{prop:bifunctorcomponents}
Let $F$ be a function $\mathcal{C} \times \mathcal{D} \to \mathcal{E}$. $F$ is a bifunctor if and only if the functions:
\begin{align*}
F(c, -)&: \mathcal{D} \to \mathcal{E} \\
       &: d \mapsto F(c, d)\\
       &: (f: d \to d') \mapsto F(\text{id}_c, f) \\
F(-, d)&: \mathcal{C} \to \mathcal{E} \\
       &: c \mapsto F(c, d)\\
       &: (f: c \to c') \mapsto F(f, \text{id}_d)
\end{align*}
are functors.
\end{proposition}

\begin{proof}
todo
\end{proof}

In Haskell the bifunctor is implemented as a type class, which is implemented in the standard library as follows:

```haskell
class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
    bimap g h = first g . second h
    first :: (a -> c) -> f a b -> f c b
    first g = bimap g id
    second :: (b -> d) -> f a b -> f a d
    second = bimap id
```

Here you see a circular definition. This means it is enough to *either* provide the `bimap`, or the `first` and `second` functions, powered by Proposition \ref{prop:bifunctorcomponents}.

\begin{example}
Whenever you have a category $\mathcal{C}$ where the product of two objects exists for all pairs of objects, then this gives rise to a bifunctor:
\begin{align*}
\times&: \mathcal{C} \times \mathcal{C} \to \mathcal{C}\\
      &: (a, b) \mapsto a \times b\\
      &: (f: a \to a', g: b \to b') \mapsto (f \times g: a \times b \to a' \times b')
\end{align*}
where we find $f \times g$ by looking at the diagram:

\begin{figure}[h!]
\centering
\begin{tikzcd}[sep=large]
a \arrow[d, "f"'] & a \times b \arrow[d, dashed, "f \times g"] \arrow[l, "p_1"'] \arrow[r, "p_2"] & b \arrow[d, "g"] \\
a' & a' \times b' \arrow[l, "p'_1"] \arrow[r, "p'_2"'] & b'
\end{tikzcd}
\end{figure}

By definition of the product $a' \times b'$, we have that for any object $c$ that has arrows to $a'$ and $b'$, there should be a \emph{unique} arrow $c \to a' \times b'$. Note that $f \circ p_1$ and $g \circ p_2$ are arrows from $a \times b$ to $a'$ and $b'$ respectively, meaning that we can set $f \times g$ to the unique arrow going between $a \times b$ and $a' \times b'$.
\end{example}

By duality, there is also a bifunctor corresponding to the coproduct if it is defined everywhere. What would these two examples mean in Haskell? The product is the 'pair functor' `(,)`, and the coproduct is the sum type `Either`.

```haskell
instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)

instance Bifunctor Either where
    bimap f _ (Left x)  = Left (f x)
    bimap _ g (Right y) = Right (g y)
```

These are examples of type constructors (or algebraic data types, as we have seen). Since functors compose, we could ask ourselves: "Are all algebraic data types functors?". The answer is positive, and this allows that Haskell language to derive an implementation of `fmap` for all ADTs!

**References:**

- 2.1, 2.2, 3.1, 3.3 (partially) and 3.4 (partially) of Mac Lane
- 1.5, 1.6 and 1.8 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 2.6.7, 5.1, 5.2, 5.4 of Barr and Wells
- *Catsters*: Products and coproducts <https://www.youtube.com/watch?v=upCSDIO9pjc>



# Pure functional programming

What are functional languages, and what are the limiting things.

- purity
- immutability

list of problems:

example of Haskell program, composibility

# Monads

**References:**

- 6.1 and parts of 6.3 and 6.4 of Mac Lane
- Blogs..
- Catsters

# Monads II

## Monads in programming

## Kleisli categories

Reader writer

# Closed cartesian categories, function types

Discuss Curry-Howard Isomorphism?

**References:**

- 1.9 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 6.1, 6.2, 6.3 of Barr and Wells

# Yonedda's Lemma

# Lenses; Adjunctions and profunctors

- <https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation>
- <https://github.com/ekmett/lens>

## Hom-functors

Hom-sets give rise to a specific type of functor, the (co- and contravariant) hom-functor. See page 34 of Mac Lane.

# Limits and colimits

# Ends and co-ends

# 'Theorems for free!'

# 'Fast and loose reasoning is morally correct'

**References**

- About **Hask**: <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf>
- <http://math.andrej.com/2016/08/06/hask-is-not-a-category/>

# F-algebras:

Recursion, coalgebras and streams

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
