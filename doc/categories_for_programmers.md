% Category Theory and its Application to (Functional) Programming
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
    - \usepackage{hyperref}
    - \hypersetup{colorlinks, urlcolor=blue}
    - \urlstyle{tt}
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
    - \newtheorem{lemma}{Lemma}
    - \newtheorem{definition}{Definition}
    - \newtheorem{proposition}{Proposition}
    - \newtheorem{corollary}{Corollary}
    - \newtheorem{example}{Example}
    - \newcounter{common}
    - \usepackage{aliascnt}
    - \makeatletter
        \let\c@theorem\relax
        \let\c@lemma\relax
        \let\c@definition\relax
        \let\c@example\relax
        \let\c@proposition\relax
        \let\c@corollary\relax
        \let\c@proposition\relax
        \makeatother
    - \newaliascnt{theorem}{common}
    - \newaliascnt{lemma}{common}
    - \newaliascnt{definition}{common}
    - \newaliascnt{example}{common}
    - \newaliascnt{proposition}{common}
    - \newaliascnt{corollary}{common}
    - \numberwithin{common}{chapter}
    - \renewcommand{\thetheorem}{\thechapter.\arabic{theorem}}
    - \renewcommand{\thelemma}{\thechapter.\arabic{lemma}}
    - \renewcommand{\thedefinition}{\thechapter.\arabic{definition}}
    - \renewcommand{\theexample}{\thechapter.\arabic{example}}
    - \renewcommand{\theproposition}{\thechapter.\arabic{proposition}}
    - \renewcommand{\thecorollary}{\thechapter.\arabic{corollary}}
    - \usepackage{geometry}
    - \geometry{margin=4cm}
---

This document contains notes for a small-scale seminar on category theory in the context of (functional) programming, organized at CWI. The goal of the seminar is to gain familiarity with concepts of category theory that apply (in a broad sense) to the field of functional programming. It could be an idea to have an associated (toy) project that examplifies the concepts that are discussed.

Although the main focus will be on the mathematics, examples should be made in Haskell to illustrate how to apply the concepts, and possibly examples in other languages as well (such as Python and C++).

I would like to thank:

- Tom Bannink for supplying the proof for the bifunctor example in Chapter 3.

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
$$\mu = \{ \mu_a: Fa \to Ga~|~a \in \mathcal{C} \},$$
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

## References

- 1.1 -- 1.4 and 1.8 of Mac Lane
- 1.1, 1.2, 2.1, 3.1 and 3.2 of Asperti and Longo
- 2.1, 2.7, 2.8, 3.1, 4.2, 4.3 of Barr and Wells
- 1.1, 1.7, 1.10 of the 'Category Theory for Programmers' blog by Bartosz Milewski (best to study after reading Chapter 2)

# Types and functions: a category for programmers

\epigraph{""A monad is a monoid in the category of endofunctors, what's the problem?"}{\emph{James Iry jokes about Haskell in his blog post A Brief, Incomplete, and Mostly Wrong History of Programming Languages}}

To establish a link between functional programming and category theory, we need to find a category that is applicable. Observe that a _type_ in a programming language, corresponds to a _set_ in mathematics. Indeed, the type `int` in C based languages, corresponds to some finite set of numbers, the type `char` to a set of letters like `'a'`, `'z'` and `'$'`, and the type `bool` is a set of two elements (`true` and `false`). This category, the category of types, turns out to be a very fruitful way to look at programming.

Why do we want to look at types? Programming safety and correctness. In this part we will hopefully give an idea of how category theory applies to programming, but we will not go into to much detail yet, this is saved for later parts.

We will take as our model for the category of types the category **Set**. Recall that the elements of **Set** are sets, and the arrows correspond to maps. There is a major issue to address here: Mathematical maps and functions in a computer program are not identical (bottom value $\perp$). We may come back to this, but for now we consider **Set**..

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

As mentioned, `List` is implemented in the standard library as `[]`, and `Cons` is written as `:`, while the empty list is written also as `[]`. This allows you to write:
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

```haskell
fmap f . alpha = alpha . fmap f
```

This can be shown in a very general context, and it has to do with the fact that the 'bodies'for `f`, `fmap` and `alpha` are the same for all types. We will show this in an upcoming part when we discuss _free theorems_.

Let us revisit our `head :: [a] -> Maybe a` example, and consider the naturality condition here. It says that:

```haskell
fmap f . head = head . fmap f
```
Here, the fmap on the lhs corresonds to the `Maybe` functor, while on the rhs it corresponds to the `[]` functor. The lhs can b e read like this; take the first element of the list, then apply f on it. The rhs can be read as "apply the function f to the enitre list, then take the first element". The result is the same; the funtion f applied to the head of the list (if any). But for the rhs we apply the function `f` for each element in the list, while on the lhs we only apply it to the head. Because of the constraint on polymorphic function, the compiler knows that the result is equal and can choose which one to use!

## References

- 1.2, 1.7 of the 'Category Theory for Programmers' blog by Bartosz Milewski

# Products, Co-products and Algebraic Datatypes

## Duality and products of objects

\subsection*{Duality}

For any category, we can define the category with all arrows (and composition) reversed.

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

Initial objects and terminal objects have a *universal property*, they are defined by the property that e.g. all other objects have a *unique morphism to the object*. A more involved example of such a universal property is the *notion of a product of objects*. The categorical product is a unifying definition for many 'products' encountered in mathematics, such as the cartesian product, product group, products of topological spaces, and so on.

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
& 2 \arrow[dl, "\times 4"'] \arrow[dr, "\times 8"] \arrow[d, "\times 4"] & \\
8 & 8 \arrow[l, "\times 1"] \arrow[r, "\times 2"'] & 16
\end{tikzcd}
\end{figure}
$$4 = 2 \times 2,~8 = 4 \times 2.$$
This seems to indicate that in 'some category related to numbers' (in fact, precisely the category of natural numbers with arrows to their multiples, that we gave as an example in the first chapter), the product would correspond to the gcd!


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
     Although this is mostly an entertaining (and, depending on your view, an overly complicated) way of looking at types, a similar correspondence from types to logical operations forms the basis of the Curry-Howard isomorphism that connects type theory to logic in a very fundamental way.

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

We now ask ourselves how bifunctors relate to functors. This is summarized in the following proposition, where we denote pairs as $\langle c, d \rangle \in \mathcal{C} \times \mathcal{D}$:

\begin{proposition}
\label{prop:bifunctorcomponents}
Let $F: \mathcal{C} \times \mathcal{D} \to \mathcal{E}$ be a bifunctor. Then:
\begin{align*}
    F \langle c, - \rangle \equiv G_c&: \mathcal{D} \to \mathcal{E},~d \mapsto F \langle c, d \rangle,~(g: d \to d') \mapsto F \langle \text{id}_c, g \rangle \\
     F \langle -, d \rangle \equiv H_d&: \mathcal{C} \to \mathcal{E},~c \mapsto F \langle c, d \rangle,~(f: c \to c') \mapsto F \langle f, \text{id}_d \rangle
\end{align*}
are functors for all $c \in \mathcal{C}$ and $d \in \mathcal{D}$ respectively, and furthermore they satisfy:
\begin{align}
G_c d &= H_d c \label{bif1}\\
G_{c'} g \circ H_d f &= H_{d'} f \circ G_c g \label{bif2}
\end{align}
for all $c, c' \in \mathcal{C}$ and $d, d' \in \mathcal{D}$.
Conversely, let $G_c, H_d$ be family of functors so that \eqref{bif1} and \eqref{bif2} hold, then:
$$\tilde{F}: \mathcal{C} \times \mathcal{D} \to \mathcal{E},~\langle c, d \rangle \mapsto G_c d,~\langle f, g \rangle \mapsto H_{d'} f \circ G_c g$$
\end{proposition}
is a bifunctor, and satisfies $\tilde{F} \langle c, - \rangle = G_c$ and $\tilde{F} \langle -, d \rangle = H_d$.

\begin{proof}
Let us first show that we can construct the functors $G_c$ and $H_d$ from a bifunctor $F$. We show that $G_c$ is a functor, $H_d$ follows similarly.
\begin{align*}
G_c(\text{id}_d) &= F \langle \text{id}_c, \text{id}_d \rangle = \text{id}_{F \langle c, d \rangle} \\
G_c(g \circ g') &= F \langle \text{id}_c, g \circ g' \rangle = F (\langle \text{id}_c, g \rangle \circ \langle \text{id}_c, g' \rangle) \\
&= F \langle \text{id}_c, g \rangle \circ F \langle \text{id}_c, g' \rangle = G_c g \circ G_c g'
\end{align*}
and clearly the mapped arrows have the correct (co)domains, hence $G_c$ is a functor for all $c$. Showing \eqref{bif1} is simply, by definition both sides are equal to $F \langle c, d \rangle$. To show \eqref{bif2} we compute:
\begin{align*}
G_{c'} g \circ H_d f &= F \langle \text{id}_{c'}, g \rangle \circ F \langle f, \text{id}_d \rangle \\
&= F(\langle \text{id}_c, g \rangle \circ \langle f, \text{id}_d \rangle) = F(\langle f, g \rangle) = F(\langle f, \text{id}_{d'} \rangle \circ \langle \text{id}_c, g \rangle) \\
&= F\langle f, \text{id}_{d'} \rangle \circ F\langle \text{id}_c, g \rangle = H_{d'} f \circ G_c g
\end{align*}

To show the converse statement, we compute:
\begin{align*}
F \langle \text{id}_c, \text{id}_d \rangle &= G_c \text{id}_d \circ H_d \text{id}_c = \text{id}_{G_c d} \circ \text{id}_{H_d c} = \text{id}_{F \langle c, d \rangle} \circ \text{id}_{F \langle c, d \rangle} = \text{id}_{F \langle c, d \rangle} \\
F(\langle f, g \rangle \circ \langle f', g' \rangle) &= F\langle f \circ f', g \circ g' \rangle = G_{c'} g \circ G_{c'} g' \circ H_d f \circ H_d f' \\
&= G_{c'} g \circ H_{d'} f \circ G_{c} g' \circ H_d f' = F \langle f, g \rangle \circ F \langle f', g' \rangle
\end{align*}
\qedhere
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

\begin{figure}[H]
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

These are examples of type constructors (or algebraic data types, as we have seen). Since functors compose, we could ask ourselves: "Are all algebraic data types functors?". The answer is positive, and this allows the Haskell language to derive an implementation of `fmap` for all ADTs!

## References

- 2.1, 2.2, 3.1, 3.3 (partially) and 3.4 (partially) of Mac Lane
- 1.5, 1.6 and 1.8 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 2.6.7, 5.1, 5.2, 5.4 of Barr and Wells
- *Catsters*: Products and coproducts <https://www.youtube.com/watch?v=upCSDIO9pjc>

# The Yoneda Lemma

The Yoneda Lemma relates a category $\mathcal{C}$ with the functors from $\mathcal{C}$ to $\mathbf{Set}$. Before we can introduce the lemma's we will introduce a number of concepts; first we introduce a class of functors called *hom-functors*, we introduce the notion of *representable functors*, we will discuss the *Yoneda embedding* and finally we will move on to the Yoneda Lemma; one of the important tools in category theory

## Hom-functors

The *hom-functor* for some fixed object $c$, is a functor that sends any object $a$ to the hom-set $\text{Hom}(c, a)$. It is clear that for each object we get an associated object in **Set**, but what should this functor do with arrows? We will denote the candidate functor with $F = \text{Hom}(c, -)$. Say we have an arrow $f: a \to b$:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
a \arrow[d, "f"] \arrow[r, "F"] & \text{Hom}(c, a) \arrow[d, "?"] \\
b  \arrow[r, "F"] & \text{Hom}(c, b)
\end{tikzcd}
\end{figure}

The arrow marked with a question marked is an arrow in **Set**. Arrows in sets are functions, which we can define by saying what it does on elements. The elements of the hom-sets are arrows in $\mathcal{C}$. Given some element of $\text{Hom}(c, a)$, i.e. an arrow in $\mathcal{C}$: $g: c \to a$, we need to obtain an element of $\text{Hom}(c, b)$, i.e. an arrow from $c \to b$. We have the following picture

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
c \arrow[r, "g"] \arrow[rr, bend left, "Ff(g) = ?"] & a & b
\end{tikzcd}
\end{figure}

We can go to $a$ from $c$ using $g$, but then we need a way to get from $a$ to $b$. We actually have a way to do this, namely the arrow $f: a \to b$ that we started with. We need only to compose! This motivates the following definition:

\begin{definition}
Let $\mathcal{C}$ be a category, and let $c \in \mathcal{C}$ and $f: a \to b \in \mathcal{C}$. We define the (covariant) \textbf{hom-functor} $\text{Hom}(c, -): \mathcal{C} \to \mathbf{Set}$ as:
\begin{align*}
\text{Hom}(c, -)(a) = &\text{Hom}(c, a) \\
\text{Hom}(c, -)(f) : &\text{Hom}(c, a) \to \text{Hom}(c, b),\\
                      &g \mapsto f \circ g
\end{align*}
Clearly the identity arrow gets mapped to the identity map. To show that compositions are preserved, we compute for any arrow $h: c \to a$:
\begin{align*}
\text{Hom}(c, -)(g \circ f)(h) &= (g \circ f) \circ h \\
                               &= g \circ (f \circ h) \\
                               &= g \circ (\text{Hom}(c, -)(f)(h)) \\
                               &= \text{Hom}(c, -)(g) \left( \text{Hom}(c, -)(f)(h)\right) \\
                               &= \left(\text{Hom}(c, -)(g) \circ \text{Hom}(c, -)(f) \right)(h)
\end{align*}
\end{definition}

We can also define the contravariant hom-functor: $\mathcal{C}^{\text{op}} \to \mathbf{Set}$ by *precomposing with $f$*, and we denote it as $\text{Hom}(-, d)$.

Let us introduce a term; functors are called **naturally isomorphic** if there is a natural transformation between them for which all components are isomorphisms. Hom-functors are such an important class of functors from $\mathcal{C} \to \mathbf{Set}$, that they motivate the following definition:

\begin{definition}
A functor $F: \mathcal{C} \to \mathbf{Set}$ is called \textbf{representable} if it is naturally isomorphic to a hom-functor.
\end{definition}

To simplify the notation in the upcoming sections, we will denote the covariant hom-functor $\text{Hom}(a, -) = h^a$ and the contravariant hom-functor $\text{Hom}(-, b) = h_b$.

## Yoneda Embedding

For any category $\mathcal{C}$ the Yoneda embedding is a functor between the opposite category and the category of functors between $\mathcal{C}$ and **Set**. Let us first introduce this target category.

\begin{definition}
Let $\mathcal{C}$ and $\mathcal{D}$ be two categories, then we define $\mathbf{Fun}(\mathcal{C}, \mathcal{D})$ as the category with as objects functors $\mathcal{C} \to \mathcal{D}$, and as arrows natural transformations between these functors.
\end{definition}

Now, we are ready to describe the Yonedda embedding. Note that because it is a functor between *the opposite of* $\mathcal{C}$ and the category of *functors* between $\mathcal{C}$ and **Set**, it should take objects to functors, and arrows to natural transfomrations. For all objects, we have introduced a functor associated to it in the previous section; the *hom-functor*.

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
a \arrow[d, "f"'] \arrow[r, "Y"] & h^a \arrow[d, Leftarrow, "Yf"] \\
b \arrow[r, "Y"] & h^b
\end{tikzcd}
\end{figure}

The natural transformation $Yf$ should have components which are arrows in **Set**, indexed by objects in $\mathcal{C}$. Let $k: d \to c$ (note the reversed order), the corresponding naturality square looks like this:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
\text{Hom}(a, c) \arrow[r, "(Yf)_c"] \arrow[d, "h^a(k)"'] & \text{Hom}(b, c)  \arrow[d, "h^b(k)"] \\
\text{Hom}(a, d) \arrow[r, "(Yf)_d"] & \text{Hom}(b, d)
\end{tikzcd}
\end{figure}

So the natural components should be maps between hom-sets, and again we can find these maps by composition! This is summarized in the following definition:

\begin{definition}[Yoneda embedding]
The \textbf{Yoneda functor} $Y: \mathcal{C}^{\text{op}} \to \mathbf{Fun}(\mathcal{C}, \mathbf{Set})$, is defined as follows. Let $a \in \mathcal{C}$ and $f: b \to c$ in $\mathcal{C}$.
\begin{align*}
Ya =& h^a \\
Yf^{\text{op}} :& h^c \to h^b \\
(Yf^{\text{op}})_a:& \text{Hom}(c, a) \to \text{Hom}(b, a) \\
                  :& (g: c \to a) \mapsto (g \circ f: b \to a) \\
                  =& h_a f
\end{align*}
\end{definition}

Note that the component is defined using *pre-composition*, it is a contravariant hom-functor, whereas the objects $Ya$ are *covariant* hom-functors, i.e. use *post-composition*. Let us check that $Yf$ is indeed a natural transformation by looking at the naturality square introduced above, let $\ell: a \to c$, and lets trace it through the diagram:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
\ell \in \text{Hom}(a, c) \arrow[r, "(Yf)_c"] \arrow[d, "h^a(k)"'] & \text{Hom}(b, c) \ni \ell \circ f  \arrow[d, "h^b(k)"] \\
k \circ \ell \in \text{Hom}(a, d) \arrow[r, "(Yf)_d"] & \text{Hom}(b, d) \ni k \circ (\ell \circ f) = (k \circ \ell) \circ f
\end{tikzcd}
\end{figure}
In other words, the naturality condition corresponds simply to the associativity in $\mathcal{C}$. We say that $Yf$ is the \emph{induced natural transformation} of $f$.

The reason that the Yoneda functor is of such interest is because of the following:
\begin{theorem}
The Yoneda functor $Y$ is \emph{full} and \emph{faithful}.
\label{thm:yon_full_faithful}
\end{theorem}
We will prove this in the next section, after we state and prove the Yoneda lemma. Theorem \ref{thm:yon_full_faithful} has the following corollary:

\begin{corollary}
Let $\mu: h^a \to h^b$ be a natural transformation between hom-functors, then it is given by composition with a unique arrow $f: b \to a$. Furthermore, $\mu$ is a (natural) isomorphism if and only if $f$ is an isomorphism.
\label{cor:natural_transformation_arrow}
\end{corollary}

This means in particular that if a set-valued functor $F$ is represented by both $a$ and $b$, then there is an isomorphism $a \overset{\sim}{\rightarrow} b$.

Again, by duality, there exists also a full and faithful functor from $\mathcal{C} \to \mathbf{Fun}(\mathcal{C}^{\text{op}}, \mathbf{Set})$.

## The Yoneda Lemma

Corollary \ref{cor:natural_transformation_arrow} tells us that any natural transformation between covariant hom-functors $h^a$ and $h^b$ is given by composition with an arrow in the reverse direction $f: b \to a$. Note that this arrow is an element of $h^b a = \text{Hom}(b, a)$.

Less obviously, this result holds also for natural transformations between $h^a$ and any other set-valued functor $F$.

What would a function between $h^a$ and $F$ look like? We see that a component of the natural transformation should take an element from $h^a b$, i.e. an arrow $g: a \to b$, to some element of $Fb$. We can do this by *evaluating* the lifted arrow $Fg$ , which is a map between the sets $Fa$ and $Fb$, at a fixed $x \in \mathcal{C}$.

This gives us an idea for a natural transformation corresponding to an element of $Fa$. We summarize this in the following proposition:

\begin{proposition}
Let $F: \mathcal{C} \to \mathbf{Set}$ be a functor, and $a \in \mathcal{C}$. Any element $x \in Fa$ induces a natural transformation from $h^A$ to $f$, by evaluating any lifted arrow in $x$.
\end{proposition}

\begin{proof}
We have to show that this induces a natural transformation, i.e\ that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
h^a b \arrow[d, "h^a f"'] \arrow[r, "F \_ (x)"] & F b \arrow[d, "F f"] \\
h^a c \arrow[r, "F \_ (x)"'] & F c
\end{tikzcd}
\end{figure}
Here we denote:
$$F \_ (x): h^a b \to F b,~f \mapsto F f(x).$$
To show that the diagram commutes, fix an arrow $g: a \to b \in h^a b$. If we start taking it along the top side we obtain:
\begin{align*}
F f (F g(x)) = (F f \circ F g)(x) = F(f \circ g)(x) = (F \_ (x))(f \circ g) = (F \_ (x))((h^a f)(g))
\end{align*}
which is equal to taking it along the bottom, hence the diagram commutes.
\qedhere
\end{proof}

The Yoneda lemma states that *all natural transformations between $h^a$ and $F$ are of this form*.

\begin{theorem}[The Yoneda lemma]
Let $\mathcal{C}$ be a category, let $a \in \mathcal{C}$, and let $F: \mathcal{C} \to \mathbf{Set}$ be a set-valued functor. There is a one-to-one correspondence between elements of $Fa$, and natural transformations:
$$\mu: h^a \Rightarrow F.$$
\end{theorem}

\begin{proof}
We already saw that each element of $Fa$ induces a natural transformation, so we have a map:
$$\Phi: F a \to \text{Nat}(h^a, F).$$
Here, $\text{Nat}(h^a, F)$ denotes the set of natural transformations between $h^A$ and $F$. We now need to show show that $\Phi$ has an inverse.
Let $\mu$ be any natural transformation, then we can obtain an element of $F a$ by looking at the component $\mu_a$ and let it act on the identity arrow $\text{id}_c \in h^a a$, i.e.:
$$\Psi: \mu \mapsto \mu_a(\text{id}_a).$$
Now let us show that $\Phi$ and $\Psi$ are inverses of each other. First, we compute:
\begin{align*}
    \Psi(\Phi(x)) = \Psi(F \_ (x)) = F \text{id}_a (x) = \text{id}_{F a}(x) = x,
\end{align*}
so $\Psi$ is a left inverse of $\Phi$. To show that it is also a right inverse, we need to show that:
$$\Phi(\Psi(\mu)) = \mu,$$
or in components:
$$\Phi(\Psi(\mu))_b = \mu_b.$$
We note that by definition, for any $f: a \to b$ in $h^a b$:
$$\Phi(\Psi(\mu))_b (f) = (\Phi(\mu_a(\text{id}_a)))_b (f) = Ff (\mu_a(\text{id}_a)).$$
Since $\mu$ is a natural transformation we have that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
h^a a \arrow[d, "h^a f"'] \arrow[r, "\mu_a"] & F a \arrow[d, "F f"] \\
h^a b \arrow[r, "\mu_b"'] & F b
\end{tikzcd}
\end{figure}
In particular, consider the element $\text{id}_a \in h^a a$. Tracing this along bottom this gets mapped to $\mu_b (f)$, while along the top it gives precisely $Ff (\mu_a(\text{id}_a))$, so we have shown that:
$$\Phi(\Psi(\mu))_b (f) = Ff (\mu_a(\text{id}_a)) = \mu_b(f).$$
And hence, $\Psi$ is also a right inverse of $\Phi$, and thus $\Phi$ is a bijection, as required.

\qedhere

\end{proof}

One can also show, that this correspondence is 'natural' in $a \in \mathcal{C}$ and $F$.

Let us now prove Theorem \ref{thm:yon_full_faithful}.

\begin{proof}
\qedhere
\end{proof}

Let us recap what we have seen so far. We discussed a special class of set-valued functors called *hom-functors*. These hom-functors, like hom-sets, relate objects directly with the arrows between them.

Next we showed that we can *embed* any category into the category of contravariant set-valued functors of this category, sending objects to their hom-functors. We also showed that this embedding, as a functor, is *full* and *faithful*, which suggests that all the information of the category and its objects, is contained in its hom-functors.

When looking at what this means for the arrows, we noted that in particular any natural transformation between hom-functors is given by composition with arrows of our category.

To prove this, we stated and proved the Yoneda lemma -- which is an important result in its own right. It shows that for an arbitrary set-valued functor, there is a bijection between elements of the set $F a$ and natural transformations from $h^a$ to $F$,

All functors in Haskell are set-valued, since that is our category of interest. We first show two simple applications of Yoneda's lemma in mathematics, and next we see some initial applications of the Yoneda lemma to Haskell. In later parts we will see more advanced uses.

## Examples of applications

\begin{example}[Matrix row operations]
In linear algebra, row operations can be performed without changing the solutions of the linear system. Examples are row permutations, adding the j-th row to the i-th row, or multiplying a row by a (non-zero) scalar. We will show that these \emph{row operations} are natural, in the following sense.

Let $\mathcal{C}$ be the category where the objects are natural numbers $1, 2, 3, \ldots$, and where arrows $n \to m$ correspond to $m \times n$ matrices. Composition is given by matrix multiplication, indeed if we have arrows:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
n \arrow[r, "A_{m \times n}"] & m \arrow[r, "B_{k \times m}"] & k
\end{tikzcd}
\end{figure}
then the composite $B_{k \times m} A_{m \times n} = C_{k \times n}$ is an arrow from $n$ to $k$, as required. Consider contravariant hom-functors $h_n$ for this category. The hom-set $h_n k = \text{Hom}(k, n)$ consists of $n \times k$ matrices. To show that row operations can be seen as natural transformations $\mu: h_n \Rightarrow h_n$, we fix some $k \times m$ matrix $B$, and look at the following naturality square:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
h_n k \arrow[r, "\mu_k"] \arrow[d, "h_n B"'] & h_n k \arrow[d, "h_n B"]\\
h_n m \arrow[r, "\mu_k"] & h_n m
\end{tikzcd}
\end{figure}
Considering some $n \times k$ matrix $A$, the naturality condition states:
$$\mu(A) B \overset{?}{=} \mu(AB).$$
To show this, we observe that for all row transformations we have:
$$\mu(A) = A + \tilde{A}$$
where the rows of $A$ are either empty, or are multiples of rows of $A$, or:
$$\mu(A) = A + \text{diag}(\lambda_1, \ldots, \lambda_n) A.$$
This means we have
$$\mu(A) B = (A + \text{diag}(\lambda_1, \ldots, \lambda_n) A) B = AB + \text{diag}(\lambda_1, \ldots, \lambda_n) AB = \mu(AB).$$
as required. By Corollary \ref{cor:natural_transformation_arrow} we have that any natural transformation $\mu: h_n \Rightarrow h_n$ is given by postcomposition with a unique arrow $D: n \to n$. The Yoneda lemma allows us to identify this arrow, it is equal to:
$$D = \mu_n(\text{Id}_n),$$
so to perform row operations on a matrix, one can equivalently left multiply with a matrix obtained by applying these operations to the identity matrix. This powers the technique of manually inverting a matrix $A$, where you perform row operations to the matrix $A$ and simultaneously to another matrix $B$ that is initially the identity matrix, until you reduce $A$ to the identity matrix. The resulting matrix $B$, when left multiplied with the original $A$ will perform the row operations, and hence $BA = \text{Id}$, or $B = A^{-1}$.

\end{example}

\begin{example}
Another application of Yoneda is the following classic result from group theory:
\begin{corollary}[Cayley's Theorem]
Any group $G$ is isomorphic to a subgroup of a permutation group.
\end{corollary}

\begin{proof}
Recall that we can view a group $G$ as a category $\mathcal{C}_G$ with a single object $\{ \bullet \}$ and with arrows $\bullet \to \bullet$ corresponding to the elements of $g$. Consider the Yoneda embedding $Y$ of this category into $\mathbf{Fun}(\mathcal{C}_G^{\text{op}}, \mathbf{Set})$, and in particular we consider the shape of the image of $\bullet$ under the contravariant hom-functor $h_\bullet$:
\begin{figure}[H]
\centering
\begin{tikzcd}
\bullet \arrow[loop left, dashed, "G"] \arrow[r, "Y"] & h_\bullet \arrow[loop right, dashed, "\text{Nat}(h_{\bullet}{,} h_{\bullet})"]
\end{tikzcd}
\end{figure}
The arrows on the left (displayed collectively using a dashed arrow), corresponding to the elements of $G$, get mapped \emph{fully and faithfully} (by Theorem \ref{thm:yon_full_faithful}) to the natural transformations between $h_\bullet$ and itself (natural endomorphisms).

The natural endomorphisms $h_\bullet$ are characterized, by Corollary \ref{cor:natural_transformation_arrow}, (at the only component $G$) by left-multiplication of elements $G$ on the set $h_\bullet \bullet \simeq G_{\text{set}}$ which is the underlying set of $G$ (since it is $\text{Hom}(\bullet, \bullet)$). For each element $g \in G$ we obtain an automorphism $G_{\text{set}} \to G_{\text{set}}$ given by $h \mapsto gh$.

Recall that $\text{Aut}(G_{\text{set}})$ is a group (a permutation group), and note that the collection of automorphisms defined by left multiplication of elements of $G$ is indeed a subgroup of this permutation group. The correspondence between $G$ and the "automorphisms by left-multiplication" is easily seen to be a group isomorphism.
\qedhere
\end{proof}


\end{example}

## Yoneda in Haskell

We will discuss two examples, the first is a hopefully intuitive way of looking at Yoneda's lemma, by pinpointing a function with a single evaluation, while the second has to do with Generalized ADTs.

Let us first see how we can translate the relevant tools of Yoneda to Haskell. We have the following concepts:

- *hom-sets* : the hom-set of types `a` and `b` are the arrows between `a` and `b`, i.e. functions of the type `(a -> b)`. Note that this hom-set is again in the category of types.
- The *hom-functor* corresponding to a type `a` should be a functor, i.e. a type constructor, that produces the hom-set `(a -> b)` when given a type `b`, for some fixed type `a`. On functions `(b -> c)` it should get a function between the hom-sets of `a` and `b, c` respectively, i.e.:
```haskell
instance Functor (HomFunctor a) where
    fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap f g = f . g
```
And indeed, we see that we can simply use composition.
- Yoneda's lemma says that for any other functor `F`, we can produce a natural transformation from the hom-functor for `a` (i.e.\ polymorphic function) by looking at elements of `F a`.

Next we look at a simple example.

### Reverse engineering machines

We set `F` equal to `Id`, the identity functor, and consider a natural transformation between `HomFunctor a` and `Id`, this has the form (at the component `b`):

```haskell
--    (HomFunctor a) b      Id b
--            |               |
machine :: (a -> b)     ->    b
```
Say we are given any function with this signature, and we want to know how it is implemented. We can actually do this in a *single evaluation*, using the Yoneda lemma. The Yoneda lemma says precisely that such a *machine* is given uniquely by any element of `Id a = a`, i.e. some value of the type `a`. This makes a lot of sense in this context, since we can be given *any* `b`, and the only tool that we have to produce a value for `b` is to use the function `f :: a -> b` that is supplied to us. Furthermore, the polymorphic function should behave the same for any type, so it can only be implemented as:
```haskell
machine :: (a -> b) -> b
machine f = f x
```
where `x` is some fixed element of type `a`. Now, the Yoneda lemma also tells us a way to obtain `x`, we simply supply `f = id`:
```haskell
x <- machine id -- obtain the 'hidden element'
```

What if `F` is not the identity function, but say the `List` functor. The story actually does not change much, we now have a function with the signature:
```haskell
--    (HomFunctor a) b      List b
--            |               |
machine :: (a -> b)     ->   [b]
```
the Yoneda lemma says that internally, any function of this signature should maintain a list of the type `[a]`, and when given a function `f :: a -> b` it fmaps this over the internal list to produce a value of the type `[b]`. Again, we can get this list by feeding the `id` function into the machine.

### Generalized ADTs

## References

- 2.3, 2.4 and 2.5 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 2.2, 3.2 of Mac Lane.
- 3.1, 4.5 of Barr and Wells
- 2.2 of Riehl
- *Catsters*: Yoneda and representables : <https://www.youtube.com/playlist?list=PLUWfjhrIRed_PgAmTFFyuEtFRdJzgcOZE>
- Blogs:
    - <http://www.haskellforall.com/2012/06/gadts.html>
    - <http://blog.sigfpe.com/2006/11/yoneda-lemma.html>
    - <https://www.schoolofhaskell.com/user/bartosz/understanding-yoneda#yoneda-lemma>

# Monads and functional programming

\epigraph{"Mathematics is the art of giving the same name to different things"}{\emph{Henri Poincar\'e}}

Today, the most common programming style is *imperative*. Imperative programming lets the user describes *how* a program should operate, mostly by directly changing the memory of a computer. Most computer hardware is imperative; a processor executes a machine code sequence, and this sequence is certainly imperative. This is originally described by mathematicians such as Turing and von Neuman in the 30s.

A constrasting way of programming is *declarative programming*, which is a way of expressing *what* you want the program to compute (without explicitely saying how it should do this). A good way of expressing what you want to have computed, is by describing your program mathematically, i.e. *using functions*, which is what we explore here. This functional style of looking at computations is based on work in the 20s/30s by Curry and Church among others.

The difficulty in using a *(typed, pure) functional* programming language, is that the **functions that you write** between types **should behave like mathematical functions** on the corresponding sets. This means, for example, that if you call a function multiple times with the same arguments, it should produce the same result every time. This is often summarized as a *side-effect free function*. Other difficulties are that values are in principle immutable.

Something else that would allow us to more accurately describe our programs in a mathematical way is if executing is *lazy*, and Haskell indeed is lazy. This means we can work with **infinite lists and sequences**, and only peeking inside such as a list causes the necessary computations to be done (or 'collapses the wave function' if you want a quantum analogy).

We will explore what this means for actual programs, and discover what problems and difficulties pop up. In the coming chapters we will fix these problems (and go beyond!) using our categorical language.

## Problem 1: Computations and IO in Haskell

A typical Haskell program consists of the description of a number of functions, which are 'composed', and then run against some input^[In this section (and the following sections) we will write in 'pseudo-Haskell' to illustrate some points, not all examples will be valid Haskell programs.] Say we two functions:

```haskell
f :: Int -> Int
f x = ..

g :: Int -> Int
g x = ..
```

We want to perform `g . f`, and output the result.

### IO actions

First, assume that the input is static. The 'function' executed by Haskell is called `main`, and say there is some `print :: a -> ??` function that prints the value of any type to standard output. Then our code would look something like this

```haskell
-- NOTE: not real Haskell
main = print(g(f 123))
```

In Haskell there are two alternative ways of writing this composition (to prevent overuse of paranthesis):
```haskell
main = print $ g $ f 123
main = (print . g . f) 123
```
Here, `$` makes sure that all the fucntions on the right have been evaluated before statements on the left come in to play. There are a two things unclear about this code:

- If `main` should *behave*, then it should return the same function every time. However, we would like to support user input (`cin, scanf, getLine, ..)` so what should be its type if it should 'behave mathematically'?
- Similarly, for `print`, what would be its type? It should take a value, convert it to a string, and output this in a terminal somewhere. The first part seems doable, but what is the *type* of *printing to screen*?

Although this is quite a fundamental problem, Haskell 'fixes this' using *IO actions*. This is not limited to input/output for terminal, it can also be network related, or mouse/keyboard input for a video game!

An IO action is a *value* with a type of `IO a`. We can also have an 'empty IO action', if the result is not used. Let us look at some examples:

- The print function has the signature from a String to an IO action:

    ```haskell
    putStrLn :: String -> IO ()
    ```
    To print the value of any type, we precompose this function with `show :: a -> String`.

- The function `main` itself is an IO action! `main :: IO ()`.
- The `getLine` function is an IO action `getLine :: IO String`.


### Handling input

Let us use this `getLine` action to interact with the user of our program. We would like to do something like this:

```haskell
f :: Int -> Int
f x = 2 * x

main = print $ f getLine
```

But this does not type check! The action `getLine` has type `IO String`, while `f` expects an `Int`. Assume we could convert the `String` to an `Int`:

```haskell
toInt :: String -> Int
```

Then to work on the IO action, We want to *lift* `toInt` to take an `IO String` and produce an `IO Int`. This sounds like an `fmap`, and indeed `IO` provides fmap, it is a functor!

The `print`^[`print` actually corresponds to `(putStrLn . show)` in Haskell] statement we used here has signature `a -> IO ()`. Applying the `fmap` of IO on this function would give something like:

```haskell
fmap print :: IO a -> IO (IO ())
```

Since `main` should corespond to `IO ()`, we need either a way to remove a 'nested IO tag', or we need a function for functors that only lifts the first argument. I.e. let `F` be a functor, then we require either:

```haskell
join :: F (F a) -> F a
lift_one :: (a -> F b) -> (F a -> F b)
```

Since parantheses can be placed however (currying, we will describe this in more detail in the section on Closed Cartesian Categories (CCCs)), and order of arguments does not matter -- we can write `life_one` equivalently as:
```haskell
>>= :: F a -> (a -> F b) -> F b
```
Where the `>>=` (which is the Haskell notation, pronounced 'bind') is isomorphic to the arrow `lift_one`.

Note, that we have:
```haskell
join :: F (F a) -> F a
join x = x >>= id

-- indeed, consider `(>>=) x id`, where `x :: F (F a)`
id :: (F a -> F a) ==> b = a ==> join x :: F a
```
So we can define a `join` function only using our *bind*, so that implementing `>>=` is enough. Conversely, we can also retrieve bind from `join` and `fmap`:

```haskell
x >>= f = join (fmap f x)

-- e.g.
getLine >>= putStrLn
=> join fmap putStrLn(getLine)
=> join y -- y :: IO IO ()
=> z :: IO ()
```

Note also that we can pack an object inside an IO 'container':
```haskell
unit :: a -> IO a
```
So what does this give us. The bind notation can be used *infix*: so what would this code do:

```haskell
main = getLine >>= putStrLn
```

This is equivalent to `(>>=) getLine putStrLn`, whose type deduces to:
```haskell
F a = IO String ==> F = IO, a = String
a -> Fb ==> String -> IO () ==> b = ()
```
which gives us a type of `F b = IO ()`, as required. So the 'bind' function can be used to chain `IO` operations together!

What if we want to discard the inbetween values (because e.g. they are `IO ()`, when we output more than one line). This is kind of the role of `;` in imperative languages. For this there exists the `>>` notation:

```haskell
main = putStrLn "a" >> putStrLn "b"
```

The `>>` (pronounced: then) function can be implemented as:
```haskell
>> :: IO a -> IO b -> IO b

(>>) (putStrLn "a") (putStrLn "b")
==> a == b == ()
==> putStrLn "a" >> putStrLn "b" :: IO () -- as required

```

To summarize, `bind` and `return` allows us to **compose functions that may or may not require IO operations**.

## Problem 2: Data structures in Haskell

Trivial (finite) data structures are easily implemented in Haskell as product types, but we have also seen a different type of container namely a *functorial* one. The examples we have looked at so far are `[]` and `Maybe`. Let us explore these more deeply, and see how we can make their usage more flexible. First consider the `Maybe` functor. Say we have a number of functions, where some may or may not produce a result:

```haskell
f :: a -> Maybe a
g :: a -> a
h :: a -> Maybe a
```

And we want to do something like `h . g . f` using these definitions. What would happen:

1. Let `x :: a`.
2. Apply `f`, and obtain `f x :: Maybe a`
3. To apply `g`, we need to `fmap` it, so that `fmap g $ f x :: Maybe b`.
4. Now notice the pattern `h :: a -> F a`, which we have seen in the previous section, and we already saw the solution 'bind':
```haskell
(fmap g $ f x) >>= h
```
Although this is still somewhat ugly, we see that the *bind* function gives us the tools to do any computations with the Maybe monad regardless of the specific signature of the functions, and the order of composition. We can see that `bind` (and `return`) allow us to **compose arbitrary functions that may or may not fail**.

Next we look at `[]`. A common pattern, which is easy to do in mathematics, is when you have a function:

```haskell
f :: A -> B
```

to consider a sequence $a_1, a_2, \ldots$ and map this over $f$ to obtain $f(a_1), f(a_2), \ldots$. This corresponds of course to the `fmap` of the `[]`, and we already made this easy to do by considering `[]` as a functor. What does the `bind` operator do for us here? It lets us take a list of *inputs*, apply a function to each of them returning a variable number of *outputs*, and then gather all the results in a single list:

```haskell
[1, 2, 3] >>= \x -> [2 * x, 3 * x] -- [2, 3, 4, 6, 6, 9]
```

Here, the `bind` and `return` pair lets us **compose operations defined on data structures or their elements**.

## Problem 3: Side effects in Haskell

Both `IO`, `Maybe` and `[]` may be seen as 'functional containers', but let us now look at a completely different example where again we see that `bind` and `return` pair.

Consider a *function with side effects*, the canonical case is a function that logs that it has been used.
When thinking about how to accomplish this in Haskell, a candidate solution to letting some function `f :: a -> b` log a message, is to let it return a *pair* of `(b, String)`, where the `String` part contains the message.

As before, we want to **compose operations that involve logging or non-logging functions**. How would we do this in this case? First, we define the `Writer` functor. A `type` in Haskell corresponds to a `typedef` or `using` in C++.
```haskell
data Writer m a = Writer (a, m)

instance Functor (Writer m) where
    fmap f (Writer (a, m)) = Writer (f a, m)
```

This `Functor` instance allows us to lift functions that don't log to work with functions that log, however, what we really want to do do is to not worry about this decomposition, and any function that logs should just have the signature:
```haskell
logging_function :: a -> Writer String b
```
For this, we use the same schema we have seen before, and define two functions:
```
return :: a -> Logger a
x = Logger x
```

Note, as suggested by the way `Writer m a` was introduced, that if we would use some other *monoid* instead of `String`^[Indeed, String gives rise to a monoid with binary operation `++` (concatenation)], we could accomplish different goals than logging.

## Problem 4: Random numbers in Haskell

`State` monad

## Putting it together; Monads

In this section we have talked about `return` and *bind* `>>=`.

So, in various forms, we have seen the following pattern over and over: `F` is a functor (`IO, [], Maybe, Writer, ...`), along with two operations:

```haskell
join :: F F a -> F a
unit :: a -> F a
```

Note that in categorical terms:

- `unit` can be seen as a natural transformation between the identity endofunctor `Identity`, and `F`.
- `join` is a natural transformation between `F^2` and `F`.

## References

If you want to learn *yourself a bit of Haskell*, the following resources are helpful as a first step:

- 5 minute tutorial to get an idea: <https://tryhaskell.org/>
- A book that has quite a (cult) following in the Haskell community: <http://learnyouahaskell.com/chapters>
- The wiki book on Haskell is quite good: <https://en.wikibooks.org/wiki/Haskell>

About IO:

- <https://wiki.haskell.org/Introduction_to_IO>

Some posts dealing specifically with Monads from a Haskell perspective:

- <http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html>
- <https://bartoszmilewski.com/2013/03/07/the-tao-of-monad/>

# Monads

Definition of a Monad

Example: power set

Algebras of a monad

## References

- 6.1 and parts of 6.3 and 6.4 of Mac Lane
- Blogs:
    - <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/>
    - <https://bartoszmilewski.com/2016/11/30/monads-and-effects/>
    - <http://www.stephendiehl.com/posts/monads.html>
- Catsters

# Monads II

## Monads in programming

## Kleisli categories

Reader writer

## Background

- <https://golem.ph.utexas.edu/category/2012/09/where_do_monads_come_from.html>

# Closed cartesian categories, function types

Discuss Curry-Howard Isomorphism?

## References

- 1.9 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 6.1, 6.2, 6.3 of Barr and Wells
- <https://en.wikibooks.org/wiki/Haskell/The_Curry–Howard_isomorphism>

# Purely functional datastructures

- https://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504

# Adjunctions, Free monads

- https://www.youtube.com/watch?v=K8f19pXB3ts

# Lenses; Yoneda, adjunctions and profunctors

- <https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation>
- <https://github.com/ekmett/lens>

# Limits and colimits

# Ends and co-ends

# 'Theorems for free!'

# 'Fast and loose reasoning is morally correct'

## References

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
2. S. Mac Lane, Category Theory for the working mathematician
3. Barr and Wells, Category Theory for Computer Scientists
4. E. Riehl, Category theory in context,
