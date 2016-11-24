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
    - \newtheoremstyle{custom}
        {3pt} % Space above
        {3pt} % Space below
        {} % Body font
        {} % Indent amount
        {\bfseries} % Theorem head font
        {.} % Punctuation after theorem head
        {.5em} % Space after theorem head
        {}
    - \theoremstyle{custom}
    - \newtheorem{theorem}{Theorem}
    - \newtheorem{definition}{Definition}
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

We will write for objects and arrows respectively simply $a \in \mathcal{C}$ and $f \text{ in } \mathcal{C}$, instead of $a \in O$ and $f \in A$.

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

Another example of a category is a \emph{monoid}, which is a category of a single element. A monoid is a set $M$ with a binary operation $\cdot: S \times S \to S$ and a unit element (indeed, a group without necessarily having inverse elements, or a \emph{semi-group with unit}).

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

A functor is a very powerful concept, since intuitively it allows you to translate between different branches of mathematics! They also play an important role in functional programming.

**Examples**

The 'power-set functor': $\mathcal{P}:$ **Set** $\to$ **Set** sends subsets to their image under maps. Let $A, B \in$ **Set**, $f: A \to B$ and $S \subset A$:
\begin{align*}
\mathcal{P}A &= \mathcal{P}(A),\\
\mathcal{P}f&: \mathcal{P}(A) \to \mathcal{P}(B),~S \mapsto f(S)
\end{align*}

**Category of functors**

## Special objects, arrows and functors

For objects, we distuinguish two special kinds:

> **Definition:** An object $x \in \mathcal{C}$ is *terminal* if for all $a \in \mathcal{C}$ there is exactly one arrow $a \to x$. Similarly, it is *initial* if there is exactly one arrow $x \to a$ to all objects.


\begin{figure}[h]
\centering
\begin{tikzcd}[sep=tiny]
  &a \arrow[dr]&  \\
i\arrow[ur] \arrow[rr] \arrow[dr] & & t\\
  &b \arrow[ur] &
\end{tikzcd}
\end{figure}
Here, $i$ is initial, and $t$ is terminal.


For arrows we also have special kinds:

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is a \textbf{monomorphism} (or simply mono), if for all objects $x$ and all arrows $g, h: x \to a$ and $g \neq h$ we have:
$$g \circ f \neq h \circ f.$$
\end{definition}

To put this into perspective, we show that in the category \textbf{Set} monomorphisms correspond to injective functions;

\begin{theorem}
In \textbf{Set} a map $f$ is mono if and only if it is an injection.
\end{theorem}

\begin{proof}
Suppose $f: A \to B$ is injective, and let $g, h: X \to A$. If $g \neq h$, then $g(x) \neq h(x)$ for some $x$. But since $f$ is injective, we have $f(g(x)) \neq f(h(x))$, and hence $h \circ f \neq h \circ f$, thus $f$ is mono.

Suppose $f$ is mono. Let $\{ * \}$ be the set with a single element. Then for $x \in A$ we have an arrow $\{ * \} \to A$ corresponding to the constant function $\tilde{x}(*) = x$. Let $x \neq y$, then $f \circ \tilde{x} = f(x)$. Since $f$ is mono, $f \circ \tilde{x} \neq f \circ \tilde{y}$, and hence $f(x) \neq f(y)$, thus $f$ is an injection.\qedhere
\end{proof}

- Functors: full, faithful

## Natural transformations


# Types and functions: categories in functional programming

To define a function $f: A \to B$ in Haskell:

```haskell
    f :: A -> B
    g :: B -> C
    h = f . g
```

# Monads

# Ideas

Section on 'modularity':

> *Bartosz Milewski*: "... Elegant code creates chunks that are just the right size and come in just the right number for our mental digestive system to assimilate them. So what are the right chunks for the composition of programs? Their surface area has to increase slower than their volume ... The surface area is the information we need in order to compose chunks. The volume is the information we need in order to implement them ...  Category theory is extreme in the sense that it actively discourages us from looking inside the objects ... The moment you have to dig into the implementation of the object in order to understand how to compose it with other objects, you’ve lost the advantages of your programming paradigm."

# Literature

1. *Bartosz Milewski*: "Category Theory for Programmers", a blog post series that gives an excellent overview of interesting topics. <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>
2. Conceptual Mathematics: A first introduction to categories.
3. Maclane, Category Theory for the working mathematician
4. Category Theory for Computer Scientists

# Pandoc examples:
