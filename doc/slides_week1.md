---
numbersections: true
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

**Category theory for programmers**

\begin{definition}
A \emph{category} $\mathcal{C} = (O, A)$ is a set $O$ of \emph{objects} $a \in O$ and $A$ of \emph{arrows} $(f: a \to b) \in A$ between these objects, along with a notion of \emph{composition (f $\circ$ g) of arrows} and the existence of an identity arrow $\text{id}_a$ for each object $a \in O$, so that the following laws hold:
\end{definition}

- *Composition*: If $f: a \to b$ and $g: b \to c$ then $g \circ f: a \to c$.

\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[r, "f"] \arrow[rr, "g \circ f", bend right=50] & b \arrow[r, "g"]  & c
\end{tikzcd}
\end{figure}

- *Composition with identity arrows*:  If $f: x \to a$ and $g: a \to x$ where $x$ is arbitrary, then:
$$ \text{id}_a \circ f = f,~g \circ \text{id}_a = g.$$


\begin{figure}[h]
\centering
\begin{tikzcd}
a \arrow[loop left, "\text{id}_a"] \arrow[r, "g"', bend right=20] & x \arrow[l, "f"', bend right=20]
\end{tikzcd}
\end{figure}

- *Associativity*: If $f: a \to b$, $g: b \to c$ and $h: c \to d$ then:
$$(h \circ g) \circ f = h \circ (g \circ f).$$

\begin{figure}[h]
\centering
\begin{tikzcd}[sep=huge]
a \arrow[r, "f"] \arrow[d, "h \circ g \circ f"'] \arrow[rd, crossing over, near start, "g \circ f"] & b  \arrow[d, "g"] \arrow[ld, crossing over, near end, "h \circ g"] \\
d & c \arrow[l, "h"]
\end{tikzcd}
\end{figure}

If $f: a \to b$, then we say that $a$ is the *domain* and $b$ is the *codomain* of $b$. It is also written as:
$$\text{dom}(f) = a,~\text{cod}(f) = b.$$
The composition $g \circ f$ is only defined on arrows $f$ and $g$ if the domain of $g$ is equal to the codomain of $f$.

We will write for objects and arrows respectively simply $a \in \mathcal{C}$ and $f \in \mathcal{C}$, instead of $a \in O$ and $f \in A$.

**Examples of categories:**

here are also a number of very simple examples of categories:

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

- A partially ordered set (poset): binary relation $\leq$ over a set $S$ s.t.:

    - $a \leq a$
    - $a \leq b,~b \leq a \implies a = b$
    - $a \leq b,~b \leq c \implies a \leq c$

- $(\mathbb{N}, *)$, natural numbers with multiplication

\begin{figure}[h]
\centering
\begin{tikzcd}
1 \arrow[r, bend right=0] \arrow[rr, bend right=10] \arrow[rrr, bend right=20] \arrow[rrrr, bend right=30] \arrow[rrrrr, bend right=40] & 2 \arrow[rr, bend left=20, "\times 2"] \arrow[rrrr, bend left=30, "\times 3"] & 3 \arrow[rrr, bend left=20, "\times 2"] & 4 & 5 & 6 \ldots
\end{tikzcd}
\end{figure}

\begin{definition}
A \emph{monoid}, is a set $M$ with a associative binary operation $\cdot: S \times S \to S$ and a unit element. Category $\mathcal{C}(M)$ where:

$$O = \{ M \}$$
$$A = \{ m \in M \}$$
$$s_1 \circ s_2 \equiv s_1 \cdot s_2.$$

\end{definition}

Some examples of familiar categories:

* **Set**, sets, maps
* **Top** , topological spaces, continuous functions
* **Vect**, vector spaces, linear transformations
* **Grp**, groups, group homomorphisms



**Functors**

\begin{definition}
A \emph{functor} $T$ between categories $\mathcal{C}$ and $\mathcal{D}$ consists of two functions (both denoted $T$):
\begin{itemize}
\item An \emph{object function} $a \mapsto Ta \in \mathcal{D}$
\item An \emph{arrow function}
$$(f: a \to b) \in \mathcal{C}, (Tf: Ta \to Tb) \in \mathcal{D},$$
such that:
$$T(\text{id}_a) = \text{id}_{Ta},~T(g \circ f) = Tg \circ Tf.$$
\end{itemize}
\end{definition}

**Examples**

* The identity functor: $\text{id}_{\mathcal{C}}: \mathcal{C} \to \mathcal{C}$:
\begin{align*}
\text{id}_{\mathcal{C}}:~&a \mapsto a\\
&f \mapsto f
 \end{align*}

* The constant functor $\Delta_d: \mathcal{C} \to \mathcal{D}$ for fixed $d \in \mathcal{D}$:
\begin{align*}
\Delta_{d}:~&a \mapsto d\\
&f \mapsto \text{id}_d
 \end{align*}

* The 'power-set functor': $\mathcal{P}:$ **Set** $\to$ **Set**. Let $A, B \in$ **Set**, $f: A \to B$ and $S \subset A$:
\begin{align*}
\mathcal{P}A &= \mathcal{P}(A),\\
\mathcal{P}f&: \mathcal{P}(A) \to \mathcal{P}(B),~S \mapsto f(S)
\end{align*}

* Dual-functor
\begin{align*}
*&: \textbf{Vect} \to \textbf{Vect}\\
&: W \mapsto W^*\\
&: (f: V \to W) \mapsto (f^*: W^* \to V^*)
\end{align*}
Example of a \emph{contravariant functor} (a functor from \textbf{Vect} to $\textbf{Vect}^{\text{op}}$, the category with reversed arrows and composition rules.

\newpage

**Special objects, arrows, functors**

*Objects*

\begin{figure}[h]
\centering
\begin{tikzcd}[sep=tiny]
  &a \arrow[dr]&  \\
i\arrow[ur] \arrow[rr] \arrow[dr] & & t\\
  &b \arrow[ur] &
\end{tikzcd}
\end{figure}

\begin{definition}
An object $x \in \mathcal{C}$ is \textbf{terminal} if 
$$\forall a \in \mathcal{C}~\exists!~f: a \to x.$$
Similarly, it is \textbf{initial} if there is exactly one arrow $x \to a$ for all $a \in \mathcal{C}$.
\end{definition}


*Arrows*

There are a number of special kind of arrows:

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is \textbf{mono}, if $\forall x \in \mathcal{C}$ and all $g, h: x \to a$ with $g \neq h$ we have:
$$g \circ f \neq h \circ f.$$
\end{definition}

\begin{theorem}
In \textbf{Set} a map $f$ is mono iff it is an injection.
\end{theorem}

\begin{proof}
\emph{Inj $\implies$ Mono}

Let $f: A \to B$ injective, $g, h: X \to A$, $g \neq h \implies g(x) \neq h(x)$ for some $x$.
$$f \text{ inj.} \implies f(g(x)) \neq f(h(x)) \implies h \circ f \neq h \circ f.$$

\emph{Mono $\implies$ Inj}

$\{ * \}$ singleton set.
$$\forall x \in A~\exists!~\tilde{x}: \{ * \} \to A, \tilde{x}(*) = x.$$
Note $f \circ \tilde{x}(*) = f(x)$.
$$x \neq y \implies \tilde{x} \neq \tilde{y} \implies f \circ \tilde{x} \neq f \circ \tilde{y}$$
$$\implies f(x) \neq f(y).$$
\qedhere
\end{proof}

\begin{definition}
$f: a \to b \in \mathcal{C}$ is \textbf{epi}, if for all objects $x$ and all arrows $g, h: b \to x$:
$$g \circ f = h \circ f \implies g = h.$$
\end{definition}

\begin{definition}
An arrow $f: a \to b \in \mathcal{C}$ is \textbf{iso} if there exists $g: b \to a$ s.t.:
$$g \circ f = \text{id}_a~\text{ and }~f \circ g = \text{id}_b.$$
\end{definition}

In \textbf{Set}: Epi + Mono $\iff$ Iso. Not in general!

*Functors*

\begin{definition}
The \emph{hom-set} of $a$ and $b$, is the 'set' of all arrows from $a$ to $b$:
$$\text{Hom}_\mathcal{C}(a, b) = \{ f \in \mathcal{C}~|~f: a \to b \}.$$
\end{definition}

\begin{definition}
A functor $F: \mathcal{C} \to \mathcal{D}$ is \textbf{full} if for all pairs $a, b \in \mathcal{C}$ the induced function:
\begin{align*}
F:~\text{Hom}_\mathcal{C}(a, b) &\to \text{Hom}_\mathcal{C}(Fa, Fb),\\
   f &\mapsto Ff
\end{align*}
is a surjection. It is called \textbf{faithful} if it is an injection.
\end{definition}

When after applying $F$ an arrow $Ff$ or an object $Fa$ has a certain property (i.e. being initial, terminal or epi, mono), and it is implied that $f$ (or $a$) had this property, then we say the \emph{$F$ \textbf{reflects} the property}.

\begin{theorem}
A faithful functor reflects epis and monos.
\end{theorem}

\begin{proof}
E.g. monoic $Ff$. Let $f: a \to b$ such that $Ff$ is mono, and let $h,g: x \to a$ such that $h \neq g$.

\begin{figure}[h]
\centering
\begin{tikzcd}[sep=huge]
x \arrow[dr, "F"] \arrow[r, bend right=20, "g"'] \arrow[r, bend left=20, "h"]& a \arrow[dr, "F"]  \arrow[r, "f"] & b \arrow[dr, "F"]  &\\
& Fx \arrow[r, bend right=20, "Fg"'] \arrow[r, bend left=20, "Fh"]& Fa \arrow[r, "Ff"] & Fb\\
\end{tikzcd}
\end{figure}

$$g \neq h,~F \text{ faithf.} \implies Fg \neq Fh,$$
$$Ff \text{ mono} \implies Ff \circ Fg \neq Ff \circ Fh$$
$$F \text{ functor} \implies F(f \circ g) \neq F(f \circ h)$$
Thus, $f \circ g \neq f \circ h$, and therefore $f$ is mono.

\qedhere
\end{proof}


