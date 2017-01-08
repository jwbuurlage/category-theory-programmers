---
numbersections: false
documentclass: memoir
classoption: extrafontsizes
classoption: 17pt
header-includes:
    - \usepackage{tikz-cd}
    - \usepackage{amsthm}
    - \usepackage{epigraph}
    - \usepackage{float}
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
    - \newtheorem{corollary}{Cor}
    - \newtheorem{proposition}{Prop}
    - \newtheorem{example}{Ex}
    - \setlength{\columnsep}{1cm}
    - \usepackage[margin=2.0cm,landscape,twocolumn]{geometry}
    - \setlength{\parindent}{0em}
    - \setlength{\parskip}{0.5cm}
    - \pagestyle{empty}
---

**The Yoneda Lemma**

- $\mathcal{C} \overset{?}{\sim} \mathbf{Fun}(\mathcal{C}, \mathbf{Set})$

**Hom-functors**

$F: a \mapsto \text{Hom}(c, a)$.

$F = \text{Hom}(c, -)$. Let $f: a \to b$:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
a \arrow[d, "f"] \arrow[r, "F"] & \text{Hom}(c, a) \arrow[d, "? \text{ in } \mathbf{Set}"] \\
b  \arrow[r, "F"] & \text{Hom}(c, b)
\end{tikzcd}
\end{figure}

A function between sets. What does it do on elements, i.e.\ arrows in $\mathcal{C}$? Let $g: c \to a$, then $Fg \in \text{Hom}(c, b)$:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
c \arrow[r, "g"] \arrow[rr, bend left, "Ff(g) = ?"] & a & b
\end{tikzcd}
\end{figure}

\underline{Use $f$!}

\begin{definition}
Let $\mathcal{C}$ be a category, and let $c \in \mathcal{C}$ and $f: a \to b \in \mathcal{C}$. We define the (covariant) \textbf{hom-functor} $\text{Hom}(c, -): \mathcal{C} \to \mathbf{Set}$ as:
\begin{align*}
\text{Hom}(c, -)(a) = &\text{Hom}(c, a) \\
\text{Hom}(c, -)(f) : &\text{Hom}(c, a) \to \text{Hom}(c, b),\\
                      &g \mapsto f \circ g
\end{align*}
\begin{itemize}
\item Identity arrow gets mapped to the identity map. 
\item Composition:
\begin{align*}
\text{Hom}&(c, -)(g \circ f)(h) \\
                               &=(g \circ f) \circ h \\
                               &= g \circ (f \circ h) \\
                               &= g \circ (\text{Hom}(c, -)(f)(h)) \\
                               &= \text{Hom}(c, -)(g) \left( \text{Hom}(c, -)(f)(h)\right) \\
                               &= \left(\text{Hom}(c, -)(g) \circ \text{Hom}(c, -)(f) \right)(h)
\end{align*}
\end{itemize}
\end{definition}

Contravariant hom-functor: $\mathcal{C}^{\text{op}} \to \mathbf{Set}$ by *precomposing with $f$*, denoted $\text{Hom}(-, d)$.

\newpage

**Notation:**

- Covariant hom-functor $\text{Hom}(a, -) = h^a$ 
- Contravariant hom-functor $\text{Hom}(-, b) = h_b$.

**Yoneda Embedding**

\begin{definition}
Let $\mathcal{C}$ and $\mathcal{D}$ be categories, then $\mathbf{Fun}(\mathcal{C}, \mathcal{D})$ is a category with 
\begin{itemize}
\item \underline{Objects}: functors $\mathcal{C} \to \mathcal{D}$
\item \underline{Arrows}: natural transformations between these functors.
\end{itemize}
\end{definition}

$Y: \mathcal{C} \hookrightarrow \mathbf{Fun}(\mathcal{C}^{\text{op}}, \mathbf{Set})$.

- objects to functors 
- arrows to natural transformations

\underline{Use hom-functors!}

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
a \arrow[d, "f"'] \arrow[r, "Y"] & h^a \arrow[d, Leftarrow, "Yf"] \\
b \arrow[r, "Y"] & h^b
\end{tikzcd}
\end{figure}

The natural transformation $Yf$:

- components are arrows in **Set**, indexed by objects in $\mathcal{C}$.

Let $k: d \to c$ (note the reversed order), naturality square:

\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
\text{Hom}(a, c) \arrow[r, "(Yf)_c"] \arrow[d, "h^a(k)"'] & \text{Hom}(b, c)  \arrow[d, "h^b(k)"] \\
\text{Hom}(a, d) \arrow[r, "(Yf)_d"] & \text{Hom}(b, d)
\end{tikzcd}
\end{figure}

Again: *composition*!

\begin{definition}[Yoneda embedding]
Let $\mathcal{C}$ be a category. Let $a \in \mathcal{C}$ and $f: b \to c$ in $\mathcal{C}$. The \textbf{Yoneda functor} $Y: \mathcal{C}^{\text{op}} \to \mathbf{Fun}(\mathcal{C}, \mathbf{Set})$, is defined as follows: 
\begin{align*}
Ya &= h^a \\
Yf^{\text{op}} &: h^c \to h^b \\
(Yf^{\text{op}})_a&: \text{Hom}(c, a) \to \text{Hom}(b, a) \\
                  &: (g: c \to a) \mapsto (g \circ f: b \to a) \\
                  &= h_a f
\end{align*}
\end{definition}

Note $Y$ contravariant, while $(Yf)_a$ covariant.

\begin{figure}[H]
\centering
\begin{tikzcd}
\text{Hom}(a, c) \arrow[r, "(Yg^{\text{op}})_c"] \arrow[d, "h^a(k)"'] & \text{Hom}(b, c) \arrow[d, "h^b(k)"]\\
\text{Hom}(a, d) \arrow[r, "(Yg^{\text{op}})_d"] & \text{Hom}(b, d) 
\end{tikzcd}
\end{figure}

\begin{figure}[H]
\centering
\begin{tikzcd}
\ell \arrow[r, "(Yg^{\text{op}})_c"] \arrow[d, "h^a(k)"'] & \ell \circ g  \arrow[d, "h^b(k)"] \\
k \circ \ell \arrow[r, "(Yg^{\text{op}})_d"] & k \circ (\ell \circ g) = (k \circ \ell) \circ g
\end{tikzcd}
\end{figure}

\underline{Associativity (in $\mathcal{C}$).}

\begin{theorem}
The Yoneda functor $Y$ is \emph{full} and \emph{faithful}.
\label{thm:yon_full_faithful}
\end{theorem}

\begin{corollary}
Let $\mu: h^a \to h^b$ be a natural transformation between hom-functors, then it is given by composition with a unique arrow $f: b \to a$. Furthermore, $\mu$ is a (natural) isomorphism if and only if $f$ is an isomorphism.
\label{cor:natural_transformation_arrow}
\end{corollary}

Duality $\Rightarrow$ a full and faithful functor from $\mathcal{C} \to \mathbf{Fun}(\mathcal{C}^{\text{op}}, \mathbf{Set})$.

Proof: Yoneda's lemma.


\newpage

**The Yoneda Lemma**

What about natural transformations: $h^a \Rightarrow F$, where $F: \mathcal{C} \to \mathbf{Set}$.

- Components, $h^a b \to Fb$.
- \underline{Evaluation} of $Fg$: a map between the sets $Fa$ and $Fb$ at fixed $x \in F a$.

\begin{proposition}
Let $F: \mathcal{C} \to \mathbf{Set}$ be a functor, and $a \in \mathcal{C}$. Any element $x \in Fa$ induces a natural transformation from $h^A$ to $F$, by evaluating any lifted arrow in $x$.
\end{proposition}

\begin{proof}
We have to show this gives a natural transformation, i.e\ that the following diagram commutes:
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
F f (F g(x)) &= (F f \circ F g)(x) = F(f \circ g)(x)\\
&= (F \_ (x))(f \circ g) = (F \_ (x))((h^a f)(g))
\end{align*}
which is equal to taking it along the bottom, hence the diagram commutes.
\qedhere
\end{proof}

The Yoneda lemma states that *all natural transformations between $h^a$ and $F$ are of this form*:

$$\mathbf{Nat}(h^a, F) \simeq F a.$$

\newpage

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
Let us show that $\Phi$ and $\Psi$ are inverses of each other. First:
\begin{align*}
    \Psi(\Phi(x)) = \Psi(F \_ (x)) = F \text{id}_a (x) = \text{id}_{F a}(x) = x,
\end{align*}
hence $\Psi$ is a left inverse of $\Phi$. To show that it is also a right inverse, we need to show that:
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

\begin{proof}[proof of Theorem \ref{thm:yon_full_faithful}]
By Yoneda's Lemma there is a bijection between the sets:
$$\text{Nat}(h^b, h^a) \simeq h^a b = \text{Hom}(a, b)$$
for all objects $a$ and $b$ of $\mathcal{C}$, which directly implies that the functor $Y$ is full and faithful.
\qedhere
\end{proof}

**Recap:** 

- \underline{hom-functors}
- \underline{Yoneda embedding} of category into set-valued functors
- Any natural transformation between hom-functors is given by composition with arrows of $\mathcal{C}$
- \underline{Yoneda lemma}: bijection between $F a$ and natural transformations from $h^a$ to $F$,

All functors in Haskell are set-valued, since that is our category of interest.

\newpage

**Examples**

\begin{example}[Matrix row operations]
Let $\mathcal{C}$ be the category where:
\begin{itemize}
\item \underline{objects} $1, 2, 3, \ldots$
\item \underline{arrows} $n \to m$ correspond to $m \times n$ matrices.
\item Composition is given by matrix multiplication
\end{itemize}
indeed if we have arrows:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
n \arrow[r, "A_{m \times n}"] & m \arrow[r, "B_{k \times m}"] & k
\end{tikzcd}
\end{figure}
then the composite $B_{k \times m} A_{m \times n} = C_{k \times n}$ is an arrow from $n$ to $k$, as required.

Consider contravariant hom-functors $h_n$ for this category. The hom-set $h_n k = \text{Hom}(k, n)$ consists of $n \times k$ matrices. To show that row operations can be seen as natural transformations $\mu: h_n \Rightarrow h_n$, we fix some $k \times m$ matrix $B$, and look at the following naturality square:
\begin{figure}[H]
\centering
\begin{tikzcd}[sep=large]
h_n k \arrow[r, "\mu_k"] \arrow[d, "h_n B"'] & h_n k \arrow[d, "h_n B"]\\
h_n m \arrow[r, "\mu_m"] & h_n m
\end{tikzcd}
\end{figure}
Considering some $n \times k$ matrix $A$, the naturality condition states:
$$\mu(A) B \overset{?}{=} \mu(AB).$$
To show this, we observe that for all row transformations we have:
$$\mu(A) = A + \tilde{A}$$
where the rows of $\tilde{A}$ are either empty, or are multiples of rows of $A$, or:
$$\mu(A) = A + \Lambda A.$$
Where $\Lambda$ is a matrix whose elements $\Lambda_{ij}$ represent how many times row $j$ should be added to row $i$. This means we have
$$\mu(A) B = (A + \Lambda A) B = AB + \Lambda AB = \mu(AB).$$
as required. By Corollary \ref{cor:natural_transformation_arrow} we have that any natural transformation $\mu: h_n \Rightarrow h_n$ is given by postcomposition (in this category: left-multiplication) with a unique arrow $D: n \to n$. The Yoneda lemma allows us to identify this arrow; it is equal to:
$$D = \mu_n(\text{Id}_n),$$
so to perform row operations on a matrix, one can equivalently left multiply with a matrix obtained by applying these operations to the identity matrix.
\end{example}

**Yoneda in Haskell**

- *hom-sets*: the hom-set of types `a` and `b` are the arrows between `a` and `b`, i.e. functions of the type `(a -> b)`. Note that this hom-set is again in the category of types.
- The *hom-functor* corresponding to a type `a` should be a functor, i.e. a type constructor, that produces the hom-set `(a -> b)` when given a type `b`, for some fixed type `a`. On functions `(b -> c)` it should get a function between the hom-sets of `a` and `b`, and `a` and `c` respectively, i.e.:
```haskell
instance Functor (HomFunctor a) where
      fmap :: (b -> c) -> (a -> b) -> (a -> c)
      fmap f g = f . g
```
And indeed, we see that we can simply use composition.
- Yoneda's lemma says that for any other functor `F`, we can produce a natural transformation (i.e.\ polymorphic function in a type `b`) from the hom-functor for a fixed `a` by looking at elements of `F a`.

Next we look at a simple example of how to apply this final point in Haskell.

### Reverse engineering machines

Set `F` equal to `Id`, natural transformation:

```haskell
--    (HomFunctor a) b      Id b
--            |               |
machine :: (a -> b)     ->    b
```
How can `machine` be implemented? Know the function in a *single evaluation*.

Given uniquely by any element of `Id a = a`, i.e. some value of the type `a`. Can only be implemented as:
```haskell
machine :: (a -> b) -> b
machine f = f x
```
`x` is fixed value of type `a`. The Yoneda lemma also tells us a way to obtain `x`, we simply supply `f = id`:
```haskell
-- obtain the 'hidden element'
x <- machine id
```

What if `F` is not the identity function, but say the `List` functor. The story actually does not change much, we now have a function with the signature:
```haskell
--    (HomFunctor a) b      List b
--            |               |
machine :: (a -> b)     ->   [b]
```
the Yoneda lemma says that internally, any function of this signature should maintain a list of the type `[a]`, and when given a function `f :: a -> b` it fmaps this over the internal list to produce a value of the type `[b]`. Again, we can get this list by feeding the `id` function into the machine.


