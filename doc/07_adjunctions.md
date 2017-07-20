# Adjunctions

\epigraph{"Adjoint functors arise everywhere"}{\emph{Saunders Mac Lane}}

There are multiple ways to introduce adjunctions, both in terms of the intuition behind them, as well as the actual definition. The setup is that there are two functors $F, G$:

\begin{figure}[H]
\centering
\begin{tikzcd}
\mathcal{C} \arrow[r, "F", bend left=30] & \mathcal{D} \arrow[l, "G", bend left=30]
\end{tikzcd}
\end{figure}

that we want to relate. In particular, we want to generalize the _inverse_ of a functor. We say that the functor $F$ is an isomorphism with inverse $G$ if:
$$\text{Id}_\mathcal{C} = GF,~FG = \text{Id}_\mathcal{D}$$
where $\text{Id}_\mathcal{C}$ is the identity functor on $\mathcal{C}$, and $GF$ denotes $G \circ F$. A weaker notion is _isomorphism up to natural isomorphism_, where we require that there exists some natural isomorphisms
$$\text{Id}_\mathcal{C} \stackrel{\sim}{\Rightarrow} GF,~FG \stackrel{\sim}{\Rightarrow} \text{Id}_\mathcal{D}$$
Even weaker is that we only require that there exists natural transformations:
$$\text{Id}_\mathcal{C} \Rightarrow GF,~FG \Rightarrow \text{Id}_\mathcal{D}$$
This is what we are going to explore in this part.

## Universal arrow adjunctions

\begin{definition}[Universal arrow adjunction]
Let $\mathcal{C}, \mathcal{D}$ be categories. Let $F: \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$ be functors.
If there exists a natural transformation:
$$\eta: \text{Id}_\mathcal{C} \Rightarrow GF,$$
such that for all objects $c \in \mathcal{C}$ and $d \in \mathcal{D}$, and all arrows $f: c \to Gd$ there exists a unique arrow $g: Fc \to d$ such that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
c \arrow[r, "\eta_c"] \arrow[rd, "f"']  & GFc \arrow[d, "Gg"] \\
& Gd
\end{tikzcd}
\end{figure}
We call the triple $(F, G, \eta)$ an \emph{adjunction}, and $\eta$ the \emph{unit} of the adjunction. We say that $F \text{ is left adjoint to } G$,  and $G \text{ is right adjoint to } F$, or simply $F \dashv G$.
\end{definition}

In other words, given an adjunction and any arrow $f: c \to Gd$, i.e. from an arbitrary object of $\mathcal{C}$ to something in the image of $G$ (so _relevant to the functor $G$_), we can equivalently consider an arrow $g: Fc \to d$ in $\mathcal{D}$ relating to the functor $F$, because we use the natural transformation $\eta$ and our functors to convert them to the same arrow.

This means that the _relevant structure_ of $\mathcal{C}$ with respect to the functor $G$, can also be found in $\mathcal{D}$ with respect to the functor $F$.

\begin{example}
View $\mathbb{Z}$ and $\mathbb{R}$ as categories, with $a \to b \iff a \leq b$. Let $I: \mathbb{Z} \to \mathbb{R}$ be the inclusion functor that sends $z \to \iota(z)$. $I$ is left adjoint to the functor $\lfloor \cdot \rfloor: \mathbb{R} \to \mathbb{Z}$ that sends $r \to \lfloor r \rfloor$. Indeed, consider the following diagram in $\mathbb{Z}$:
\begin{figure}[H]
\centering
\begin{tikzcd}
z \arrow[r, "z \leq z"] \arrow[rd, "z \leq \lfloor r \rfloor"'] & \lfloor \iota(z) \rfloor = z \arrow[d, "G(\iota(z) \leq r)"] \\
& \lfloor r \rfloor
\end{tikzcd}
\end{figure}
the existence of a unique $g = \iota(z) \leq r$ for such an $f$ corresponds to the statement:
$$ \iota(z) \leq r \iff z \leq \lfloor r \rfloor.$$
For the converse, consider the ceiling functor $\lceil \cdot \rceil : \mathbb{R} \to \mathbb{Z}$ and the following diagram in $\mathbb{R}$:
\begin{figure}[H]
\centering
\begin{tikzcd}
r \arrow[r, "r \leq \iota(\lceil r \rceil)"] \arrow[rd, "r \leq \iota(z)"'] & \iota(\lceil r \rceil) \arrow[d, "\iota(\lceil r \rceil \leq \iota(z))"] \\
& \iota(z)
\end{tikzcd}
\end{figure}
Which corresponds to the statement:
$$r \leq \iota(z) \iff \lceil r \rceil \leq z,$$
showing that the inclusion functor is right adjoint to the ceil functor. So we have the adjunction chain:
$$\lceil \cdot \rceil \dashv I \dashv \lfloor \cdot \rfloor.$$
\end{example}

\begin{example}
An important class of adjunctions take the form $\textbf{free} \dashv \textbf{forgetful}$.
Let $X$ be a set. The free monoid $F(X)$ is defined as:
$$F(X) = (X^*, \concat, ()),$$
see Example \ref{exa:kleene-closure} for the definition of $X^*$, $\concat$ denotes the concatenation of words as a binary operator, and $()$ denotes the empty word. $F$ defines a \emph{free functor}:
$$F: \mathbf{Set} \to \mathbf{Mon},$$
sending a set to the free monoid over that set. There is also a \emph{forgetful functor}:
$$U: \mathbf{Mon} \to \mathbf{Set},$$
sending a monoid to its underlying set, that sends monoid homomorphisms to the corresponding function on sets. We define:
$$\eta: \text{Id}_{\mathbf{Set}} \Rightarrow U \circ F,$$
as having components defining a function that sends an element $x \in X$ to a singleton word containing that element:
$$\eta_X(x) = (x).$$
To show that $(F, U, \eta)$ form an adjunction, we consider some $f: X \to U(M)$ where $M$ is a monoid, and we want to show that there is a unique monoid homomorphism $g: F(X) \to M$ that makes the following diagram commute:
\begin{figure}[H]
\centering
\begin{tikzcd}
X \arrow[r, "\eta_X"] \arrow[rd, "f"']  & U(F(X)) \arrow[d, "U(g)"] \\
& U(M)
\end{tikzcd}
\end{figure}
We have to define:
\begin{align*}
g(()) &= \text{id}_M \\
g((x)) &= f(x)\\
g((x_1, x_2, \ldots, x_n)) &= f(x_1) f(x_2) \ldots f(x_n)
\end{align*}
to make $g$ into a monoid homomorphism that satisfies also:
$$f(x) = U(g)(\eta_X x) = U(g)((x)).$$
\end{example}

Before moving on, we first show that there are other definitions of adjunctions, which we will show are equivalent to the one we gave above, but are useful for describing other examples of adjunctions.

## Equivalent formulations

There is an alternative way of describing adjunctions, as a natural bijection between hom-sets.

\begin{definition}[Hom-set adjunctions]
Let $\mathcal{C}, \mathcal{D}$ be categories. Let $F: \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$ be functors. If there is a natural bijection:
$$\text{Hom}_{\mathcal{D}}(Fc, d) \stackrel{\phi_{c,d}}{\longrightarrow} \text{Hom}_{\mathcal{C}}(c, Gd),$$
for each $c \in \mathcal{C}$ and $d \in \mathcal{D}$, then $(F, G, \{ \phi_{c, d} \}_{c \in \mathcal{C}, d \in \mathcal{D}})$ is an \emph{adjunction}. Here, the bijection should be natural in both $c$ and $d$, where in $\mathcal{D}$ we have that for all $g: d \to d'$ in $\mathcal{D}$ the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
\text{Hom}_\mathcal{D}(Fc, d) \arrow[d, "g \circ \_"'] \arrow[r, "\phi_{c, d}"] & \text{Hom}_\mathcal{C}(c, Gd)  \arrow[d, "Gg \circ \_"] \\
\text{Hom}_\mathcal{C}(Fc, d') \arrow[r, "\phi_{c, d'}"] & \text{Hom}_\mathcal{C}(c, Gd')
\end{tikzcd}
\end{figure}
while naturality in $\mathcal{C}$ means that for all $f: c' \to c$ the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
\text{Hom}_\mathcal{D}(Fc, d) \arrow[d, "\_ \circ Ff"'] \arrow[r, "\phi_{c, d}"] & \text{Hom}_\mathcal{C}(c, Gd)  \arrow[d, "\_ \circ f"] \\
\text{Hom}_\mathcal{C}(Fc', d) \arrow[r, "\phi_{c', d}"] & \text{Hom}_\mathcal{C}(c', Gd)
\end{tikzcd}
\end{figure}
\end{definition}

We can show that given a universal arrow adjunction, we can obtain a hom-set adjunction.

\begin{proposition}
Let $(F, G, \eta)$ be a univeral arrow adjunction. Then the family of functions:
\begin{align*}
\phi_{c, d}:~&\text{Hom}_\mathcal{D}(Fc, d) \to \text{Hom}_\mathcal{C}(c, Gd), \\
             & (\alpha: Fc \to d) \mapsto G \alpha \circ \eta_c
\end{align*}
defines a hom-set adjunction $(F, G, \{ \phi_{c, d} \}_{c \in \mathcal{C}, d \in \mathcal{D}})$.
\end{proposition}

\begin{proof}
First we show that $\phi_{c, d}$ is a bijection. Because $(F, G, \eta)$ is an adjunction, we know that:
$$\forall f: c \to Gd, \exists !~g: Fc \to d, \text{ s.t. } f = Gg \circ \eta_c.$$
Injectivity of $\phi_{c, d}$ is guaranteed by the uniqueness of the arrow $g$, while surjectivity is guaranteed by the existence of such an arrow.

Next we have to show that it is natural in both $\mathcal{C}$, and $\mathcal{D}$ which means respectively that for all $f: c' \to c$ and $g: d \to d'$:
\begin{align}
G\alpha \circ \eta_c \circ f &= G(\alpha \circ Ff) \circ \eta_{c'} \label{eqn:natural-in-c}\\
Gg \circ G\alpha \circ \eta_c &= G(g \circ \alpha) \circ \eta_c \label{eqn:natural-in-d}
\end{align}
Equation \ref{eqn:natural-in-c} follows from the functoriality of $G$ and the naturality of $\eta$:
$$G(\alpha \circ Ff) \circ \eta_{c'} = G(\alpha) \circ G(Ff) \circ \eta_{c'} = G(\alpha) \circ \eta_c \circ f.$$
Equation \ref{eqn:natural-in-d} follows directly from the functoriality of $G$.
\end{proof}

\begin{definition}[Unit-counit adjunctions]
Let $\mathcal{C}, \mathcal{D}$ be categories. Let $F: \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$ be functors. If there are natural transformations:
$$\eta: \text{Id}_\mathcal{C} \Rightarrow GF,~\epsilon: FG \Rightarrow \text{Id}_\mathcal{D},$$
such that the following diagrams (the \emph{triangle identities}) commute:

\begin{figure}[H]
\centering
\begin{tikzcd}
F \arrow[r, Rightarrow, "F\eta"] \arrow[dr, Rightarrow, "\text{id}_F"'] & FGF \arrow[d, Rightarrow, "\epsilon F"] \\
  & F
\end{tikzcd}
\end{figure}

\begin{figure}[H]
\centering
\begin{tikzcd}
G \arrow[r, Rightarrow, "\eta G"] \arrow[dr, Rightarrow, "\text{id}_G"'] & GFG \arrow[d, Rightarrow, "G \epsilon"] \\
  & G
\end{tikzcd}
\end{figure}

where we use the notation (now in components) $(\eta G)_d = \eta_{Gd}$ and $(F \eta)_c = F(\eta_c)$, then $(F, G, \eta, \epsilon)$ is an \emph{adjunction}. We call $\eta$ the \emph{unit} and $\epsilon$ the \emph{counit} of the adjunction.
\end{definition}

Note that this means that the unit is the \emph{translated inverse} of the counit and vice versa.

\begin{proposition}
We can construct a unit-counit adjunction $(F, G, \eta, \epsilon)$ from a hom-set adjunction.
\end{proposition}

\begin{proof}
We define $\eta$ and $\epsilon$ as having components:
\begin{align}
\eta_c: c \to GFc &= \phi_{c, Fc}(\text{id}_{Fc}) \label{eqn:unit-from-hom}\\
\epsilon_d: FGd \to d &= \phi^{-1}_{Gd, d}(\text{id}_{Gd}) \label{eqn:counit-from-hom}
\end{align}
Let us prove that $\eta$ is a natural transformation, the proof of the naturality of $\epsilon$ is dual to this. We want to show that the following diagram commutes for all $f: c \to c'$:
\begin{figure}[H]
\centering
\begin{tikzcd}
c \arrow[d, "f"] \arrow[r, "\eta_c"] & GFc \arrow[d, "GFf"] \\
c'\arrow[r, "\eta_{c'}"] & GFc'
\end{tikzcd}
\end{figure}

i.e. that:
$$\alongtop = GF f \circ \eta_c = \eta_{c'} \circ f = \alongbottom$$
Plugging in our definition for $\eta_c$, and using the naturality of $\phi_{c, d}$ we see:
\begin{align*}
GFf \circ \phi_{c, Fc}(\text{id}_{Fc}) &= \phi_{c, Fc'} (Ff \circ \text{id}_{Fc}) \\
&=  \phi_{c, Fc'} (\text{id}_{Fc'} \circ Ff) \\
&=  \phi_{c', Fc'} (\text{id}_{Fc'}) \circ f = \eta_{c'} \circ f
\end{align*}
To show the first triangle identity, i.e. that for all $c \in \mathcal{C}$:
$$\epsilon_{Fc} \circ F(\eta_c) = \text{id}_{Fc},$$
we use naturality of $\phi_{GFc, Fc}^{-1}$:
\begin{align*}
\phi^{-1}_{GFc, Fc}(\text{id}_{GFc}) \circ F(\phi_{c, Fc}(\text{id}_{Fc})) &= \phi^{-1}_{c, Fc} (\text{id}_{GFc} \circ \phi_{c, Fc}(\text{id}_{Fc})) \\
&= \phi^{-1}_{c, Fc} (\phi_{c, Fc}(\text{id}_{Fc})) = \text{id}_{Fc}
\end{align*}
For the second triangle identity, i.e. for all $d \in \mathcal{D}$:
$$G(\epsilon_d) \circ \eta_{Gd} = \text{id}_{Gd},$$
we use the naturality of $\phi_{Gd, FGd}$:
\begin{align*}
G(\phi^{-1}_{Gd, d}(\text{id}_Gd)) \circ \phi_{Gd, FGd}(\text{id}_{FGd}) &= \phi_{Gd, d}(\phi^{-1}_{Gb, b} (\text{id}_{Gb}) \circ \text{id}_{FGb}) \\
 &= \phi_{Gd, d}(\phi^{-1}_{Gb, b} (\text{id}_{Gb})) = \text{id}_{Gb}
\end{align*}

\qedhere
\end{proof}

To complete the cycle of equalities, we show that we can retrieve our original universal arrow adjunction from the unit-counit adjunction.

\begin{proposition}
Let $(F, G, \eta, \epsilon)$ be a unit-counit adjunction. Then $(F, G, \eta)$ forms a universal arrow adjunction.
\end{proposition}

\begin{proof}
Let $f: c \to Gd$. We need to show that there is a unique solution to the equation $G(?) \circ \eta_c = f$.

From the second triangle identity, naturality of $\eta$, and functorality of $G$, we have:
\begin{align*}
G(\epsilon_d) \circ \eta_{Gd} &= \text{id}_{Gd} \\
G(\epsilon_d) \circ \eta_{Gd} \circ f &= f \\
G(\epsilon_d) \circ GF f \circ \eta_{c} &= f \\
G(\epsilon_d \circ F f) \circ \eta_{c} &= f
\end{align*}
So that the required $g \equiv \epsilon_d \circ F f: F c \rightarrow d$ exists. To show that it is unique, let:
\begin{align*}
f &= G(g) \circ \eta_c \\
Ff &= FG(g) \circ F\eta_c \\
\epsilon_d \circ Ff &= \epsilon_d \circ FG(g) \circ F\eta_c \\
\epsilon_d \circ Ff &= g \circ \epsilon_{Fd} \circ F\eta_c \\
\epsilon_d \circ Ff &= g \circ \text{id}_{Fc} \\
\epsilon_d \circ Ff &= g
\end{align*}
So $g$ must be of this form, as required.
\qedhere
\end{proof}

Summarizing what we saw so far, adjunctions can be defined either as:

1. _Universal arrow adjunction_: As a triple $(F, G, \eta)$ together with a universal mapping property.
2. _Hom-set adjunction_: As a natural bijection between hom-sets
3. _Unit-counit adjunction_: As $(F, G, \eta, \epsilon)$ satisfying the triangle identities.

And we showed $1 \implies 2 \implies 3 \implies 1$, meaning that all these definitions are equivalent.

## Uniqueness of adjoints

You can show that adjoints are unique up to natural isomorphism. Say $F, F': \mathcal{C} \to \mathcal{D}$ and $G: \mathcal{D} \to \mathcal{C}$. Assume $F \dashv G$ and $F' \dashv G$, with natural bijections $\phi_{c, d}$ and $\phi'_{c, d}$ respectively. Then we have for all $c \in \mathcal{C}$:
$$\text{Hom}_\mathcal{D}(Fc, -) \simeq \text{Hom}_\mathcal{C}(c, G-) \simeq \text{Hom}_\mathcal{D}(F'c, -),$$
through natural isomorphisms in $\mathbf{Set}$ defined by $\phi_{c, -}$ and $\phi'_{c, -}$ respectively, by composing them we obtain:
$$\text{Hom}_\mathcal{D}(Fc, -) \simeq \text{Hom}_\mathcal{D}(F'c, -),$$
but the Yoneda embedding then says that $Fc$ and $F'c$ are isomorphic (see Corollary \ref{cor:natural_transformation_arrow}). To show that these isomorphisms $Fc \to Fc'$ define the components of a natural isomorphism $F \Rightarrow F'$ we have to show that the following diagram commutes:

\begin{figure}[H]
\centering
\begin{tikzcd}
Fc \arrow[d, "Ff"'] \arrow[r, "\simeq"] & F'c \arrow[d, "F'f"]\\
Fc' \arrow[r, "\simeq"'] & F'c'
\end{tikzcd}
\end{figure}
Because the Hom-functor $\text{Hom}_\mathcal{C}(-, d)$ is faithful, the above diagram commutes if\footnote{You can prove that faithful functors reflect commutative diagrams, by showing that it preserves non-commutative diagrams}:
\begin{figure}[H]
\centering
\begin{tikzcd}
\text{Hom}(d, Fc) \arrow[d, "h^d(Ff)"'] \arrow[r, "\simeq"] & \text{Hom}(d, F'c) \arrow[d, "h^d(F'f)"]\\
\text{Hom}(d, Fc') \arrow[r, "\simeq"'] & \text{Hom}(d, F'c')
\end{tikzcd}
\end{figure}
which commutes by the naturality of $\phi_{c, d}$ (in $\mathcal{D}$).

We conclude that adjoints are unique up to natural isomorphism.

## Examples

\begin{example}
The exponential object of a CCC is described by an adjunction.

Consider the functor:
\begin{align*}
- \times c:~&\mathcal{C} \to \mathcal{C},\\
&a \mapsto a \times c,\\
&f: a \to b \mapsto f \times \text{id}_c.
\end{align*}
Here, $f \times \text{id}_c$ is the unique arrow from $a \times c \to b \times c$ that makes the following diagram commute:
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[dd, "f"'] & a \times c \arrow[l, "p_1"'] \arrow[r, "p_2"] \arrow[d, dashed, "f \times \text{id}_c"] & c \arrow[dd, "\text{id}_c"] \\
  & b \times c \arrow[dl, "p_1'"'] \arrow[dr, "p_2'"]&   \\
b &            & c
\end{tikzcd}
\end{figure}
If $- \times c$ has a right adjoint, which we will suggestively denote:
$$(- \times c) \dashv (c \to -),$$
then for this adjunction, the universal property in Exercise \ref{exc:universal-mapping-property-counit} states:

For any $g: a \times c \to b$ there exists a unique arrow $f \equiv \lambda g : a \to (c \to b)$ such that the following diagram commutes:

\begin{figure}[H]
\centering
\begin{tikzcd}
b  & (c \to b) \times c \arrow[l, "\epsilon_b"'] \\
& \arrow[lu, "g"] \arrow[u, "\lambda g \times \text{id}_c"'] a \times c
\end{tikzcd}
\end{figure}
so that the universal property for the counit is identical to the universal property of the evaluation function, compare also with Definition \ref{def:ccc} of a CCC. Since adjoints are essentially unique, the exponential is determined by the adjunction.

You can show that adjunctions preserve (among other constructions involving universal properties) initial objects, terminal objects and products, which can be used to prove many useful and familiar equalities in a CCC. For example, we have $R_a(b \times c) \simeq R_a(b) \times R_a(c)$ which in the notation $a \to b \equiv b^a$ says:
$$(b \times c)^a \simeq b^a \times c^a.$$
Conversely, the product functor preserves coproducts, in that $(- \times c)(a + b) \simeq (- \times c)a + (- \times c)b$, or:
$$(a + b) \times c \simeq (a \times c) + (b \times c),$$
which shows that CCC's are distributative.
\end{example}

Other examples:

- Free/forgetful functor pairs.
- Groups $G$ and their abelianizations $G^{ab} \equiv G / [G, G]$ form an adjunction.
- As an interesting application that we will see shortly, adjunctions also give rise to monads.

## Exercises

\begin{exercise}
Argue using duality that the counit satisfies the following universal mapping property:

For any $g: Fc \to d$ there is a unique arrow $f: c \to Gd$ such that the following diagram commutes:

\begin{figure}[H]
\centering
\begin{tikzcd}
d  & FGd \arrow[l, "\epsilon_d"'] \\
& \arrow[lu, "g"] \arrow[u, "Ff"'] Fc
\end{tikzcd}
\end{figure}

\label{exc:universal-mapping-property-counit}
\end{exercise}

\begin{exercise}
Let $\Delta: \mathcal{C} \to \mathcal{C} \times \mathcal{C}$ be the \emph{diagonal functor} defined as:
\begin{align*}
\Delta a &= (a, a) \\
\Delta (f: a \to b) &= (f, f) : (a, a) \to (b, b)
\end{align*}
Show that the category $\mathcal{C}$ has binary products if and only if $\Delta$ has a right adjoint $\Pi$. Here, the functor $\Pi: \mathcal{C} \times \mathcal{C} \to \mathcal{C}$ should send $(a, b) \mapsto a \times b$.

\emph{Hint:} write the components of the counit and the arrows that arise in the universal arrow property of the counit (see Exercise \ref{exc:universal-mapping-property-counit}), in terms components of $\mathcal{C} \times \mathcal{C}$, i.e. $\epsilon_d = (p_1, p_2)$, $f = (q_1, q_2)$.

Use that a diagram in $\mathcal{C} \times \mathcal{C}$ commutes if and only if the diagrams for each component commute, and show that you obtain the definition for the binary product.
\end{exercise}

\begin{exercise}
Although we proved almost everything equationally in this part, some parts can be proved more efficiently using the Yoneda lemma, for example we consider the natural bijection in the definition of a hom-set adjunction as a natural transformation between the hom-functors:
$$\text{Hom}(F-, -) \Rightarrow \text{Hom}(-, G-)$$
from $\mathcal{C}^{\text{op}} \times \mathcal{D} \to \mathbf{Set}$. Think about this.
\end{exercise}

## References

- <https://www.youtube.com/watch?v=K8f19pXB3ts>
- Chapter 13 of Barr and Wells
- Chapter 4 of Riehl
- Chapter 4 of Mac Lane


