# Recursion and F-algebras

In this part we introduce $F$-algebras, which are not only an important _theoretical_ tool for studying recursion (for functions and data types), but as we will see can also be applied in functional programming to obtain modular descriptions of recursive functions, allowing us to perform multiple transformations over data structures in a single pass, as well as decoupling the _recursion scheme_ from the actual transformation or computation performed at every level.

## Algebras for endofunctors

\begin{definition}
Let $F: \mathcal{C} \to \mathcal{C}$ be an endofunctor. An \textbf{$F$-algebra} is a pair $(a, \alpha)$ where $a \in \mathcal{C}$ and $\alpha: F a \to a$ is an arrow in $\mathcal{C}$. The object $a$ is called the \emph{carrier} of the algebra.

A homomorphism between $F$-algebras $(a, \alpha)$ and $(b, \beta)$ is an arrow $h: a \to b$ such that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
Fa \arrow[r, "\alpha"]  \arrow[d, "Fh"'] & a \arrow[d, "h"]\\
Fb \arrow[r, "\beta"'] & b
\end{tikzcd}
\end{figure}
\end{definition}

For every endofunctor $F$, the collection of $F$-algebras together with homomorphisms of these $F$-algebras form a category which we will denote $\mathbf{Alg}_F$.

Recall that a fixed point of a function $f$ is an $x$ in the domain of $f$, such that $f(x) = x$. Considering this, a sensible definition for a fixed point of an endofunctor would be an object $a$ such that $F(a) = a$, but we will be a bit more lenient, and only require that $F(a) \simeq a$.

\begin{definition}
A \textbf{fixed point} of $F$ is an algebra $(a, \alpha)$ for which $\alpha$ is an isomorphism.
\end{definition}

A special fixed point is the  _least fixed point_. We take inspiration from partially ordered sets, where the least point is an initial object, and define it as follows.

\begin{definition}
A \textbf{least fixed point} of $F$ is an initial algebra $(a, \alpha)$, i.e. an algebra that is an initial object in the category $\mathbf{Alg}_F$.
\end{definition}

An immediate issue that we have to resolve is to show that any _least_ fixed point is indeed a fixed point.

\begin{lemma}[Lambek]
Let $F: \mathcal{C} \to \mathcal{C}$ be an endofunctor. If $(t, \tau)$ is initial in $\mathbf{Alg}_F$, then $\tau$ is an isomorphism.
\end{lemma}

\begin{proof}
Let $(t, \tau)$ be an initial object in $\mathbf{Alg}_F$, and consider the algebra $(Ft, F\tau)$. Since $(t, \tau)$ is initial there is a unique homomorphism $h: t \to Ft$ such that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
Ft \arrow[r, "\tau"]  \arrow[d, "Fh"'] & t \arrow[d, "h"]\\
F^2 t \arrow[d, "F\tau"'] \arrow[r, "F \tau"] & Ft \arrow[d, "\tau"] \\
Ft \arrow[r, "\tau"'] & t
\end{tikzcd}
\end{figure}
Here, the top square commutes because $h$ is a homomorphism, and the bottom square commutes trivially.
First, we note that by commutativity of this diagram, $\tau \circ h$ is a homomorphism between $(t, \tau) \to (t, \tau)$, and since $(t, \tau)$ is initial it is the unique homomorphism, i.e. the identity, and hence:
$$\tau \circ h = \text{id}_{t},$$
i.e. $h$ is a right inverse to $\tau$. To show that it is also a left inverse (and hence that $\tau$ is an isomorphism) we compute using the commutativity of the top square:
$$h \circ \tau = F\tau \circ Fh = F(\tau \circ h) = F(\text{id}_{t}) = \text{id}_{Ft}.$$
which shows that $\tau$ is an isomorphism, and hence that $(t, \tau)$ is a fixed point.
\end{proof}

Let $(a, \alpha)$ be an $F$-algebra. In the functional programming literature, the unique homomorphism from the initial algebra $(t, \tau)$ to $(a, \alpha)$ is called a _catamorphism_ and is denoted $\lbanana \alpha \rbanana$.

The following result is a useful tool when working with catamorphisms.

\begin{proposition}[Fusion law]
Let $F: \mathcal{C} \to \mathcal{C}$ be such that it has an initial algebra. Let $(a, \alpha)$ and $(b, \beta)$ be $F$-algebras and let $h$ be an algebra homomorphism between them. Then:
$$h \circ \lbanana \alpha \rbanana = \lbanana \beta \rbanana.$$
\end{proposition}

\begin{proof}
This is immediate from the following commutative diagram:
\begin{figure}[H]
\centering
\begin{tikzcd}
Ft \arrow[r, "\tau"] \arrow[d, "F \lbanana \alpha \rbanana"'] & t \arrow[d, "\lbanana \alpha \rbanana"] \\
Fa \arrow[r, "\alpha"] \arrow[d, "F h"'] & a \arrow[d, "h"] \\
Fb \arrow[r, "\beta"'] & b \\
\end{tikzcd}
\end{figure}
Note that $h \circ \lbanana \alpha \rbanana$ is a homomorphism, and since $(t, \tau)$ is initial it should correspond to the unique homomorphism $\lbanana \beta \rbanana$.
\end{proof}

The following examples are from 'Bart Jacobs, Jan Rutten; A tutorial on (co)algebras and (co)induction'.

\begin{example}[Peano numbers]
Consider the endofunctor on $\mathbf{Set}$ given by:
\begin{align*}
F(X) = 1 + X.
\end{align*}
We can construct an algebra of this functor with carrier $\mathbb{N}_{\geq 0}$, the set of natural numbers, as follows:
\begin{align*}
\nu &: F(\mathbb{N}) \to \mathbb{N} \equiv 1 + \mathbb{N} \to \mathbb{N}, \\
\nu &\equiv 0 \sqcup s
\end{align*}
here $s(n) \equiv n + 1$ denotes a successor function, and $0$ denotes the constant function to $0 \in \mathbb{N}$.

If $f: a \to c$, $g: b \to c$, then the notation $f \sqcup g: a + b \to c$ denotes the unique arrow that factors the arrows $f, g$.

We will show that $(\mathbb{N}, \nu)$ is in fact the initial algebra for $F$. To this end, let $(A, \alpha)$ be any other $F$-algebra, where $\alpha = a \sqcup h$ for some $a \in A$, and $h: A \to A$. We define the candidate homomorphism $\lbanana \alpha \rbanana$ between $(\mathbb{N}, \nu)$ and $(A, \alpha)$ to be:
$$\lbanana \alpha \rbanana(n) \equiv h^{n} (a),$$
i.e. $\lbanana \alpha \rbanana(0) = a, \lbanana \alpha \rbanana(1) = h(a)$ and so on.

We have to show that 1) it is indeed a homomorphism of $F$-algebras, and 2) that it is the unique such homomorphism.

For the former, we show that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
1 + \mathbb{N} \arrow[d, "\text{id}_* \sqcup \lbanana \alpha \rbanana"'] \arrow[r, "\nu"] & \mathbb{N} \arrow[d, "\lbanana \alpha \rbanana"] \\
1 + A \arrow[r, "\alpha"'] & A
\end{tikzcd}
\end{figure}
We do this directly, by chasing an element from the top left. We consider an $x \in 1 + \mathbb{N}$. There are two cases, either $x$ is in $1$, for which we will write $x = *$, or in $\mathbb{N}$, for which we will write $x = n$. If $x = *$ then:
\begin{align*}
\alongtop &= \lbanana \alpha \rbanana ( \nu(*) ) = \lbanana \alpha \rbanana(0) = a,\\
\alongbottom &= \alpha (\text{id}_*(*)) = \alpha(*) = a,
\end{align*}
as required. If $x = n$ then:
\begin{align*}
\alongtop &= \lbanana \alpha \rbanana ( \nu(n) ) = \lbanana \alpha \rbanana(n + 1) = h^{n + 1}(a),\\
\alongbottom &= \alpha (\lbanana \alpha \rbanana(n)) = \alpha (h^n(a)) = h^{n + 1}(a),
\end{align*}
such that $\lbanana \alpha \rbanana$ is indeed a homomorphism. Letting $g$ be an arbitrary homomorphism, then following the previous argument in reverse shows that $g \equiv \lbanana \alpha \rbanana$ such that it is unique, and $(\mathbb{N}, \nu)$ is indeed initial.
\end{example}

\begin{example}[Lists]
Next, we consider the endofunctor on $\mathbf{Set}$ given by:
$$F(X) = 1 + (A \times X).$$
We consider the \emph{list algebra} $(A^*, () \sqcup (\frown))$. Here, $A^*$ is the Kleene closure introduced in Example \ref{exa:kleene-closure}, () denotes the (constant function to) the empty list, and $(\frown): A \times A^* \to A$, the \emph{prepend} function, (which we will write using infix notation) is defined as:
$$a \frown (a_1, a_2, a_3, \ldots) = (a, a_1, a_2, a_3, \ldots).$$
Let $(B, \beta)$, with $\beta = b \sqcup h$ where $b \in B$ and $h: A \times B \to B$, be any other $F$-algebra, and define the candidate homomorphism:
$$\lbanana \beta \rbanana : A^* \to B,$$
between the list algebra and this algebra as:
$$\lbanana \beta \rbanana(\tilde{x}) = \begin{cases}
b & \text{ if } \tilde{x} = () \\
h(x, \lbanana \beta \rbanana(\tilde{y})) & \text{ if } \tilde{x} = x \frown \tilde{y} \\
\end{cases}$$
To show that this indeed a homomorphism, we look at the diagram:
\begin{figure}[H]
\centering
\begin{tikzcd}
1 + (A \times A^*) \arrow[d, "\text{id}_* \sqcup (\text{id}_A \times \lbanana \beta \rbanana)"'] \arrow[r, "() \sqcup (\frown)"] & A^* \arrow[d, "\lbanana \beta \rbanana"] \\
1 + (A \times B) \arrow[r, "b \sqcup h"'] & B
\end{tikzcd}
\end{figure}
Like in the previous example, we split in two cases. First, let $x = *$, then:
\begin{align*}
\alongtop &= \lbanana \beta \rbanana(()(*)) = \lbanana \beta \rbanana(()) = b, \\
\alongbottom &= b (\text{id}_*(*)) = b.
\end{align*}
Next, we let $x = a \times \tilde{a}$, and compute:
\begin{align*}
\alongtop &= \lbanana \beta \rbanana((\frown) (a \times \tilde{a})) = \lbanana \beta \rbanana(a \frown \tilde{a}) = h(a \times \lbanana \beta \rbanana(\tilde{a})), \\
\alongbottom &= h ((\text{id}_A \times \lbanana \beta \rbanana)(a \times \tilde{a})) = h(a \times \lbanana \beta \rbanana(\tilde{a})),
\end{align*}
as required. To show that it is the unique such arrow, we again follow the previous argument in reverse.
\label{exa:list_initial_algebra}
\end{example}

Note in both of these examples, the catamorphisms correspond to the usual notions of folds (see the Haskell exercises). It is in this sense, that catamorphisms are a generalization of folds (and thus lead to a specific recursion scheme).

When does a least fixed point exist? Lambek's theorem implies that e.g. the $\mathcal{P}$ power-set endofunctor does not have an initial algebra because $\mathcal{P}(X)$ is never isomorphic to $X$ (Cantor's theorem).

Before we can state a sufficient condition for the existence of initial algebras, we first have to introduce the categorical notion of limits.

## Limits

We have already come across an example of a limit, namely the categorical (binary) product. Recall that a product $a \times b$ has a universal property of the form _for any other object $c$ with morphisms to $a$ and $b$, we can factor the morphisms through $a \times b$_. This turns out to be a very general pattern that will define a limit. First, we will give two more examples of limits before stating the formal definition. We will follow roughly the discussion in Chapter 5 of Leister.

\begin{definition}[Fork and equalizer]
A \textbf{fork} from $a$ at $x$ to $y$ in a category $\mathcal{C}$ is defined by the data:
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[r, "f"] & x \arrow[r, shift left, "s"] \arrow[r, shift right, "t"'] & y
\end{tikzcd}
\end{figure}
such that $s \circ f = t \circ f$.

Given a diagram in $\mathcal{C}$ of the form:
\begin{figure}[H]
\centering
\begin{tikzcd}
x \arrow[r, shift left, "s"] \arrow[r, shift right, "t"'] & y
\end{tikzcd}
\end{figure}
An \textbf{equalizer} of $s$ and $t$ is an object $e \in \mathcal{C}$ and a map $i: e \to x$ such that:
\begin{figure}[H]
\centering
\begin{tikzcd}
e \arrow[r, "i"] & x \arrow[r, shift left, "s"] \arrow[r, shift right, "t"'] & y
\end{tikzcd}
\end{figure}
is a fork, and for each other fork from some $a$ at $x$ to $y$, there exists a unique map $\bar{f}$ such that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[d, "\bar{f}"'] \arrow[rd, "f"] \\
e \arrow[r, "i"] & x
\end{tikzcd}
\end{figure}
\end{definition}

For example, in $\mathbf{Set}$ the equalizer $E$ of two functions $s, t: X \to Y$ is given by:
$$E = \{ x \in X~|~s(x) = t(x) \}.$$
with $i$ the inclusion $E \hookrightarrow X$. It is easy to see that for all forks from some $A$ at $X$ to $Y$, that we must have $A \subseteq E$, such that $\bar{f}$ is simply the inclusion $A \hookrightarrow E$.

\begin{definition}[Pullback]
A \textbf{pullback} of a diagram in $\mathcal{C}$ of the form:
\begin{figure}[H]
\centering
\begin{tikzcd}
& y \arrow[d, "t"] \\
x \arrow[r, "s"'] & z
\end{tikzcd}
\end{figure}
is an object $p \in \mathcal{C}$, with maps $p_1, p_2$ to $x, y$ respectively, such that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
p \arrow[r, "p_2"]  \arrow[d, "p_1"'] & y \arrow[d, "t"] \\
x \arrow[r, "s"'] & z
\end{tikzcd}
\end{figure}
and is universal, in the sense that for all other diagrams:
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[r, "f_2"]  \arrow[d, "f_1"'] & y \arrow[d, "t"] \\
x \arrow[r, "s"'] & z
\end{tikzcd}
\end{figure}
we have a unique map $\bar{f}$ such that the following diagram commutes.
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[rd, "\bar{f}"] \arrow[rdd, bend right, "f_2"'] \arrow[rrd, bend left, "f_1"]& & \\
& p \arrow[r, "p_2"] \arrow[d, "p_1"'] & y \arrow[d, "t"] \\
& x \arrow[r, "s"'] & z
\end{tikzcd}
\end{figure}
\end{definition}

The pullback of a diagram $X \stackrel{s}{\rightarrow} Z \stackrel{t}{\leftarrow} Y$ in $\mathbf{Set}$ is given by:
$$P = \{ (x, y) \in X \times Y~|~s(x) = t(y) \}$$
together with the usual projection maps. Many common constructions are instances of pullbacks, for example taking _inverse images_ or _subset intersection_.

Now that we have seen three examples of limits, we can hopefully see and appreciate the general pattern: we begin with some diagram in $\mathcal{C}$, and construct a new object with maps to the object in this diagram that is _universal_ in a specific sense.

Before we finally give the definition of a limit, we first formalize the notion of a diagram.

\begin{definition}
Let $\mathcal{C}$ be a category. Let $\mathbf{A}$ be some (typically small) category. A \textbf{diagram} in $\mathcal{C}$ of \emph{shape} $\mathbf{A}$ is a functor $\mathbf{A} \to \mathcal{C}$.
\end{definition}

The relevant shapes (small categories) of the diagrams for products, equalizers and pullbacks respectively are given by:
\begin{figure}[H]
\centering
\textbf{T} = \begin{tikzcd}[framed]
\bullet & \bullet
\end{tikzcd}\room \textbf{E} =
\begin{tikzcd}[framed]
\bullet \arrow[r, shift left]  \arrow[r, shift right] & \bullet
\end{tikzcd}\room \textbf{P} =
\begin{tikzcd}[framed]
& \bullet \arrow[d] \\
\bullet \arrow[r] & \bullet
\end{tikzcd}
\end{figure}

\begin{definition}[Limit]
Let $\mathcal{C}$ be a category, $\mathbf{A}$ a small category, and $D: \mathbf{A} \to \mathcal{C}$ a diagram in $\mathcal{C}$.

A \textbf{cone} on the diagram $D$ is given by an object $n \in \mathcal{C}$, with a family of arrows indexed by the objects $x \in \mathbf{A}$:
$$(f_x : n \to Dx)_{x \in \mathbf{A}},$$
such that for all maps $u: x \to y$ in $\mathbf{A}$, the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
& Dx \arrow[dd, "Du"] \\
n \arrow[ru, "f_x"] \arrow[rd, "f_y"']& \\
& Dy
\end{tikzcd}
\end{figure}
The object $n$ is called the \emph{vertex} or \emph{apex} of the cone. The diagram $D$ is called the \emph{base} of the cone.

A \textbf{limit} of a diagram $D$ is a cone with vertex $\ell$ and a family of arrows
$$(p_x: \ell \to Dx)_{x \in \mathbf{A}},$$
such that for each other cone $(f_x: n \to Dx)_{x \in \mathbf{A}}$ on $D$, there exists a unique map $\bar{f}$ such that for each $f_x$ the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
n \arrow[rd, "f_x"] \arrow[d, "\bar{f}"'] & \\
\ell \arrow[r, "p_x"] & Dx
\end{tikzcd}
\end{figure}
the maps $p_x$ are called the \emph{projections}. The map $\bar{f}$ is often called the \emph{mediating arrow}.
\end{definition}

You can think of the vertex of a cone as 'hovering above the diagram $D$ in $\mathcal{C}$', with maps extending to each vertex in $Dx$, which indeed forms the shape of a cone.

There are of course also the dual notions of colimits, cocones and so on. In fact, in the following we are mostly interested in colimits.

Let us sketch why $\mathbf{Set}$ has all limits, i.e. that $\mathbf{Set}$ is _complete_. Note that, as always, there is also the dual notion of being cocomplete.

We let $D: \mathbf{A} \to \mathbf{Set}$ be a diagram.

- We write $L = \varprojlim D$ for the (candidate) limit of $D$.
- For each set $X$ we have $X \simeq \text{Hom}_{\mathbf{Set}}(1, X)$, where $1$ is the singleton set (for every element of $X$ there is a unique map from $1$). Note that $L$ is some set.
- Note that in general, for any limit $\ell \in \mathcal{C}$ of some diagram $D$, and any $a \in \mathcal{C}$, cones with vertex $a$ are in bijection with maps $a \to \ell$. Indeed, any such map leads to a cone by composition with the projections from $\ell$. Conversely, for each cone a unique such arrow is given by $\bar{f}$, by definition of the limit.

This means in particular that:
\begin{align*}
L &=  \varprojlim D \simeq \text{Hom}_{\mathbf{Set}}(1, L)\\
  &\simeq \{ \text{cones on } D \text{ with vertex } 1 \} \\
  &\simeq \{ (x_a)_{a \in \mathbf{A}}~|~ x_a \in Da \text{ such that } \forall u: a \to b \text{ in } D~ \text{ we have } Du(x_a) = x_b \}
\end{align*}
In other words, the limit corresponds to all possible tuples with elements in $Da$, indexed by elements of $\mathbf{A}$, that are compatible with the structure of the diagram.

This can be used to show that $\mathbf{Set}$ has all limits. $\mathbf{Set}$ is in fact bicomplete (both complete and cocomplete).

### $\omega$-chains

Next, we follow the discussion in Chapter 5 and Chapter 10 of Awodey. Presently, in the context of initial algebras, we are interested in diagrams of shape $\omega: (\mathbb{N}, \leq)$, i.e. the poset structure of the natural numbers seen as a category, which has the shape:
\begin{figure}[H]
\centering
$\omega =$ \begin{tikzcd}[framed]
\ldots & \bullet \arrow[l] & \bullet \arrow[l] & \bullet \arrow[l] & \bullet \arrow[l]
\end{tikzcd}
\end{figure}
of course along with the composites of these arrows. Because the diagram is induced by a functor, we can ignore these composite arrows since we can define them to get taken to the composites of the images of the arrows that are pictured. A diagram of shape $\omega$ in a category $\mathcal{C}$ takes the form:
\begin{figure}[H]
\centering
\begin{tikzcd}
\ldots & a_3 \arrow[l, "f_3"'] & a_2 \arrow[l, "f_2"'] & a_1 \arrow[l, "f_1"'] & a_0 \arrow[l, "f_0"']
\end{tikzcd}
\end{figure}
we will call a diagram like this an $\omega$-chain. If there exists a colimit for all $\omega$-chains, then we say $\mathcal{C}$ has $\omega$-colimits. We can denote the the colimit of an $\omega$-chain like the one above as:
$$a_{\omega} = \varinjlim a_i.$$

As a final notion, note that if $F$ is an endofunctor, then for each $\omega$-chain (or more generally each diagram $D$ of shape $\mathbf{A}$), we obtain another $\omega$-chain (diagram $FD$ of shape $\mathbf{A}$):
\begin{figure}[H]
\centering
\begin{tikzcd}
\ldots & Fa_3 \arrow[l, "Ff_3"'] & Fa_2 \arrow[l, "Ff_2"'] & Fa_1 \arrow[l, "Ff_1"'] & Fa_0 \arrow[l, "Ff_0"']
\end{tikzcd}
\end{figure}
We say that a functor $F$ preserves $\omega$-colimits (or is $\omega$-cocontinuous) if:
$$\varinjlim Fa_i = F \varinjlim a_i.$$

## Polynomial functors have initial algebras

\begin{definition}[Polynomial functor]
Let $\mathcal{C}$ be a category with finite (co-)products. A \textbf{polynomial functor} from $\mathcal{C} \to \mathcal{C}$ is defined inductively as:
\begin{itemize}
\item The identity functor $\text{Id}_\mathcal{C}$ is a polynomial functor.
\item All constant functors $\Delta_c: \mathcal{C} \to \mathcal{C}$ are polynomial functors.
\item If $F$ and $F'$ are polynomial functors, then so are $F \circ F'$, $F + F$ and $F \times F$.
\end{itemize}
\end{definition}

For example, the functors $F(X) = 1 + X$ (natural numbers) and $F'(X) = 1 + A \times X$ (lists) that we treated before are polynomial.

\begin{lemma}
Polynomial functors on $\mathbf{Set}$ are $\omega$-cocontinuous.
\end{lemma}

\begin{proof}
Constant and identity functors clearly preserve colimits. It is a general result that compositions, products and coproducts of preserving functors are also preserving.
\end{proof}

Now we arive at the main result of this chapter, which I have seen attributed to either Adamek, or to Smyth and Plotkin:
\begin{proposition}
Let $\mathcal{C}$ be a category. If $\mathcal{C}$ has an initial object $0$ and $\omega$-colimits, and if the functor:
$$F: \mathcal{C} \to \mathcal{C}$$
preserves $\omega$-colimits, then $F$ has an initial algebra.
\end{proposition}

\begin{proof}
Let $f_!$ be the unique arrow $0 \to F0$. Consider the $\omega$-chain:
\begin{figure}[H]
\centering
\begin{tikzcd}
\ldots & F^30 \arrow[l, "F^3f_!"'] & F^20 \arrow[l, "F^2f_!"'] & F0 \arrow[l, "Ff_!"'] & 0 \arrow[l, "f_!"']
\end{tikzcd}
\end{figure}
Let $\ell$ be the object of the limit of this sequence Since $F$ preserves colimits, we have:
$$F\ell \equiv F \varinjlim F^i \simeq \varinjlim F(F^i 0) \simeq F^i 0 \equiv \ell.$$
here, the last isomorphism says that the limit of the $\omega$-chain disregarding $0$ is the same as the one of the original $\omega$-chain. Intuitively, $0$ has a unique arrow to the limit anyway, so removing it does not change anything.

So we have an isomorphism $\phi: F\ell \simeq \ell$. To show that $(\ell, \phi)$ is initial we consider any $F$-algebra $(a, \alpha)$, and look at the data:
\begin{figure}[H]
\centering
\begin{tikzcd}
F\ell \arrow[d, "Fg"'] \arrow[r, "\sim"] & \ell \arrow[d, "g"] \\
Fa \arrow[r, "\alpha"'] & a \\
F0 & \arrow[l, "f_!"'] \arrow[u, "a_!"'] 0
\end{tikzcd}
\end{figure}
Note that any $F$-algebra $(a, \alpha)$ defines a cocone with vertex $a$, and family of morphisms:
$$(\alpha_i: F^i 0 \to a)_{i \in \omega},$$
denoting with $a_!: 0 \to a$ the unique arrow from $0$ to $a$, we define $\alpha_i$ inductively as:
\begin{align*}
\alpha_0 &\equiv a_! \\
\alpha_n &= \alpha \circ F(\alpha_{n - 1})
\end{align*}
We will write for the colimit cocone with vertex $\ell$:
$$(c_i: F^i 0 \to \ell)_{i \in \omega}.$$

To show that there is a unique algebra homomorphism which we suggestively denote $\lbanana \alpha \rbanana$ from $\ell$ to $a$, we will first show that if it exists, then it should be the unique mediating arrow $\bar{f}$ between the cocones with vertices $\ell$ and $a$ respectively, i.e. we should have for all $i \in \omega$:
$$\lbanana \alpha \rbanana \circ c_i = \alpha_i.$$
The first case is trivially true, because both arrows have domain $0$ which is initial. We proceed using induction, and we use that $F\ell$ is the vertex of a cocone $(F c_n)$, and the mediating arrow has to be given by the isomorphism $\phi$: 
\begin{align*}
\lbanana \alpha \rbanana \circ c_{n + 1} &= \lbanana \alpha \rbanana \circ \phi \circ F(c_n)\\
&= \alpha \circ F \lbanana \alpha \rbanana \circ Fc_n \\
&=  \alpha \circ F ( \lbanana \alpha \rbanana \circ c_n)) \\
&=  \alpha \circ F ( \alpha_{n})) \\
&= \alpha_{n + 1}
\end{align*}
So that if such a homomorphism $\lbanana \alpha \rbanana$ exists, it is unique by the uniqueness of the mediating arrow. To show that it exists, we define it as the mediating arrow, and show that this is a algebra homomorphism. We do this by showing that both:
$$\lbanana \alpha \rbanana \circ \phi, \text{ and } \alpha \circ F \lbanana \alpha \rbanana.$$
are mediating between the cones of $F\ell$ and $a$, and must therefore correspond to the unique mediating arrow (showing that the mediating arrow is an homomorphism). The cases for $i = 0$ are again trivial. The inductive step for the first arrow:
$$\lbanana \alpha \rbanana \circ \phi \circ Fc_n = \lbanana \alpha \rbanana \circ c_{n + 1} = \alpha_{n + 1}$$
In the first step, we use the $\phi$ is mediating between the cones at $F\ell$ and $\ell$, and at the second step we use that we defined $\lbanana \alpha \rbanana$ to be mediating between $\ell$ and $a$.
For the second arrow:
$$\alpha \circ F \lbanana \alpha \rbanana \circ Fc_n = \alpha \circ F(\lbanana \alpha \rbanana \circ c_n) = \alpha \circ F(\alpha_n) = \alpha_{n+1}$$
as required.

\end{proof}

\begin{corollary}
Every polynomial functor $F: \mathbf{Set} \to \mathbf{Set}$ has an initial algebra.
\end{corollary}

## Least fixed points in Haskell

In this part we will assume familiarity with the `foldr` class of functions, see the exercises and the appendix on Haskell.

In Haskell, the point of using $F$-algebras is to convert functions with signature:
```haskell
alpha :: f a -> a
```
for a given functor `f`, to functions that look like:
```haskell
alpha' :: Fix f -> a
```
where `Fix f` is the fixed point of `f`. Compared to our category theory notatation we have:
\begin{align*}
\texttt{alpha} &\equiv \alpha \\
\texttt{alpha}' &\equiv \lbanana \alpha \rbanana
\end{align*}
So the universal property corresponding to a least fixed point in Haskell, expressed in Haskell, is the existence of a function that does this conversion of $\alpha$ to $\lbanana \alpha \rbanana$. Let us call it `cata`:
```haskell
cata :: (f a -> a) -> Fix f -> a
```
Whenever you see a universal property like this in Haskell, and you want to find out what the type of `Fix f` should be, there is an easy trick, we simply define the type to have this universal property.
```haskell
-- we look at
flip cata :: Fix f -> (f a -> a) -> a
-- which leads us to define
data Fix f = Fix { unFix :: (f a -> a) -> a }
-- now we have
unFix :: Fix f -> (f a -> a) -> a
-- so we can define
cata = flip unFix
```
Now we have our `cata` function, but it is of no use if `Fix f` is not inhabited. We want to be able to convert any value of type `f a` into a `Fix f` value. We first introduce the following equivalent description of `Fix f`:
```haskell
-- for a fixed point, we have `x ~= f x`
-- the conversion between x and f x, where x == Fix' f
-- can be done using Fix' and unFix'
data Fix' f = Fix' { unFix' :: f (Fix' f) }
-- for Fix, we can define cata as:
cata' :: Functor f => (a -> f a) -> Fix' f -> a
cata' alpha = alpha . fmap (cata' alpha) . unFix'
-- to show that we can convert between the Fix and Fix':
iso :: Functor f => Fix' f -> Fix f
iso x = Fix (flip cata' x)
invIso :: Functor f => Fix f -> Fix' f
invIso y = (unFix y) Fix'
```
`Fix' f` is sometimes written as $\mu F$ (or `Mu f` in Haskell).

So, in summary, catamorphing an algebra can be done recursively using:
```haskell
type Algebra f a = f a -> a
data Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Mu f -> a
cata a = f . fmap (cata a) . unFix
```

## Using catamorphisms in Haskell

To give an interpretation of `cata`, we first show how we usually construct values of `Fix f`. Say we have a very simple expression language, with constants and addition:
```haskell
data Expr' = Cst' Int | Add' (Expr', Expr')
-- we introduce 'holes' in the add, instead of recurring
data ExprR b = Cst Int | Add (b, b)
-- we reobtain the original expression by finding the 'fixed point'
type Expr = Fix' ExprR
-- we make wrappers to construct values of type Expr
cst = Fix' . Cst
add = Fix' . Add
-- we turn ExprR into a functor
instance Functor ExprR where
  fmap _ (Cst c) = Cst c
  fmap f (Add (x, y)) = Add (f x, f y)
```
We can use this in the following way:
```haskell
eval = cata algebra where
    algebra (Cst c) = c
    algebra (Add (x, y)) = x + y

printExpr = cata algebra where
    algebra (Cst c) = show c
    algebra (Add (x, y)) = "(" ++ x ++ " + " ++ y ++ ")"
```
And it allows us to perform our optimizations independently, during the same traversal, e.g.:
```haskell
leftUnit :: ExprR Expr -> Expr
leftUnit (Add (Fix (Cst 0), e)) = e
leftUnit e = Fix e

rightUnit :: ExprR Expr -> Expr
rightUnit (Add (e, Fix (Cst 0))) = e
rightUnit e = Fix e

comp f g = f . unFix . g
optimize = cata (leftUnit `comp` rightUnit)
```

## References

Books:

- Limits are in all basic CT books, here we followed Tom Leinster Basic Category Theory
- Barr & Wells: Chapter 14 deals with F-algebras and fixed points, or final chapter of Awodey
- Some theory on fixed points in Haskell can be found in 'Hinze; Adjoint Folds and Unfolds' and 'Bird; Generalized Folds for RDT'. See also Philip Wadler; Recursive types for free.
- Catamorphisms and its siblings were popularized by 'Meijer et al; Functional Programming using bananas, envelopes and barbed wire'. Also discussed in 'Bird, de Moor; Algebra of Programming'.

On the web:

- <http://comonad.com/reader/2013/algebras-of-applicatives/>
- <http://files.meetup.com/3866232/foldListProduct.pdf>
- <https://deque.blog/2017/01/17/catamorph-your-dsl-introduction/>
- <https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms>
- <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras>
- <http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt>
- <http://web.cecs.pdx.edu/~sheard/course/AdvancedFP/notes/CoAlgebras/Code.html>


