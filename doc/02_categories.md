\part{Basic theory}

# Categories, functors and natural transformations

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

    As an additional example, there is also a forgetful functor $F: \mathbf{Cat} \to \mathbf{Graph}$, sending each category to the graph defined by its objects and arrows.

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
  &a \arrow[dr] \arrow[dl, shift right] &  \\
i\arrow[ur, shift right] \arrow[rr] \arrow[dr] & & t \arrow[dl, shift right]\\
  &b \arrow[ur, shift right] &
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
Let $f: A \to B$. Suppose $f$ is injective, and let $g, h: X \to A$. If $g \neq h$, then $g(x) \neq h(x)$ for some $x$. But since $f$ is injective, we have $f(g(x)) \neq f(h(x))$, and hence $f \circ g \neq f \circ h$, thus $f$ is mono.

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

## Exercises

\begin{exercise}
Let $\mathcal{C}$ be a category, and let $f: a \to b$ in $\mathcal{C}$ be iso with inverse $g: b \to a$. Show that $g$ is unique, i.e. for any $g'$ that is an inverse of $f$ we have $g' = g$.
\end{exercise}

\begin{exercise}
Let $F: \mathcal{C} \to \mathcal{D}$, and let $f: a \to b$ be an isomorphism in $\mathcal{C}$. Show that $F f: F a \to F b$ is an isomorphism in $\mathcal{D}$.
\end{exercise}

\begin{exercise}
Is there a functor $Z: \mathbf{Grp} \to \mathbf{Grp}$ so that $Z(G)$ is the center of $G$?
\end{exercise}

\begin{exercise}
Let $F: \mathcal{C} \to \mathcal{D}, G: \mathcal{D} \to \mathcal{E}$ be functors, define $G \circ F: \mathcal{C} \to \mathcal{E}$ and show that it is a functor.
\end{exercise}

\begin{exercise}
Let $F, G: \mathcal{C} \to \mathcal{D}$ be functors, and let $\mu: F \Rightarrow G$. Show that $\mu$ is an isomorphism (in the category of functors between $\mathcal{C}$ and $\mathcal{D}$) if and only if its components are isomorphisms (in $\mathcal{D}$) for all $a \in \mathcal{C}$.
\end{exercise}

## References

- 1.1 -- 1.4 and 1.8 of Mac Lane
- 1.1, 1.2, 2.1, 3.1 and 3.2 of Asperti and Longo
- 2.1, 2.7, 2.8, 3.1, 4.2, 4.3 of Barr and Wells
- 1.1, 1.7, 1.10 of the 'Category Theory for Programmers' blog by Bartosz Milewski (best to study after reading Chapter 2)


