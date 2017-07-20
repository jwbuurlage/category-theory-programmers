# Products, coproducts and algebraic data types

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

Let us revisit the idea of *duality*. What would be the dual notion of the product? Let us take the product diagram, and reverse the arrows:

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

Consider any element $a \in A$. It gets mapped to $f(a) \in V$, and to $i_1(a) = (a, 0)$ in $A + B$. Then we should set $q(a, 0) \equiv f(a)$, and similarly we should set $q(b, 1) \equiv g(b)$. This already defines $q$ uniquely and completely, so we conclude that the disjoint union is indeed the coproduct in the category \textbf{Set}.
\end{example}

We note there that the coproduct (and product) of two objects, generalizes also to products of more than 2 objects (by simply adding more maps $i_1, i_2, i_3 \ldots$).

## Algebraic data types

Let us apply the product (and coproduct) concepts to the category of types. Since we already saw what these constructs mean for sets, namely the cartesian product and the disjoint union respectively, it should be clear what this means for types.

Given a type $a$ and a type $b$, the product corresponds to a *pair*, written `(a, b)` in Haskell. We could implement this ourselves using simply:
```haskell
data Pair a b = Pair a b
```
Here, we give the unique value constructor the same name as its type constructor. In C this would correspond roughly to a `struct` (more specifically a POD data type), although a `Record` in Haskell corresponds more precisely to a `struct`. Note for this to make sense, the product type should be (and is) defined for  more than 2 elements.

In C++ this is known as a `std::pair` (or a `std::tuple` for n-ary products). However, its implementation (and also usage) is awkward and convoluted. Functional programming (and product/coproduct types) is not yet a first-class citizen in C++.

The coproduct (or _sum type_ corresponds to a value that has either type $a$, or type $b$. This is implemented as the `Either` data type:
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
\label{exa:productbifunctor}
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

## Exercises

\begin{exercise}
In the category $\mathbf{Vect}$, show that the product corresponds to the direct sum.
\end{exercise}

## References

- 2.1, 2.2, 3.1, 3.3 (partially) and 3.4 (partially) of Mac Lane
- 1.5, 1.6 and 1.8 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 2.6.7, 5.1, 5.2, 5.4 of Barr and Wells
- *Catsters*: Products and coproducts <https://www.youtube.com/watch?v=upCSDIO9pjc>


