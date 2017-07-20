# Cartesian closed categories and $\lambda$-calculus

In Haskell, functions that take two arguments can be written as:
```haskell
-- 1) idiomatic haskell
f :: a -> b -> c
-- 2) more conventional style
f :: (a, b) -> c
```
the first style can be read as "for any fixed value `x` of type `a`, you are given a function `b -> c` which sends `y` to `f(x, y)`". In this part we will discuss why we are allowed to do this, and see the theory that underpins this. The process of converting the second to the first style is called *currying* (the reverse is called *uncurrying*) and can be described in the context of category theory.

In the language of category theory, we are trying to show the equivalence between arrows of the form $a \times b \to c$ and arrows of the form $a \to [b \to c]$, where $[b \to c]$ is some 'function object'. We will first state what it means to *curry* a function between sets.

\begin{definition}
Let $A, B, C$ be sets. We define $[A \to B]$ to be the \emph{set of functions} between $A$ and $B$. Given a function of two variables:
$$f: A \times B \to C,$$
we have a function:
$$\lambda f: A \to [B \to C],$$
defined by $\lambda f(a)(b) = f(a, b)$. We say that $\lambda f$ is the \emph{curried} version of $f$, and going from $f$ to $\lambda f$ is called \emph{currying}.

Going the other way around is called \emph{uncurrying}. Let
$$g: A \to [B \to C],$$
be a function, then we can define $\lambda^{-1}g: A \times B \to C$ by setting $\lambda^{-1}g(a, b) = g(a)(b)$. In other words, we have an isomorphism $\lambda$ between the hom-sets:
$$\text{Hom}_{\mathbf{Set}}(A \times B, C) \simeq \text{Hom}_{\mathbf{Set}}(A, [B \to C]).$$
\end{definition}

We are now ready to discuss this process more generally, but for this we need to specify what properties our category should have in order for this to work.

\begin{definition}[Cartesian closed category]
A category $\mathcal{C}$ is called \emph{cartesian closed} (or a \emph{CCC}), if the following conditions are satisfied:
\begin{enumerate}
\item It has a terminal object $1$.
\item For each pair $a, b \in \mathcal{C}$ there exists a product $a \times b$.
\item For each pair $a, b \in \mathcal{C}$ there exists an object $[a \to b]$ called the \emph{exponential} such that:
\begin{itemize}
\item there exists an arrow: $\text{eval}^a_b: [a \to b] \times a \to b.$
\item For any arrow $f: a \times b \to c$ there is a unique arrow $\lambda f: a \to [b \to c]$ so that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
& {[}b \to c{]} \times b \arrow[rd, "\text{eval}^b_c"] & \\
a \times b \arrow[ru, "\lambda f \times \text{id}_b"] \arrow[rr, "f"'] & & c \\
\end{tikzcd}
\end{figure}
Here, the product of arrows $f \times g$ is as given in Example \ref{exa:productbifunctor}.
\end{itemize}
\end{enumerate}
\label{def:ccc}
\end{definition}

Wherever possible, we will denote $\text{eval}_b^a$ simply as $\text{eval}$. Another common notation for the exponential $[a \to b]$ is $b^a$.

Note that the commutative diagram that shows up in the definition directly implies that we indeed have a bijection of hom-sets (i.e. it makes sense to curry). That is to say, let $\mathcal{C}$ be a CCC:
$$\text{Hom}_{\mathcal{C}}(a \times b, c) \simeq \text{Hom}_{\mathcal{C}}(a, [b \rightarrow c])$$
are isomorphic, by sending a $f: a \times b \to c$ using:
$$\lambda: f \mapsto \lambda f,$$
and vice versa:
$$\lambda^{-1} g = \text{eval}_b^c \circ (g \times \text{id}_b).$$
which is an isomorphism by the commutativity of the diagram and the uniqueness of $\lambda f$.

To prove that curried and uncurried version of binary functions are actually *equivalent* we would have to show something stronger, that there is an arrow between $[a \times b \to c] \to [a \to [b \to c]]$ that is *iso*, but for this we need some more complicated machinery which for now would be too big of a diversion.

One can show that exponentials are unique up to unique isomorphism, but this again requires some machinery that we have not yet developed. We may revisit this when when we get to discuss adjunctions.

We have already seen that \textbf{Set} is a CCC. Before we give some additional properties of CCCs and the exponential objects in them, let us look at some additional examples of CCCs:

\begin{example}[Boolean algebras as CCCs]
\begin{definition}
A \textbf{Boolean algebra} is a partially ordered set $B$ such that:
\begin{itemize}
\item For all $x, y \in B$, there exists an infimum $x \wedge y$ and a supremum $x \vee y$.
\item For all $x, y, z$ we have a distributive property:
$$x \wedge (y \vee z) = (x \wedge y) \vee (x \wedge z).$$
\item There exists a \emph{smallest} element $0$, and a \emph{greatest} element $1$, which satisfiy e.g. $0 \vee x = x$, $1 \vee x = 1$.
\item There exists a complement $x \vee \neg x = 1$, $x \wedge \neg x = 0$.
\end{itemize}
\end{definition}
Let us check that a Boolean algebra is a CCC.
\begin{itemize}
\item It has a terminal object $1$.
\item It has infimums (i.e. products) for all pairs of elements.
\item We define the exponential as Boolean implication, i.e. $[a \to b] = \neg a \vee b$. Since $\text{eval}^a_b: [a \to b] \times a \to b$ and arrows between objects of posets are unique, we simply have to show that $[a \to b] \wedge a \leq b$ to obtain evaluation arrows:
\begin{align*}
[a \to b] \wedge a &= (\neg a \vee b) \wedge a = (\neg a \wedge a) \vee (b \wedge a) \\
                   &= 0 \vee (b \wedge a) = b \wedge a \leq b
\end{align*}
Where we used a distributive property, and in the final step the definition of an infimum.
\item Next we need to be able to curry, i.e. show that $\lambda f$ exists. Note that indeed we only have to show that such an arrow exists, by definition every diagram in a poset category commutes, since arrows between objects are unique. Say we have an arrow from $a \times b \to c$, i.e. we have $a \wedge b \leq c$. Then:
\begin{align*}
a &= a \wedge 1 = a \wedge (b \vee \neg b) = (a \wedge b) \vee (a \wedge \neg b) \leq c \vee (a \wedge \neg b) \\
&\leq c \vee \neg b \equiv \neg b \vee c \equiv [b \to c]
\end{align*}
So there is indeed an arrow from $a \to [b \to c]$, as required.
\end{itemize}
\end{example}

\begin{example}[Small categories as a CCC]
Before, we briefly discussed $\mathbf{Cat}$, the category of small categories. Let $\mathcal{C}, \mathcal{D} \in \mathbf{Cat}$, then we can define:
$$[\mathcal{C} \to \mathcal{D}] \equiv \mathbf{Fun}(\mathcal{C}, \mathcal{D}).$$
So the exponentials correspond to the \emph{functor categories} between the categories in question.

Let $F: \mathcal{C} \times \mathcal{D} \to \mathcal{E}$ be a functor, then we want to construct a functor $\lambda F: \mathcal{C} \to [\mathcal{D} \rightarrow \mathcal{E}]$. This functor should send each object $c$ to a functor $\lambda F(c)$ between $\mathcal{D}$ and $\mathcal{E}$, and arrows of $\mathcal{C}$ to natural transformations between $\mathcal{D}$ and $\mathcal{E}$. We define this, using $F$, as:
\begin{itemize}
\item The functor for $c \in \mathcal{C}$:
    \begin{itemize}
        \item For objects $d \in \mathcal{D}$ we have $\lambda F(c)(d) = F(c, d).$
        \item For arrows $g: d \to d'$ we have $\lambda F(c)(g) = F(\text{id}_c, g).$
    \end{itemize}
\item The natural transformations for $f: c \to c'$ in $\mathcal{C}$ should be between the functors $F(c), F(c')$:
    \begin{itemize}
    \item For $d \in \mathcal{D}$, the component of $\lambda F f \equiv \mu$ at $d$ is given by:
    \begin{align*}
    \mu: &F(c) \Rightarrow F(c') \\
    \mu_d: &F(c)(d) \rightarrow F(c')(d) \\
         : &F(c, d) \rightarrow F(c', d) \\
    \mu_d \equiv &F(f, \text{id}_d)
    \end{align*}
    Let us check that this indeed defines a natural transformation. Let $g: d \to d'$ in $\mathcal{D}$:
\begin{figure}[H]
\centering
\begin{tikzcd}
F(c, d) \arrow[d, "F(c)(g)"'] \arrow[r, "\mu_d"] & F(c', d) \arrow[d, "F(c')(g)"] \\
F(c, d') \arrow[r, "\mu_{d'}"] & F(c', d')
\end{tikzcd}
\end{figure}
    To show that this commutes, we compute:
    \begin{align*}
    \alongtop &= F(c')(g) \circ \mu_d = F(\text{id}_{c'}, g) \circ F(f, \text{id}_d) \\
              &= F(\text{id}_{c'} \circ f, g \circ \text{id}_d) \\
              &= F(f \circ \text{id}_{c'}, \text{id}_{d'} \circ g) \\
              &= F(f, \text{id}_{d'}) \circ F(\text{id}_{c}, g) = \mu_{d'} \circ F(c)(g) = \alongbottom
    \end{align*}
    Where we used the definition of composition in a product category, and the fact that $F$ is a functor so it plays nice with composition.
    \end{itemize}
\end{itemize}
So we can indeed 'curry' in the category of small categories, the other properties (e.g. that it has a terminal object, the category with one object and its identity arrow) are easy to check.
\end{example}

## $\lambda$-calculus and categories

One of the interesting features of a CCC is that it can model a $\lambda$-calculus, which is one of the universal models of computations, and corresponds to underlying computational model for functional programming (whereas imperative languages are based on Turing machines).

In this section we will give a brief and incomplete introduction to (typed) $\lambda$-calculus. Our reason to discuss them is to better understand functional languages, and to give further motivation to the definition of CCCs.

*Expressions*, or *$\lambda$-terms*, form the key component of $\lambda$-calculus. In these expressions there can be *variables* which are identifiers that can be seens as placeholders, and *applications*. An expression is defined recursively as one of the following:

- a *variable* $x$.
- if $t$ is an expression and $x$ a variable, $\lambda x.t$ is an expression (called an *abstraction*).
- if $t$ and $s$ are expressions, then so is $ts$ (called an *application*).

The only 'keywords' that are used in the $\lambda$-calculus language are the $\lambda$ and the dot. Multiple applications can be dismbiguated using parentheses, where the convention is that they associate from the left if these are omitted, i.e.
$$t_1 t_2 t_3 \ldots t_n = (\ldots((t_1 t_2) t_3) \ldots t_n).$$
Abstractions can model functions, for example the identity function could be written as:
$$\lambda x . x$$
Note that the choice for the name $x$ is completely arbitrary, equivalently we could have written
$$\lambda y . y \equiv \lambda z . z$$
and so on. This is called **$\alpha$-conversion**.

Note that we do not have to give this function any name, but is simply defined to be the given expression. This is why anonymous functions in programming are often called $\lambda$-functions. On the left of the dot we have the *arguments* preceded by a $\lambda$. On the right of the dot we have the *body* expression.

Functions like this can be *applied* to expressions, by **substituting** the expressions as the 'value' for the argument, i.e.\ say we have a function evaluated at some point:
$$f(x) = ax,~ f(y)$$
then the corresponding expression would be:
$$(\lambda x . ax)y \equiv ay$$
This substitution process is called **$\beta$-reduction**, and can be seen as a computational step.

A *variable* can be free or bound, for example in our example
$$\lambda x . ax,$$
$a$ is free, while $x$ is bound -- i.e. associated to an argument. We can make this formal:
\begin{definition}[Free and bound variables]
A variable $x$ is \textbf{free} only in the following cases:

\begin{itemize}
\item $x$ is free in $x$.
\item $x$ is free in $\lambda y.t$ if $y \neq x$ are not the same identifier, and $x$ is free in $t$.
\item $x$ is free in $st$ if it is free in either $s$ or $t$.
\end{itemize}

A variable $x$ is \textbf{bound} in the following cases:
\begin{itemize}
\item $x$ is bound in $\lambda y.t$ if $y = x$ is the same identifier, or if $x$ is bound in $t$.
\item $x$ is bound in $st$ if it is bound in either $s$ or $t$.
\end{itemize}

\end{definition}
Note that a variable can be both bound *and* free in the same expression. For example, $y$ is both bound and free in:
$$(\lambda y.y)(\lambda x.xy).$$
Also, note that this implies that the same identifiers may be used indepently in multiple expressions, but should not be mixed up. We should rename identifiers wherever necessary when applying functions.

Say $f$ does not contain $x$ as a free variable, then we can equivalently write:
$$\lambda x.fx \equiv f,$$
this is called **$\eta$-conversion**.

\begin{example}[Natural numbers]
Since the $\lambda$-calculus forms a very minimal programming language, we may expect it to be able to perform basic mathematical tasks. Indeed it can, and as an example we will see how we can model the natural numbers as expressions in $\lambda$-calculus.

We define:
$$0 \equiv \lambda s.(\lambda z.z) \equiv \lambda sz.z,$$
where we also introduced syntax for functions of multiple parameters. Note that by convention these associate from the right, contrary to expressions.

The natural numbers are defined recursively by applying $s$ to the body of the function corresponding previous number:
\begin{align*}
1 &= \lambda sz.sz\\
2 &= \lambda sz.s(sz)\\
3 &= \lambda sz.s(s(sz))\\
&\ldots
\end{align*}
This leads naturally to the \emph{successor function}, which corresponds to the following expression:
$$S = \lambda wyx.y(wyx).$$
Writing $s^k z = s(s(s(s(s \ldots (sz)))))$, with $k$ occurences of $s$, we can see that:
\begin{align*}
S k &= (\lambda wyx.y(wyx))(\lambda sz. s^k z) \\
    &= (\lambda yx.y((\lambda sz. s^k z)yx)) \\
    &= (\lambda yx.y(y^k x)) \\
    &= (\lambda yx.y^{k+1} x) \\
    &\equiv (\lambda sz.s^{k+1} z) \\
    &\equiv k + 1
\end{align*}
\end{example}
In similar ways, one can define addition and multiplication, logical operations, equality, and ultimately even simulate a Turing machine using $\lambda$-calculus.

## Typed $\lambda$-calculus

In the context of typed functional languages, we are interested in *typed* $\lambda$-calculus. This is an extension of $\lambda$-calculus, where each expression has a *type*. We will sketch this extension and the associated category here, but note that we will freely glance over some technicalities and will not prove anything in too much detail. The goal of this part is to give an idea of how CCCs can be applied more broadly to functional program than just formalizing the notion of function types and currying.

To define what a type is, we introduce the set of *symbols* as:
$$\mathcal{S} = \{ S_1, S_2, S_3, \ldots \}.$$
A *type* is defined recursively as either:

- A *symbol* $S_i$
- If $T_1, T_2$ are types, then so is $T_1 \rightarrow T_2$.

If $t$ is an expression then we write $t:T$ to indicate that its type is $T$. Types corresponding to expressions have to obey a number of rules, e.g.:

- $c:T$ denotes a constant of type $T$.
- For each type, there is a countable number of variables $x_1:T$, $x_2:T$, \ldots
- If $t:T_1 \rightarrow T_2$ and $s : T_1$ then $ts:T_2$
- For a variable $x : T_1$, given an expression $t:T_2$, we obtain a *function* $\lambda x.t : T_1 \to T_2$.
- There is a singleton type $1$ with an expression $* : 1$. Any other expression of this type is equal (see below) to $*$ as seen from $\Gamma = \emptyset$.

*Equations*, or *equality judgements* in this calculus have the form:
$$\Gamma|t = s:T.$$
Here, $\Gamma$ is some set of variables that at least contains *all* the free variables in both $t$ and $s$. Such an equation means that according to $\Gamma$ (i.e. with respect to its variables), the expressions $t$ and $s$ of type $T$ are equal. These equations are subject to some rules, e.g. for fixed $\Gamma$ they define an equivalence relation of expressions of type $T$, but we will not list these here. For an overview, see the suggested literature at the end of this chapter.

## Typed $\lambda$-calculus as a CCC

We can go back and forth between CCCs and $\lambda$-calculus. Let us describe how we can obtain a CCC from a typed $\lambda$-calculus.

\begin{definition}
Given a typed $\lambda$-calculus $\mathcal{L}$, we associate it to a category $\mathcal{C}(\mathcal{L})$ where:

\begin{itemize}
\item The objects are types $T$.
\item The arrows $T \to T'$ are pairs of
\begin{itemize}
    \item an equivalence class of expressions of types $T'$. The equivalence of two expressions $t, s$ where $t$ may contain the variable $x$, and $s$ may contain the variable $y$, is defined as follows:
    \begin{itemize}
    \item both $s, t$ are of the same type $T$
    \item $x$ has the same type as $y$ and is substitutable for it in $s$ (this means that occurence of $x$ becomes bound after substituting it for $y$ in $s$)
    \item $\{x\} | (\lambda y . s)x = t : T.$
    \end{itemize}
    There are multiple reasons for needing this relation, e.g. we want all the expressions of a type $T$ that correspond to single variables to correspond to the same identity arrow of the type $T$. Also, together with the properties of the singleton type $1$, this ensures that we get a terminal object corresponding to the type $1$.
    \item a free variable $x$ of type $T$ (that does not necessarily have to occur in the expression(s))
\end{itemize}
\end{itemize}
\end{definition}

You can prove $\mathcal{C}(\mathcal{L})$ is indeed cartesian closed, and also that any CCC defines a $\lambda$-calculus, but we will not do this here primarily because the definition given here is incomplete and would be overly long otherwise.

There are a number of advantages of viewing a $\lambda$-calculus from the viewpoint of a CCC. For example, often variables (identifiers) clash between expressions, and this requires carefully renaming variables where necessary. When considering the arrows of the associated CCC, all placeholders have been identified by means of the equivalence relation, and this is no longer an issue. Also, the fact that we can compose arrows means that results from category theory can be used for further reduction of expressions.

## References

- 1.9 of the 'Category Theory for Programmers' blog by Bartosz Milewski
- 6.1, 6.2, 6.3 of Barr and Wells
- \url{https://en.wikibooks.org/wiki/Haskell/The_Curry-Howard_isomorphism}
- Raul Rojas: A tutorial introduction to $\lambda$-calculus
- Chapter 7 of van Oosten


