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
    - \newtheorem{example}{Exa}
    - \setlength{\columnsep}{1cm}
    - \usepackage[margin=2.0cm,landscape,twocolumn]{geometry}
    - \setlength{\parindent}{0em}
    - \setlength{\parskip}{0.5cm}
    - \pagestyle{empty}
---

**Natural transformations between endofunctors**

\begin{align*}
T&: \mathcal{C} \to \mathcal{C} \\
T^2&: \mathcal{C} \to \mathcal{C} \\
T^3&: \mathcal{C} \to \mathcal{C}
\end{align*}

\begin{align*}
\alpha&: F \Rightarrow G \\
H\alpha&: HF \Rightarrow HG \\
\alpha H&: FH \Rightarrow GH
\end{align*}
$$(H\alpha)_a \equiv H(\alpha_a),~(\alpha H)_a \equiv \alpha_{H_a}.$$

**Monads over a category**

\begin{definition}
A \textbf{monad} $(T, \eta, \mu)$ over a category $\mathcal{C}$:

\begin{itemize}
\item $T: \mathcal{C} \to \mathcal{C}$ endofunctor
\item $\eta: \text{Id} \Rightarrow T, ~ \mu: T^2 \Rightarrow T$.
\end{itemize}
s.t.

\underline{associativity square} 

\begin{figure}[H]
\centering
\begin{tikzcd}
T^3 \arrow[d, Rightarrow, "T\mu"'] \arrow[r, Rightarrow, "\mu T"] & T^2 \arrow[d, Rightarrow, "\mu"] \\
T^2 \arrow[r, Rightarrow, "\mu"] & T
\end{tikzcd}
\end{figure}

\underline{unit triangles}

\begin{figure}[H]
\centering
\begin{tikzcd}
T \arrow[r, Rightarrow, "\eta T"] \arrow[dr, Rightarrow, "\text{id}"'] & T^2 \arrow[d, Rightarrow, "\mu"] & \arrow[l, Rightarrow, "T \eta"'] \arrow[dl, Rightarrow, "\text{id}"] T\\
  & T &
\end{tikzcd}
\end{figure}
\end{definition}

$\eta$: unit, $\mu$: multiplication.

\newpage

\begin{example}[Power-set monad]
$$\mathcal{P}: \mathbf{Set} \rightarrow \mathbf{Set}.$$
\begin{align*}
\eta&: \text{Id} \Rightarrow \mathcal{P},\\
\eta_A&: A \rightarrow \mathcal{P}(A),~a \mapsto \{ a \}.\\
\mu&: \mathcal{P}^2 \to \mathcal{P},\\
\mu_A&: \mathcal{P}(\mathcal{P}(A)) \to \mathcal{P}(A),\\
&\{ B_1, B_2, \ldots \} \mapsto \bigcup B_i,
\end{align*}
where $B_i \subseteq A$.
\end{example}

**Adjunctions give rise to monads**

$(F, G, \eta, \epsilon)$ adjunction.
$$\begin{array}{ll}
F: \mathcal{C} \to \mathcal{D}, & \eta: \text{Id}_\mathcal{C} \Rightarrow GF \\
G: \mathcal{D} \to \mathcal{C}, & \epsilon: FG \Rightarrow \text{Id}_\mathcal{D}
\end{array}$$

\begin{align*}
T &\equiv GF: \mathcal{C} \to \mathcal{C}, \\
\eta&: \text{Id}_\mathcal{C} \Rightarrow T \\
\mu&: T^2 \Rightarrow T \\
   &: GFGF \Rightarrow GF \\
\mu&\equiv G \epsilon F
\end{align*}
$$\mu_c \equiv G(\epsilon_{Fc}).$$
\begin{figure}[H]
\centering
\begin{tikzcd}
GFGFGF \arrow[d, Rightarrow, "GF\mu"'] \arrow[r, Rightarrow, "\mu GF"] & GFGF \arrow[d, Rightarrow, "\mu"] \\
GFGF \arrow[r, Rightarrow, "\mu"] & GF
\end{tikzcd}
\end{figure}
\begin{figure}[H]
\centering
\begin{tikzcd}
GFGFGFc \arrow[d, "GFG(\epsilon_{Fc})"'] \arrow[r, "G(\epsilon_{FGFc})"] & GFGFc \arrow[d, "G(\epsilon_{Fc})"] \\
GFGFc \arrow[r, "G(\epsilon_{Fc})"] & GFc
\end{tikzcd}
\end{figure}

\newpage

- $a \equiv FGFc$
- $b \equiv Fc$
- $\tilde{G} \equiv GFG$

\begin{figure}[H]
\centering
\begin{tikzcd}
\tilde{G}a \arrow[d, "\tilde{G}(\epsilon_{b})"'] \arrow[r, "G(\epsilon_{a})"] & Ga \arrow[d, "G(\epsilon_{b})"] \\
\tilde{G}b \arrow[r, "G(\epsilon_{b})"] & Gb
\end{tikzcd}
\end{figure}

naturality square for $f \equiv \epsilon_b: a \to b$ of $G\epsilon$.

\begin{figure}[H]
\centering
\begin{tikzcd}
GFc \arrow[rd, "\text{id}_{GFc}"'] \arrow[r, "\eta_{GFc}"] & GFGFc \arrow[d, "G(\epsilon_{Fc})"] \\
 & GFc
\end{tikzcd}
\end{figure}
Second triangle identity of adjunction at $Fc$.

\begin{figure}[H]
\centering
\begin{tikzcd}
G \arrow[r, Rightarrow, "\eta G"] \arrow[dr, Rightarrow, "\text{id}_G"'] & GFG \arrow[d, Rightarrow, "G \epsilon"] \\
  & G
\end{tikzcd}
\end{figure}

\newpage

**Kleisli categories**

$(T, \eta, \mu)$ over $\mathcal{C}$ $\leadsto$  *Kleisli category* $\mathcal{C}_T$.

\begin{definition}
$\mathcal{C}_T$ for $(T, \mu, \eta)$:
\begin{itemize}
\item \underline{Objects}: $a_T \leftrightarrow a \in \mathcal{C}$.
\item \underline{Arrows}:
$$f: a \to T b \text{ in } \mathcal{C} \leadsto f_T: a_T \to b_T.$$
i.e.:
$$\text{Hom}_{\mathcal{C}_T}(a_T, b_T) \simeq \text{Hom}_{\mathcal{C}}(a, Tb).$$
\item \underline{Composition}:
$$g_T \circ_T f_T \equiv (\mu_c \circ Tg \circ f)_T.$$
\item \underline{Identity arrows}:
$$\text{id}_{a_T} \equiv (\eta_a)_T.$$
\end{itemize}
\end{definition}

\begin{figure}[H]
\centering
\begin{tikzcd}
a_T \arrow[r, "f_T"] & b_T \arrow[r, "g_T"] & c_T \arrow[r, "h_T"] & d_T
\end{tikzcd}
\end{figure}
\begin{align*}
(h_T \circ_T g_T) \circ_T f_T &= (\mu_d \circ Th \circ g)_T \circ_T f_T \\&= (\mu_d \circ T(\mu_d \circ Th \circ g) \circ f)_T,\\
h_T \circ_T (g_T \circ_T f_T) &= h_T \circ_T (\mu_c \circ Tg \circ f)_T \\&= (\mu_d \circ Th \circ \mu_c \circ Tg \circ f)_T,
\end{align*}
$\leadsto$
$$\mu_d \circ T \mu_d \circ T^2 h = \mu_d \circ Th \circ \mu_c,$$
\underline{associativity square} and \underline{naturality of $\mu$}:
\begin{align*}
\mu_d \circ T \mu_d \circ T^2 h &= \mu_d \circ \mu_{Td} \circ T^2 h \\
&= \mu_d \circ Th \circ \mu_c
\end{align*}
\emph{left-unital}: use the \underline{right unit triangle}:
$$\mu_b \circ T \eta_b = \text{id}_b$$
$\leadsto$
\begin{align*}
\text{id}_{b_T} \circ_T f_T = (\mu_b \circ T(\eta_b) \circ f)_T = f_T
\end{align*}

Composition operator $\circ_T$ is usually denoted `>=>` (the fish operator) in Haskell:

```haskell
(>=>) ::
  (a -> T b) ->
  (b -> T c) ->
  (a -> T c)
f >=> g = join . fmap g . f
```

**Every monad is induced by an adjunction**

$(T, \eta, \mu)$ monad over $\mathcal{C}$, Kleisli $\mathcal{C}_T$.
$$\exists?~F: \mathcal{C} \to \mathcal{C}_T,~G: \mathcal{C}_T \to \mathcal{C}$$
s.t. $F \dashv G$, and $GF \leadsto T$.

We define:
\begin{align*}
F_T&: \mathcal{C} \to \mathcal{C}_T, \\
& a \mapsto a_T, \\
& (f: a \to b) \mapsto (\eta_b \circ f)_T\\
G_T&: \mathcal{C}_T \to \mathcal{C}, \\
& a_T \mapsto Ta,
\\& (f: a \to Tb)_T \mapsto \mu_b \circ Tf
\end{align*}
$F_T$ a functor.
$$f: a \to b, g: b \to c$$
\begin{align*}
F_T(\text{id}_a) &= (\eta_a)_T \equiv \text{id}_{a_T} \\
F_T(g \circ f) &= (\eta_c \circ g \circ f)_T \\
F_T(g) \circ_T F_T(f) &= (\eta_c \circ g)_T \circ_T (\eta_b \circ f)_T \\
&= (\mu_c \circ T(\eta_c \circ g) \circ \eta_b \circ f)_T
\end{align*}
$\leadsto$
$$\mu_c \circ T(\eta_c) \circ Tg \circ \eta_b \stackrel{?}{=} \eta_c \circ g,$$
\underline{right unit triangle}, and \underline{naturality of $\eta$}

$F_T \dashv G_T$, in that $(F_T, G_T, \eta)$ adjunction in the \underline{universal arrow sense}.

_Adjunction_

$$(F, G, \eta)$$
$$\eta: \text{Id}_\mathcal{C} \Rightarrow GF,$$
$$\forall c \in \mathcal{C},~d \in \mathcal{D},~f: c \to Gd,~\exists!~g: Fc \to d,$$
s.t.
\begin{figure}[H]
\centering
\begin{tikzcd}
c \arrow[r, "\eta_c"] \arrow[rd, "f"']  & GFc \arrow[d, "Gg"] \\
& Gd
\end{tikzcd}
\end{figure}

\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[r, "\eta_a"] \arrow[rd, "f"'] & Ta \arrow[d, "\mu_b \circ Tg"] \\
  & Tb
\end{tikzcd}
\end{figure}
Using the \underline{left unit triangle}, _sufficient and necessary_:
$$g_T \equiv f_T.$$

\underline{counit}:
$$\epsilon_{b_T} \equiv (\text{id}_{Tb})_T: (Ta)_T \to a_T$$
$$T \equiv G_T F_T,$$
\begin{align*}
\mu_a' = G_T(\epsilon_{F_T a}) &= G_T(\epsilon_{a_T})\\
&= G_T((\text{id}_{Ta})_T) \\
&= \mu_a \circ T (\text{id}_{Ta}) = \mu_a
\end{align*}

**Monads and functional programming**

`a -> T b`

**IO**

```haskell
print :: Show a => a -> IO ()
putStr :: String -> IO ()
getLine :: IO String
getChar :: IO Char
```

**Data structures**

- `a -> Maybe b`: a function that **may fail**.
- `a -> [b]`: a function that **may produce zero or more results**.

**Logging**

```haskell
data Logger m a = Logger (a, m)
```
- `a -> Logger String b`: a function that **may log a string**.

**State**

```haskell
data State s a = State (s -> (a, s))
```
- `a -> State s b`: a function that **uses and/or manipulates a state**.

In these examples, the _contexts_ are

- `Maybe`: failure that gets propagated
- `[]`: arbitrary number of results that are gathered
- `Logger s`: a log of type `s` that is maintained
- `State s`: a state of type `s` that is passed around
