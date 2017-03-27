# Monads

\epigraph{"Mathematics is the art of giving the same name to different things"}{\emph{Henri Poincar\'e}}

Monads are used all throughout functional programming. In this part, we will try to understand them by first studying their mathematical definition and properties. Afterwards, we describe their use in functional programing by giving a number of motivating examples.

Any endofunctor $T: \mathcal{C} \to \mathcal{C}$ can be composed with itself, to obtain e.g. $T^2$ and $T^3$ (which are both again endofunctors from $\mathcal{C}$ to $\mathcal{C}$. A monad concerns an endofunctor, together with natural transformation between this functor and its composites that give it a "monoid-like structure".

## Monads over a category

Say $\alpha$ is a natural transformation $T \Rightarrow T'$, where $T, T'$ are endofunctors of $\mathcal{C}$, then note that $\alpha_x$ is a morphism from $Tx \to T'x$ in the category $\mathcal{C}$. Since this is a morphism, we can use $T$ or $T'$ to lift it, i.e. we obtain arrows at components $(T \alpha)_a \equiv T (\alpha_a)$ and $(\alpha T)_a \equiv \alpha_{Ta}$.

In particular, note that this defines natural transformations between the appropriate composite functors since the image of any commutative diagram under a functor is again commutative.

We are now ready to dive into the definition of a monad:

\begin{definition}
A \textbf{monad} $M = (T, \eta, \mu)$ over a category $\mathcal{C}$, consists of
an endofunctor $T: \mathcal{C} \to \mathcal{C}$ together with natural
transformations:
\begin{align*}
\eta&: \text{Id} \Rightarrow T\\
\mu&: T^2 \Rightarrow T
\end{align*}
so that the following diagrams commute:

\begin{figure}[H]
\centering
\begin{tikzcd}
T^3 \arrow[d, Rightarrow, "T\mu"'] \arrow[r, Rightarrow, "\mu T"] & T^2 \arrow[d, Rightarrow, "\mu"] \\
T^2 \arrow[r, Rightarrow, "\mu"] & T
\end{tikzcd}
\end{figure}

\begin{figure}[H]
\centering
\begin{tikzcd}
T \arrow[r, Rightarrow, "\eta T"] \arrow[dr, Rightarrow, "\text{id}"'] & T^2 \arrow[d, Rightarrow, "\mu"] & \arrow[l, Rightarrow, "T \eta"'] \arrow[dl, Rightarrow, "\text{id}"] T\\
  & T &
\end{tikzcd}
\end{figure}
The first of these is called the \emph{associativity square} while the two triangles in second diagram are called the \emph{unit triangles}.
\end{definition}
We call $\eta$ the _unit_, and $\mu$ the _multiplication_. Let us look at a familiar example:
\begin{example}[Power-set monad]
Let $\mathcal{P}$ be the power-set functor that we defined before. We define $\eta$ as the natural transformation:
$$\eta: \text{Id} \Rightarrow \mathcal{P},$$
with components that send elements to the singleton set corresponding to that element:
$$\eta_A: A \rightarrow \mathcal{P}(A),~a \mapsto \{ a \}.$$
We define $\mu$ as the natural transformation:
$$\mu: \mathcal{P}^2 \to \mathcal{P},$$
with components that send each set of sets, to the union of those sets.
$$\mu_A: \mathcal{P}(\mathcal{P}(A)) \to \mathcal{P}(A),~\{ B_1, B_2, \ldots \} \mapsto \bigcup B_i,$$
where $B_i \subseteq A$.
\end{example}

### Adjunctions give rise to monads

Let $(F, G, \eta, \epsilon)$ be a unit-counit adjunction. We have a functor:
$$T \equiv GF: \mathcal{C} \to \mathcal{C}.$$
We can define a natural transformation:
$$\mu: T^2 \Rightarrow T, \mu_c \equiv G(\epsilon_{Fc}).$$
Let us show that $(T, \eta, \mu)$ indeed forms a monad, first the associtivity square:
\begin{figure}[H]
\centering
\begin{tikzcd}
GFGFGF \arrow[d, Rightarrow, "GF\mu"'] \arrow[r, Rightarrow, "\mu GF"] & GFGF \arrow[d, Rightarrow, "\mu"] \\
GFGF \arrow[r, Rightarrow, "\mu"] & GF
\end{tikzcd}
\end{figure}
Looking at this diagram in terms of components and substituting in the definition of $\mu$ we obtain
\begin{figure}[H]
\centering
\begin{tikzcd}
GFGFGFc \arrow[d, "GFG(\epsilon_{Fc})"'] \arrow[r, "G(\epsilon_{FGFc})"] & GFGFc \arrow[d, "G(\epsilon_{Fc})"] \\
GFGFc \arrow[r, "G(\epsilon_{Fc})"] & GFc
\end{tikzcd}
\end{figure}
written more suggestively we write: $a \equiv FGFc, b \equiv Fc$ and $\tilde{G} \equiv GFG$,
\begin{figure}[H]
\centering
\begin{tikzcd}
\tilde{G}a \arrow[d, "\tilde{G}(\epsilon_{b})"'] \arrow[r, "G(\epsilon_{a})"] & Ga \arrow[d, "G(\epsilon_{b})"] \\
\tilde{G}b \arrow[r, "G(\epsilon_{b})"] & Gb
\end{tikzcd}
\end{figure}
such that the diagram reveals itself to be a naturality square under the function $f \equiv \epsilon_b: a \to b$ for the natural transformation $G\epsilon$.

For e.g. the left unit triangle we observe:

\begin{figure}[H]
\centering
\begin{tikzcd}
GFc \arrow[rd, "\text{id}_{GFc}"'] \arrow[r, "\eta_{GFc}"] & GFGFc \arrow[d, "G(\epsilon_{Fc})"] \\
 & GFc
\end{tikzcd}
\end{figure}
Which is just the second triangle identity of the adjunction at the object $Fc$.

### Kleisli categories

Every monad defines a new category, called the *Kleisli category*.

\begin{definition}
Let $\mathcal{C}$ be a category, and let $(T, \eta, \mu)$ be a monad over this category. Then the Kleisli category $\mathcal{C}_T$ is the category where:
\begin{itemize}
\item The \emph{objects} of $\mathcal{C}_T$ $a_T$ correspond directly to the objects $a$ of $\mathcal{C}$.
\item The \emph{arrows} of $\mathcal{C}_T$ are the arrows of the form $f: a \to T b$ in $\mathcal{C}$, and will be denoted $f_T$. In other words,
$$\text{Hom}_{\mathcal{C}_T}(a_T, b_T) \simeq \text{Hom}_{\mathcal{C}}(a, Tb).$$
\item Composition between two arrows $f_T: a_T \to b_T$ and $g_T: b_T \to c_T$ in $\mathcal{C}_T$ is given by:
$$g_T \circ_T f_T \equiv (\mu_c \circ Tg \circ f)_T.$$
\item The \emph{identity arrows} $\text{id}_{a_T}$ are equal to $(\eta_a)_T$.
\end{itemize}
\end{definition}

Let us show that this indeed forms a category. In particular we have to show that the composition operator is associative and unital. For the former, we look at the following situation

\begin{figure}[H]
\centering
\begin{tikzcd}
a_T \arrow[r, "f_T"] & b_T \arrow[r, "g_T"] & c_T \arrow[r, "h_T"] & d_T
\end{tikzcd}
\end{figure}
the left associative and right associative expressions are:
\begin{align*}
(h_T \circ_T g_T) \circ_T f_T &= (\mu_d \circ Th \circ g)_T \circ_T f_T \\&= (\mu_d \circ T(\mu_d \circ Th \circ g) \circ f)_T,\\
h_T \circ_T (g_T \circ_T f_T) &= h_T \circ_T (\mu_c \circ Tg \circ f)_T \\&= (\mu_d \circ Th \circ \mu_c \circ Tg \circ f)_T,
\end{align*}
so it is enough to show that:
$$\mu_d \circ T \mu_d \circ T^2 h = \mu_d \circ Th \circ \mu_c,$$
which holds because of the associativity square and the naturality of $\mu$:
\begin{align*}
\mu_d \circ T \mu_d \circ T^2 h &= \mu_d \circ \mu_{Td} \circ T^2 h \\
&= \mu_d \circ Th \circ \mu_c
\end{align*}
To show that it is e.g. left-unital we compute:
\begin{align*}
\text{id}_{b_T} \circ_T f_T = (\mu_b \circ T(\eta_b) \circ f)_T = f_T
\end{align*}
where we use the right unit triangle of the monad:
$$\mu_b \circ T \eta_b = \text{id}_b$$
Understanding Kleisli composition can be a convenient stepping stone to understanding how to work with Monads in Haskell.
The composition operator $\circ_T$ is usually denoted `>=>` (the fish operator) in Haskell.

### Every monad is induced by an adjunction

Let $\mathcal{C}$ be a category, $(T, \eta, \mu)$ a monad over $\mathcal{C}$, and $\mathcal{C}_T$ the associated Kleisli category. Here, we will show that there are functors $F: \mathcal{C} \to \mathcal{C}_T$ and $G: \mathcal{C}_T \to \mathcal{C}$ so that $F \dashv G$ and $T$ is equal to the monad induced by that adjunction.

We define:
$$\begin{array}{lll}
F_T: \mathcal{C} \to \mathcal{C}_T, & a \mapsto a_T, & (f: a \to b) \mapsto (\eta_b \circ f)_T\\
G_T: \mathcal{C}_T \to \mathcal{C}, & a_T \mapsto Ta, & (f: a \to Tb)_T \mapsto \mu_b \circ Tf
\end{array}$$
Let us check that e.g. $F_T$ is actually a functor. Consider two arrows in $\mathcal{C}$, $f: a \to b$ and $g: b \to c$.
\begin{align*}
F_T(\text{id}_a) &= (\eta_a)_T \equiv \text{id}_{a_T} \\
F_T(g \circ f) &= (\eta_c \circ g \circ f)_T \\
F_T(g) \circ_T F_T(f) &= (\eta_c \circ g)_T \circ_T (\eta_b \circ f)_T = (\mu_c \circ T(\eta_c \circ g) \circ \eta_b \circ f)_T
\end{align*}
So we have to show that:
$$\mu_c \circ T(\eta_c) \circ Tg \circ \eta_b \stackrel{?}{=} \eta_c \circ g,$$
which is immediate from the right unit triangle, and the naturality of $\eta$.

Next we show that $F_T \dashv G_T$, in that $(F_T, G_T, \eta)$ (we take the unit of the adjunction to be equal to the unit of the monad) forms an adjunction in the universal arrow sense. We have to show that for each $f: a \to Tb$ there is a unique $g_T: a_T \to b_T \equiv (g: a \to Tb)_T$ so that the following diagram commutes:
\begin{figure}[H]
\centering
\begin{tikzcd}
a \arrow[r, "\eta_a"] \arrow[rd, "f"'] & Ta \arrow[d, "\mu_b \circ Tg"] \\
  & Tb
\end{tikzcd}
\end{figure}
Using the left unit triangle, we obtain that it is sufficient and necessary to take simply $g_T \equiv f_T$!

The counit of the adjunction is given by $\epsilon_{b_T} \equiv (\text{id}_{Tb})_T: (Ta)_T \to a_T$. We have $T \equiv G_T F_T$, and we have that
$$G_T(\epsilon_{F_T a}) = G_T(\epsilon_{a_T}) = G_T((\text{id}_{Ta})_T) = \mu_a \circ T (\text{id}_{Ta}) = \mu_a$$
as required.

## Monads and functional programming

Because the brutal purity of Haskell is restrictive, we need non-standard tools to perform operations that we take for granted in imperative languages. In this section, we will explore what this means for some real world programs, and discover what problems and difficulties pop up. In particular, we will see how we can use monads to overcome some of these problems, by showing that functions of the type `a -> T b` are common, and hence that we are in need of a nice way to compose them.

### IO

Consider the type of some functions in Haskell regarding input and output in Haskell:

```haskell
print :: Show a => a -> IO ()
putStr :: String -> IO ()
getLine :: IO String
getChar :: IO Char
```

this allows us to do I/O as in the following snippet:
```haskell
main = do
  x <- getLine
  y <- getLine
  print (x ++ y)
```

Let us consider this snippet of code carefully. If `main` should *behave* purely, then it should return the same function every time. However, since we would like to support user input (`cin, scanf, getLine, ..)` so what should be its type if it should 'behave mathematically'? Similarly, for `print`, what would be its type? It should take a value, convert it to a string, and output this in a terminal somewhere. What is the *type* of *printing to screen*?

In Haskell, this is done using *IO actions*. This monadic style of doing IO is not limited to input/output for terminal, it can also be network related, file related, or mouse/keyboard input for a video game!

An IO action is a *value* with a type of `IO a`. We can also have an 'empty IO action', if the result is not used. The way to look at these actions is as a _recipe_ of producing an `a`. While the actual value produced by the action depends on the outside world, the _recipe_ itself is completely _pure_.

Let us consider our examples:

- The print function has the signature from a String to an IO action:
    ```haskell
    putStrLn :: String -> IO ()
    ```
    To print the value of any type, we precompose this function with `show :: a -> String`.
- The function `main` itself is an IO action! So the type is  `main :: IO ()`.
- The `getLine` function is an IO action `getLine :: IO String`.


**Case study: Handling input**

Let us consider a very simple example using `getLine` and `print`.

```haskell
f :: Int -> Int
f x = 2 * x

-- attempt 1
main = print $ f (read getLine :: Int)
```

But this does not type check! First, the action `getLine` has type `IO String`, while `read` expects `String`. Then to work on the IO action, we want to *lift* `read` to take an `IO String` and produce an `IO Int`. This sounds like an `fmap`, and indeed `IO` provides fmap, it is a functor!
```haskell
-- attempt 2
main = print $ f (read <$> getLine :: IO Int)
```
Next, we have that `f` expects an `Int`, not an `IO Int`, so we lift it again
```haskell
-- attempt 3
main = print $ f <$> (read <$> getLine :: IO Int)
```
The `print`^[`print` actually corresponds to `(putStrLn . show)` in Haskell] statement we used here has signature `a -> IO ()`. Bringing this into the `IO` context using an `fmap` gives us:
```haskell
fmap print :: IO a -> IO (IO ())
```
Since `main` should corespond to `IO ()`, we need either a way to remove a 'nested IO tag', or we need a function for functors that only lifts the first argument. In other words, let `F` be a functor, then we require either:

```haskell
join :: F (F a) -> F a
(=<<) :: (a -> F b) -> (F a -> F b)
-- the above function is more commonly used with swapped arguments
-- and is then pronounced 'bind'
(>>=) ::  F a -> (a -> F b) -> F b)
```

Note, that we can define:
```haskell
join :: F (F a) -> F a
join x = x >>= id
```
so that implementing `>>=` is enough. Conversely, we can also retrieve bind from `join` and `fmap`:
```haskell
x >>= f = join (f <$> x)
```
Note also that we can pack an object inside an IO 'container':
```haskell
return :: a -> IO a
```

Let us return to IO, and see what this notation gives us:
```haskell
main = getLine >>= putStrLn
```
This code results in an empty action `IO ()`, so the 'bind' function can be used to chain `IO` operations together! For our little toy program we can `fmap` print into the `IO` context, and `join` the result afterwards to obtain:
```haskell
main = join $ print <$> f <$> (read <$> getLine :: IO Int)
```
Using a more idiomatic style of writing this programming we get:
```haskell
main = read <$> getLine >>= (\x -> print (f x))
```
which in `do`-notation becomes:
```haskell
main = do
    x <- read <$> getLine
    print (f x)
```
To summarize, `>>=`, `join` and `return` allow us to **compose functions that may or may not require IO operations**.

### Other examples

Now that we have seen how to compose functions of the form `a -> T b`, let us look at some other examples of contexts where this structure can be found.

**Data structures**

- `a -> Maybe b`: a function that **may fail**.
- `a -> [b]`: a function that **may produce zero or more results**.

**Logging**

All of `IO`, `Maybe` and `[]` may be seen as 'functional containers', let consider a different kind of example.

```haskell
data Logger m a = Logger (a, m)
```

The data type `Logger` consists of a _composable log_ (in the form of a monoid, e.g. `(String, (++))`) `m`, and an embedded value `a`.

- `a -> Logger String b`: a function that **may log a string**.

**State**

```haskell
data State s a = State (s -> (a, s))
```

In this view, a value of type `State s a` is a function that takes some state, and produces an `a` in addition to a (possibly modified) state. For example, `s` could be some _environment_ (say a `Map`) containing information that can be used to produce an `a`, and the state function can manipulate this `Map` when producing the `a`.

- `a -> State s b`: a function that **uses and/or manipulates a state**.

In these examples, the _contexts_ are

- `Maybe`: failure that gets propagated
- `[]`: arbitrary number of results that are gathered
- `Logger s`: a log of type `s` that is maintained
- `State s`: a state of type `s` that is passed around

The bind `>>=` implementation for these _monads_ pass around this context, and can change the control depending on the result after a step. For example, it can short-circuit a computation inside the `Maybe` monad in case some function fails.

### The `Monad` type class

The triplets `(F, return, join)` that we have seen in this section, correspond to monads $(T, \eta, \mu)$ over the category of types. The type class in Haskell is defined as^[We simplify the definition slightly here, the actual class also defines a `fail` method which is seen as an historical flaw]:
```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```
We have seen that `>>=` can be defined in terms of `join`, which has the familiar type:
```haskell
join :: m (m a) -> m a
```
Indeed, `return` corresponds to a natural transformation `Identity -> m`, while `join` corresponds to a natural transformation between `m m -> m`.

## Exercises

\begin{exercise}
Show that the image of any commutative diagram under a functor $F$ is again commutative.
\end{exercise}

\begin{exercise}
Show that $G_T$ is a functor.
\end{exercise}

## References

- <https://golem.ph.utexas.edu/category/2012/09/where_do_monads_come_from.html>

- 6.1 and parts of 6.3 and 6.4 of Mac Lane
- Blogs:
    - <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/>
    - <https://bartoszmilewski.com/2016/11/30/monads-and-effects/>
    - <http://www.stephendiehl.com/posts/monads.html>
- Catsters

About IO:

- <https://wiki.haskell.org/Introduction_to_IO>

Some posts dealing specifically with Monads from a Haskell perspective:

- <http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html>
- <https://bartoszmilewski.com/2013/03/07/the-tao-of-monad/>
- <http://www.stephendiehl.com/posts/adjunctions.html>
- <https://www.reddit.com/r/haskell/comments/4zvyiv/what_are_some_example_adjunctions_from_monads_or/>


