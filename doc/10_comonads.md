# Comonads

_(This brief chapter only covers the very basics of comonads, see the suggested list of literature for further reading at the end)_

We have established some interesting notions such as monads and $F$-algebras, but we have not yet looked at their dual statements. In this chapter we will partly remedy this shortcoming.

Over the last couple of chapters, we have grown to appreciate the usefulness of monads. Here, we will explore the dual notion of a _comonad_.

## Definition

As with all dual notions, we can simply say that a comonad is a monad on $\mathcal{C}^{\text{op}}$. But let us give the definition here explicitely:

\begin{definition}
A \textbf{comonad} $W = (T, \epsilon, \delta)$ over a category $\mathcal{C}$, consists of
an endofunctor $T: \mathcal{C} \to \mathcal{C}$ together with natural
transformations:
\begin{align*}
\epsilon&: T \Rightarrow \text{Id}\\
\delta&: T \Rightarrow T^2
\end{align*}
so that the following diagrams commute:

\begin{figure}[H]
\centering
\begin{tikzcd}
T^3 \arrow[d, Leftarrow, "T\delta"'] \arrow[r, Leftarrow, "\delta T"] & T^2 \arrow[d, Leftarrow, "\delta"] \\
T^2 \arrow[r, Leftarrow, "\delta"] & T
\end{tikzcd}
\end{figure}

\begin{figure}[H]
\centering
\begin{tikzcd}
T \arrow[r, Leftarrow, "\epsilon T"] \arrow[dr, Leftarrow, "\text{id}"'] & T^2 \arrow[d, Leftarrow, "\delta"] & \arrow[l, Leftarrow, "T \epsilon"'] \arrow[dl, Leftarrow, "\text{id}"] T\\
  & T &
\end{tikzcd}
\end{figure}
\end{definition}

We call $\epsilon$ the _counit_, and $\delta$ the _comultiplication_. These are called `extract` and `duplicate` in Haskell.

We note that by duality every adjunction $F \dashv G$ gives rise to a comonad on $\mathcal{D}$. Conversely, every comonad arises from an adjunction (by factoring through e.g. the co-Kleisli category). The dual of an $F$-algebra is an $F$-coalgebra, and is given by an arrow $a \to Fa$. These form a category $\textbf{Coalg}_F$.

## Comonads in Haskell

For our purposes, we will define comonads in Haskell using the following typeclass:
```haskell
class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
```
We also have the equivalent of bind, which is usually called `extend`:
```haskell
extend :: Comonad w => (w a -> b) -> w a -> w b
extend f = fmap f . duplicate
```
Of course, we can also define duplicate in terms of `extend`:
```haskell
duplicate = extend id
```

Before we look at the Haskell versions of the comonad laws, first let us build some intuition by looking at two examples:

**Stream**

A _stream_ is like an infinite cons-list. It is defined as:
```haskell
data Stream a = Cons a (Stream a)
```
Being so similar to a list, it is clear that stream is a functor:
```haskell
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```
More interestingly, it is also a comonad:
```haskell
instance Comonad Stream where
    extract (Cons x _) = x
    duplicate (Cons x xs) = Cons (Cons x xs) (duplicate xs)
```
The `extract` function is clear, it simply takes out the _head_ of the stream.

The `duplicate` method is more interesting, it creates and infinite number of streams, each _focused_ around (that is to say, 'has at its head') different elements of the original stream. That is to say, the result represents all possible tails of the stream.

Note that the expressive power gained by working with infinite lists of infinite lists is brought to us by the laziness of Haskell.

This example immediately suggests a way of looking at (this specific class of) comonads, namely as a container of values with one _distinguished_ value (the one returned by extract), and a duplicate function that can be used to _shift the focus_ (that is to say, change which value is distinguished).

**Store**

Store is dual to the `State` monad^[What I mean here, is that the adjunction that gives rise to the `State` monad, also gives rise to the `Store` comonad.], and is defined as:
```haskell
data Store s a = Store (s -> a) s
```
We interpret a `Store` as follows. The first part, with signature `(s -> a)` of a store, can be seen as a container with values of type `a`, which are _keyed_ by elements of type `s`, so the container is a _dictionary_. Indeed, for any key `x :: s`, we can use the first component to obtain a value of type `a`.

The second part of type `s` defines the _focus_ of our store, the distinguished element is the one keyed by this value.

Store is a functor, by using composition:
```haskell
instance Functor (Store s) where
    fmap f (Store e x) = Store (f . e) x
```
It is also a comonad, with the definition:
```haskell
instance Comonad (Store s) where
    extract (Store f x) = f x
    duplicate (Store f x) = Store (Store f) x
```
The semantics of `extract` are clear and should not be surprising, we simply obtain the distinguished value.

The implementation of `duplicate` is harder to digest. Let us first interpret the specialized signature (parentheses added for emphasis):
```haskell
duplicate :: (Store s) a -> (Store s) ((Store s) a)
-- equivalently
duplicate :: Store s a -> Store s (Store s a)
```
In other words, we are given a dictionary with a distuingished key, keys of type `s` and values of type `a`. From `duplicate`, we obtain a _dictionary of dictionaries_, with a distuinguished dictionary, keys of type `s`, and values which are dictionaries. Quite a mouthful!

Intuitively, this distuinguished dictionary should correspond to our original dictionary. The other dictionaries indexed with a key `x :: s`, should be focused around the element with key `x` of our original dictionary!

We can achieve this by partially evaluating our `Store` constructor. Observe that:
```haskell
Store f :: s -> Store s a
```
takes a key `x :: s`, and gives us back a store with dictionary `f`, and distinguished key `x`. By leveraging this function, we see that the dictionary in the `Store` returned by `duplicate`, takes a key and returns a `Store s a`, focused on that key, as required.

As we will see later, the `Store` comonad is an important ingredient of the magnificant `lens` library.

## Comonad laws in Haskell

We will consider the following form of the laws that a comonad should satisfy in
Haskell.

```haskell
extend extract      = id                     -- (1)
extract . extend f  = f                      -- (2)
extend f . extend g = extend (f . extend g)  -- (3)
```

Let us discuss the intuition behind these laws, specifically for the case of
comonadic containers. We take the viewpoint
that extend applies a function to each element of the comonad, but this
'comonad valued function' can depend on other elements in the comonad. For (1), we simply
extract the value at each component of the comonad and then gather the
results, which should clearly be equivalent to doing nothing. Law (2) states
that when extending a comonad valued function over a comonad, and observing what it did to
the focused element is the same as just evaluating the function on the comonad.
Finally, (3) says that composing extended comonad valued functions can be done
either before or after extending the second function.

## References

- [@Milewski2014, \S 3.7]
- [@Awodey, \S 10.4]
- [@Riehl2016, \S 5.2]
- <http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html>
- <https://bartoszmilewski.com/2017/01/02/comonads/>
- <https://www.youtube.com/watch?v=F7F-BzOB670> spreadsheets (see also <https://github.com/kwf/ComonadSheet>)
- Game of Life / diagram: <https://github.com/bollu/cellularAutomata>, <http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html>
