# Comonads

We have established some interesting notions such as monads and $F$-algebras, but we have not yet looked at their dual statements. In this chapter we will remedy this. Now that we have grown to appreciate the usefulness of monads, we will first us explore the notion of a _comonad_.

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

## Comonads in Haskell

- Stream

- Store

## Comonads resemble objects

Gabriel Gonzalez' blog post

## References

- <http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html>

