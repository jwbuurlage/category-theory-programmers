# Lenses and other optics

Compound data structures (records, containers, tuples, sum types, ...) are the bread and butter of real-world programs. Tools for manipulating and accessing these compound data structures are collectively called _optics_. In the first part of this chapter, we will follow roughly [@Pickering].

The simplest way of accessing the components of these compounds is _viewing_ and _updating_ single components. Doing this naively is fairly simple. We have seen viewers for e.g. pair already:
```haskell
:t fst      -- (a, b) -> a
fst (5, 3)  -- 5
snd (3, 1)  -- 1
```
Writing updaters for pairs is fairly easy too:
```haskell
fst' :: c -> (a, b) -> (c, b)
fst' x (y, z) = (x, z)
```
Looking at the type signatures, we can generalize what we mean by the accessors _view_ and _update_. If `s` is a compound data structure, and `a` the type of a component in that structure, then these accessors are functions of type:
```haskell
view :: s -> a
update :: a -> s -> s
```
We can easily generalize the update function even further, by allowing the update to take a different type `b` and replacing the component of type `a`. If we allow this, then the compound is not necessarily the same after the update, so we also have to allow for a different compound type. 
```haskell
view :: s -> a
update :: b -> s -> t
```
Collecting these together in a single data structure defines the simplest version of a _lens_:
```haskell
data Lens a b s t = Lens { view :: s -> a,  update :: b -> s -> t }
```
Our `fst, fst'` example for pairs can be put into a Lens:
```haskell
_1 = Lens fst fst'
view _1 (3, 5)     -- 3
update _1 2 (3, 5) -- (2, 5)
```
We can also make 'virtual lenses' not necessarily corresponding to concrete
components, such as this _positive_ lens:
```haskell
positive :: Lens Bool Bool Integer Integer
positive = Lens view update
  where
    view = (>= 0)
    update b = if b then (abs x) else (-(abs x))
```
Let us add some syntactic sugar:
```haskell
(^.) = flip view
(2, 3)^._1      -- 2
1200^.positive  -- True
```

Another optical example is a _prism_. Prisms are to sum types as lenses are to
product types. It consists of two components, _match_ which obtains a component
if it is being held by the sum type (variant), and _build_ that creates a variant out of
a component. If match fails, it returns the original variant.
```haskell
match :: s -> Either s a
build :: a -> s
```
Generalizing, we can return or build a variant `t` different from `s`, and we
can use a different type `b` to build a variant.  Hence, we define a prism as:
```haskell
data Prism a b s t = Prism { match :: s -> Either t a
                           , build :: b -> t }
```
There is a whole zoo of optics, and we will give more examples after introducing
a better framework. The problem with using the lenses and prisms as given in the this section, is that they do not easily compose (among themselves, or with each other). In the remainder of this chapter, we will study these lenses and other examples of optics in more detail, and put them into a unifying framework which will allow us to compose them.

## Profunctor optics

It turns out that the notion of a _profunctor_ can be used to define and
construct optics in a composable way. In category theory, these are defined as
follows.

\begin{definition}
Let $\mathcal{C}$ and $\mathcal{D}$ be categories. A \textbf{profunctor}:
$$\phi: \mathcal{C} \nrightarrow \mathcal{D}$$
is a functor $\mathcal{D}^{\text{op}} \times \mathcal{C} \to \mathbf{Set}$.
\end{definition}

The underlying theory for the material we will discuss here is interesting but
vast (in particular we would have to discuss monoidal categories and tensorial strengths). Therefore, we will take a pragmatic approach in this part, for once, and define most of the
concepts in Haskell directly. We can define profunctors as:
```haskell
data Profunctor
```
with laws (omitted here) making it a bifunctor that is contravariant in the first argument, and
covariant in the second.

The intuition behind `p a b` is that it takes values of type `a` to produce
values of type `b`. The simplest example of course, is a function `a -> b`, and
indeed we can define:
```haskell
instance Profunctor (->) where
    ...
```

Let us define our optics in this framework.

## Showing the equivalence

We started out with the following definition of a lens:
```haskell
data Lens a b s t = Lens { view :: s -> a,  update :: b -> s -> t }
```
And ended up with the profunctor definition:
```haskell
data LensP = ...
```
In this section, we will use the Yoneda lemma to show the equivalence between --
hopefully giving a deeper understanding of why they are equivalent (it is fairly
easy to construct an isomorphism between the two constructions).

## Further reading

- SPJ on lenses: <https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation>
- `lens` library: <https://github.com/ekmett/lens>
- Elegant profunctor optics: <http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf>
- Glassery: <http://oleg.fi/gists/posts/2017-04-18-glassery.html>
- Van Laarhoven lenses: <https://www.twanvl.nl/blog/haskell/cps-functional-references>
- Also a number of blogposts by Bartosz Milewski.
