# Lenses and other optics

_(This brief chapter only covers the very basics of lenses and other optics, see the suggested list of literature for further reading at the end)_

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
a better framework. The problem with using the lenses and prisms as given in the this section (in _concrete_ representations), is that they do not easily compose already with other optics of the same types, let alone when optics are mixed. In the remainder of this chapter, we will study these lenses and other examples of optics in more detail, and put them into a unifying framework which will allow us to compose them.

## Profunctor optics

It turns out that the notion of a _profunctor_ can be used to define and
construct optics in a composable way. In category theory, these are defined as
follows.

\begin{definition}
Let $\mathcal{C}$ and $\mathcal{D}$ be categories. A \textbf{profunctor}:
$$\phi: \mathcal{C} \nrightarrow \mathcal{D},$$
is a functor $\mathcal{D}^{\text{op}} \times \mathcal{C} \to \mathbf{Set}$.
\end{definition}

The underlying theory for the material we will discuss here is interesting but
vast (in particular we would have to discuss monoidal categories and tensorial strengths). Therefore, we will take a pragmatic approach in this part, for once, and define most of the
concepts in Haskell directly. We can define profunctors as:
```haskell
class Profunctor p where
   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
```
with laws (omitted here) making it a bifunctor that is contravariant in the first argument, and
covariant in the second.

The intuition behind `p a b` is that it takes values of type `a` to produce
values of type `b`. The simplest example of course, is a function `a -> b`, and
indeed we can define:
```haskell
instance Profunctor (->) where
    dimap f g h  = g . h . f
```
Different classes of optics, correspond to different constraints on our functors. In this exposition, we focus on Cartesian, and coCartesian profunctors.

```haskell
class Profunctor p => Cartesian p where
    first :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)

class Profunctor p => CoCartesian p where
    left :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)
```

An intuition for Cartesian profunctors is that they transform an `a` into a `b`, but can carry along any contextual information of type `c`. Similarly, coCartesian profunctors that can turn an `a` into a `b`, can also take care of the respective sum types with `c` (e.g. by not transforming values the values in that component). The function arrow is both Cartesian and coCartesian.

```haskell
cross f g (x, y) = (f x, g y)

plus f _ (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)

instance Cartesian (->) where
    first h = cross h id
    second h = cross id h

instance CoCartesian (->) where
    left h = plus h id
    right h = plus id h
```

Let us define our two example optics, lenses and prisms, in this framework. After giving the definitions, we analyse what we gained exactly by using our new representation. First, any optic is a transformations between profunctors. 
```haskell
type Optic p a b s t = p a b -> p s t
```
A _lens_ is an optic that works uniformly for all Cartesian profunctors.
```haskell
type LensP a b s t = forall p. Cartesian p => Optic p a b s t
```
We can turn any _concrete_ lens into this representation, using the following function:
```haskell
lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = dimap (fork v id) (uncurry u) . first
  where fork f g x = (f x, g x)
```
Similarly, we can define Prisms in terms of transformations of coCartesian profunctors.
```haskell
type PrismP a b s t = forall p. Cocartesian p => Optic p a b s

prismC2P :: Prism a b s t -> PrismP a b s t
prismC2P (Prism m b) = diamp m (either id b) . right
```
In summary, with the 'concrete to profunctor' functions `lensC2P` and `prismC2P` (which, as it turns out, have inverses) we can turn any concrete lens into the (less intuitive) profunctor representation. Once they are in this representation, they compose beautifully using the standard composition operator `(.)` which means that it even looks like imperative code where nested accessors are usually written with dots in between.

As the final note of this section, we mention that with prisms and lenses we are only scratching the surface. There are other optics (in particular adapters and traversals) that can fit into this framework. 

## Further reading

- Elegant profunctor optics: <http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf>
- Van Laarhoven lenses: <https://www.twanvl.nl/blog/haskell/cps-functional-references>
- A great many blogposts by Bartosz Milewski, e.g.:
  - Showing that the concrete/profunctor equivalence is Yoneda in disguise: <https://bartoszmilewski.com/2016/09/06/lenses-yoneda-with-adjunctions/>
  - A detailed categorical look at lenses <https://bartoszmilewski.com/2017/07/07/profunctor-optics-the-categorical-view/>
- Glassery: <http://oleg.fi/gists/posts/2017-04-18-glassery.html>
- SPJ on lenses: <https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation>
- `lens` library: <https://github.com/ekmett/lens>
