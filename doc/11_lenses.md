# Lenses and other optics

Compound data structures (records, containers, tuples, sum types, ...) are the bread and butter of real-world programs. Tools for manipulating and accessing these compound data structures are collectively called _optics_.

The simplest way of accessing the components of these compounds is _viewing_ and _updating_ single components. Doing this naively is fairly simple. We have seen viewers for e.g. pair already:
```haskell
:t fst      -- (a, b) -> a
fst (5, 3)  -- 5
snd (3, 1)  -- 1
```
Writing updaters for pairs is fairly easy too:
```haskell
updateFst :: c -> (a, b) -> (c, b)
updateFst x (y, z) = (x, z)
```
Looking at the type signatures, we can generalize what we mean by _view_ and _update_. If `s` is a compound data structure, and `a` the type of a component in that structure, then accessors to this component have the type:
```haskell
view :: s -> a
update :: a -> s -> s
```
We can easily generalize the update function even further, by allowing the update to take a different type `b` and replacing the component of type `a`. If we allows this, then the compound is not necessarily the same after the update, so we also have to allow for a different compound type. 
```haskell
view :: s -> a
update :: b -> s -> t
```
Collecting these together in a single data structure defines the simplest version of a _lens_:
```haskell
data Lens a b s t = Lens { view :: s -> a,  update :: b -> s -> t }
```
Our `fst, updateFst` example for pairs can be put into a Lens:
```haskell
_1 = Lens fst updateFst
view _1 (3, 5) -- 3
update _1 2 (3, 5) -- (2, 5)
```

- SPJ on lenses: <https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation>
- `lens` library: <https://github.com/ekmett/lens>
- Elegant profunctor optics: <http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf>
- Glassery: <http://oleg.fi/gists/posts/2017-04-18-glassery.html>
- Van Laarhoven lenses: <https://www.twanvl.nl/blog/haskell/cps-functional-references>
- Also a number of blogposts by Bartosz Milewski.
