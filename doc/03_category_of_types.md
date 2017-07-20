# Types and functions: a category for programmers

\epigraph{""A monad is a monoid in the category of endofunctors, what's the problem?"}{\emph{James Iry jokes about Haskell in his blog post A Brief, Incomplete, and Mostly Wrong History of Programming Languages}}

To establish a link between functional programming and category theory, we need to find a category that is applicable. Observe that a _type_ in a programming language, corresponds to a _set_ in mathematics. Indeed, the type `int` in C based languages, corresponds to some finite set of numbers, the type `char` to a set of letters like `'a'`, `'z'` and `'$'`, and the type `bool` is a set of two elements (`true` and `false`). This category, the category of types, turns out to be a very fruitful way to look at programming.

Why do we want to look at types? Programming safety and correctness. In this part we will hopefully give an idea of how category theory applies to programming, but we will not go into to much detail yet, this is saved for later parts.

We will take as our model for the category of Haskell types (**Hask**) the category **Set**. Recall that the elements of **Set** are sets, and the arrows correspond to maps. There is a major issue to address here: Mathematical maps and functions in a computer program are not identical (bottom value $\perp$). We may come back to this, but for now we consider **Set** and **Hask** as the same category.

In Haskell, we can express that an object has a certain type:

```haskell
    a :: Integer
```

In C++ we would write:

```cpp
    int a;
```

To define a function $f: A \to B$ from type $A$ to type $B$ in Haskell:

```haskell
    f :: A -> B
```

To compose:

```haskell
    g :: B -> C
    h = g . f
```

This means that `h` is a function `h :: A -> C`! Note how easy it is to compose functions in Haskell. Compare how this would be in C++, if we were to take two polymorphic functions in C++ and compose them:

```cpp
    template <typename F, typename G>
    auto operator*(G g, F f) {
        return [&](auto x) { return g(f(x)); };
    }


    int main() {
        auto f = [](int x) -> float { return ...; };
        auto g = [](float y) -> int { return ...; };

        std::cout << (g * f)(5) << "\n";
    }
```

We need some additional operations to truly turn it into a category. It is easy to define the identity arrow in Haskell (at once for all types):

```haskell
    id :: A -> A
    id a = a
```

in fact, this is part of the core standard library of Haskell (the Prelude) that gets loaded by default. Ignoring reference types and e.g. `const` specifiers, we can write in C++:

```cpp
    template <typename T>
    T id(T x) {
        return x;
    }
```

There is one issue we have glared over; in mathematics all functions are *pure*: they will always give the same output for the same input. This is not always the case for computer programs, using IO functions, returning the current date, using a global variable are all examples of impure operations that are common in programming. In Haskell, *all functions are pure*, and this is a requirement that allows us to make the mapping to the category **Set**. The mechanism that allows Haskell programs to still do useful things is powered by *monads*, which we will discuss later.

Although many of the things we will consider can apply to other languages (such as Python and C++), there is a strong reason why people often consider Haskell as an example in the context of category theory and programming; it originates in academia and therefore takes care to model the language more accurately. For example, since we take as our model the category **Set**, there should be a type that corresponds to the empty set $\emptyset$. In C / C++, the obvious candidate would be `void` for this set, but consider a function definition:
```cpp
void f() { ... };
```
This can be seen as a function from `void -> void`. We can call this function using `f()`, but what does it mean to call a function? We always invoke a function for an argument, so `void` actually corresponds to the set with a single element! Note that C functions that return void either do nothing useful (i.e. discard their arguments), or are impure. Indeed, even using a pointer argument to return a value indirectly modifies a 'global state'!  In Haskell, the type corresponding to the *singleton set* (and its single value) is denoted with `()`. Meaning that if we have a function:
```haskell
f :: () -> Int
```
we can invoke it as `f()`! Instead, the type `Void` corresponds to the empty set, and there can never be a value of this type. There is even a (unique) polymorphic (in the return type!) function that takes `Void` added to the prelude:
```haskell
absurd :: Void -> a
```
You may be tempted to discard the type `Void` as something that is only used by academics to make the type system 'complete', but there are a number of legitimate uses for `Void`. An example is *Continuation passing style*, or CPS, where functions do not return a value, but pass control over to another function:
```haskell
type Continuation a = a -> Void
```
In other words, a continuation is a function that *never returns*, which can be used to manipulate control flows (in a type-safe manner).

Recall that an initial object has exactly one arrow to each other object, and a terminal object has exactly one arrow coming from each other object. These objects are unique up to isomorphism. In the category of types, they correspond to `Void` and `()` respectively.

To summarize this introduction, in the category of 'computer programs', types are objects, and *pure* functions between these types are arrows. Next, we consider how we can apply some of the concepts we have seen, such as functors and natural transformations, to this category.

## Containers as functors

When we consider functors in the category of types, the first question is 'to what category?'. Here, we will almost exclusively talk about functors from **Hask** to itself, i.e. _endofunctors_.

Endofunctors in **Hask** map types to types, and functions to functions. There are many examples of functors in programming. Let us first consider the concept of _lists of objects_, i.e. arrays or vectors. In C++ a list would be written as:
```cpp
std::vector<T> xs;
```
or in Python we would have;
```python
>>> import numpy as np
>>> a = np.array([1,2,3], dtype='int')
>>> type(a)
<class 'numpy.ndarray'>
>>> a.dtype
dtype('int64')
```
Note here that the true type of the numpy array is hidden inside the object, meaning its the responsiblity of the program to make sure that the types of operations match! The reason that we consider `numpy` arrays is that normal 'lists' in Python are actually _tuples_, which we will discuss when we talk about products and coproducts.

Let us consider the mathematical way of expressing this:

\begin{example}
Lists of some type are more generally called \textbf{words over some alphabet} (i.e. a set) $X$, and we denote the set of all finite words of elements\footnote{Also called the \emph{Kleene closure} of $X$} in $X$ as $X^*$. Elements in $X^*$ look like:
$$(x_1, x_2, x_3)$$
$$(x_1)$$
$$()$$
These are all examples of \emph{words} in $X$ (where the last example corresponds to the empty word). If we want to construct a \emph{word functor} $T$, then $T$ would then have the signature:
\begin{align*}
T&: X \to X^*\\
 &: (f: X \to Y) \mapsto (Tf: X^* \to Y^*)
\end{align*}
For this second option, we have an obvious candidate for the precise function, let $f: X \to Y$ be some map, then $Tf$ maps a word in $X$ gets to a word in $Y$ in the following way:
$$Tf(x_1, x_2, x_3, ... x_n) = (f(x_1), f(x_2), f(x_3), \ldots, f(x_n)).$$
\label{exa:kleene-closure}
\end{example}

**Type classes and type constructors**

We will express this idea in Haskell, but before we can do this we first have to consider type classes and -constructors. A _type constructor_ is a 'function' (on types, not an arrow) that creates a type out of a type. A _type constructor_ can have multiple _value constructors_, and these constructors can be differentiated between using something called _pattern matching_ which we will see later. As an example, consider `Bool`.
```haskell
data Bool = True | False
```
Here, we define the type constructor `Bool` as the resulting type corresponding to the _value_ given by the value constructors `True` and `False`, which both are nullary constructors (that take no argument as types!). Normally however, type constructors take one or multiple types for their value constructors:
```haskell
data Either a b = Left a | Right b
```
Here, the type constructor either hold either a value of type `a` or of type `b`, corresponding to the value constructors `Left` and `Right`. We will revisit this idea (and `Either`) when talk about products and coproducts.

A type class is a _common interface for types_. It defines a family of types that support the same operations. For example, a type class for objects that support equality is defined as:
```haskell
class Eq a where
    (==) :: a -> a -> Bool
```
If we want to express the concept^[In C++, type constructors are referred to as _concepts_, and they have been a long time coming (but are not yet in the standard)] _functor_ using a typeclass, we have to state that it can send types to types, and that it sends functions between two types to functions with the appropriate signature, i.e.:
```haskell
class Functor F where
    fmap :: (a -> b) -> F a -> F b
```
This says that `F` is a functor, if there is a function `fmap` that takes a function `f :: a -> b` and maps it to a function `fmap f :: F a -> F b`. Note that we do not explicitely have to state that `F` sends types to types, because this can be induced from the fact that we use `F a` where the compiler expects a type.

\subsection*{The List functor}

The _list functor_ in Haskell is denoted with `[]`, and a list of type `a` is denoted `[a]` (which is syntactic sugar, normally the type would be `[] a`).

Let us try to define this functor from the ground up. If we would write `List` instead of `[]`, then first we have to define what a list is. We can define this as follows:
```haskell
data List a = Nil | Cons a (List a)
```
Here the type constructor has two possible ways of constructing (partitioning the possible values of the type):  a list of `a`s is either empty (corresponding to the _constructor_ `Nil`), or that it is the concatenation (corresponding to the _constructor_ `Cons`) of an object of type `a` with another list of `a`s. Note that this is a recursive definition!

Next we define the `fmap` corresponding to our `List` functor (i.e. how it maps functions). The corresponding definition to the map described for the _word functor_ is:
```haskell
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x t) = Cons (f x) (fmap f t)
```
If a list is empty, then we get the empty set, otherwise we map the indivual values in the list recursively using the given `f`. In `C++` this `fmap` functor roughly corresponds to `std::transform`, while for Python the closest thing would be the `map` function. With these two definitions, `List` is a functor! We could check the that it satisfies the requirements.

As mentioned, `List` is implemented in the standard library as `[]`, and `Cons` is written as `:`, while the empty list is written also as `[]`. This allows you to write:
```haskell
x = 1 : 2 : [] -- this results in `[1, 2] :: [Int]`!
```

\subsection*{The Maybe functor}

As a simpler example, consider a type that either has no value or it has a value corresponding to some type `a`. In Haskell, this is called `Maybe`, while in C++ this is called `std::optional`, in Python the same idea could be achieved using:
```python
def fn(a):
    if (a >= 0)
        return sqrt(a)
    return None
```
This function returns `None` (corresponding to 'no value') if we provide 'invalid input'. This functor can be defined as:
```haskell
data Maybe = Nothing | Just a
```
And to turn it into a functor, we define `fmap`:
```haskell
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
```

## Polymorphic functions as natural transformations

Now that we view type constructors as functors, we can consider natural transformations between type constructors. If we let `a` be a type, then a natural transformation `alpha` would be something that maps between `F a` and `G a`, where `F` and `G` are type constructors:
```haskell
alpha :: F a -> G a
```
Note that implicitly we talk about the component of `alpha` at `a`, since this function is _polymorphic_ the right component gets picked by the compiler. For example, say we have a list `[a]`, and we want to obtain the first element of this list. If the list is empty, then there is no such element, otherwise we obtain an `a`; i.e. the result is a `Maybe a`:
```haskell
head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = x
```
Here, we have a natural transformation between the `List` and the `Maybe` functor!

\subsection*{Parametric polymorphism and ad-hoc polymorphism}

In C++, a template does not have to be defined for all types, i.e. we can write:
```cpp
template <typename T>
T f(T a);

template <>
int f(int a) { return 2 * a; }

template <>
double f(double a) { return 2.0 * a; }
```
Here, e.g. `f<int>(1)` would yield `2`, while `f<char>('a')` would result in a compilation error.

In Haskell, this is not allowed, polymorphic functions must work for _all types_, this is called parametric polymorphism. Specializing function definitions is done using type classes^[in C++ this would be done using overloading and (partial) template specialization]. This has an important consequence (or perhaps, it is the underlying reason): a parametric polymorphic function satisfies automatically the naturality conditions.

The corresponding diagram is:
\begin{figure}[H]
\centering
\begin{tikzcd}
\texttt{F a} \arrow[r, "\texttt{alpha}"] \arrow[d, "\texttt{fmap f :: F a -> F b}"'] & \texttt{G a} \arrow[d, "\texttt{fmap f :: G a -> G b}"]\\
\texttt{F b} \arrow[r, "\texttt{alpha}"'] & \texttt{G b}
\end{tikzcd}
\end{figure}

Here the left `fmap` works for `F`, while the right `fmap` corresponds to `G`, and the top `alpha` is implicitely the component at `a`, while the bottom one is the component at `b`. What would have to show, is that automatically

```haskell
fmap f . alpha = alpha . fmap f
```

This can be shown in a very general context, and it has to do with the fact that the 'bodies'for `f`, `fmap` and `alpha` are the same for all types. We will show this in an upcoming part when we discuss _free theorems_.

Let us revisit our `head :: [a] -> Maybe a` example, and consider the naturality condition here. It says that:

```haskell
fmap f . head = head . fmap f
```
Here, the fmap on the lhs corresonds to the `Maybe` functor, while on the rhs it corresponds to the `[]` functor. The lhs can b e read like this; take the first element of the list, then apply f on it. The rhs can be read as "apply the function f to the enitre list, then take the first element". The result is the same; the funtion f applied to the head of the list (if any). But for the rhs we apply the function `f` for each element in the list, while on the lhs we only apply it to the head. Because of the constraint on polymorphic function, the compiler knows that the result is equal and can choose which one to use!

## References

- 1.2, 1.7 of the 'Category Theory for Programmers' blog by Bartosz Milewski
