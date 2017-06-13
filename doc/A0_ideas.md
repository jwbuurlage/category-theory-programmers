\part{Advanced theory and aplications}

\chapter*{Further study}

- **Algebras for a monad**. Eilenberg-Moore category of algebras over a monad, can also be used to show that every monad arises from an adjunction. Also have coalgebras for a comonad Lens example here: <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/>

- **Adjunctions in Haskell**. The only monad over $\mathbf{Hask}$ arising from an adjunction that goes through $\mathbf{Hask}$ itself is the `State` monad:
    ```haskell
    (, f) -| (->) e
    ```
    You can show this using: <https://en.wikipedia.org/wiki/Representable_functor#Left_adjoint>. Witnessed by `curry` and `uncurry`.
    We have:
    ```haskell
    (-> e) -| (-> e)
    ```
    as an adjunction through $\mathbf{Hask}^{\text{op}}$. Witnessed by `flip`.
    This leads to the continuation monad, which we should talk about.
    - <http://www.stephendiehl.com/posts/adjunctions.html>

- **Additional adjunctions**: Additionally, we can try to find interesting adjunctions through:
    - Kleisli categories
    - Endofunctors on **Hask**
    since we can represent these categories in Haskell. On modelling categories in Haskell:
    - <https://www.youtube.com/watch?v=Klwkt9oJwg0>
    Kmett: there is a full adjoint triple for succinct dictionaries:
    ```haskell
    select -| rank -| coselect
    coselect n = select (n + 1) - 1
    ```
    <https://en.wikipedia.org/wiki/Succinct_data_structure>

- **Purely functional datastructures**:
    - <http://apfelmus.nfshost.com/articles/monoid-fingertree.html>
    - <https://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504>

- **Applicative functors**:
    - Applicative ~= Monoidal. Is strong lax functor.
    - McBride, Paterson; Applicative Programming with Effects <http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf>

- **Monad transformers**: 'Translation of a monad along an adjunction'
    - <https://oleksandrmanzyuk.files.wordpress.com/2012/02/calc-mts-with-cat-th1.pdf>

- **Proof assistants**: Curry-Howard isomorphism

- **List of advanced topics**: <http://www.haskellforall.com/2014/03/introductions-to-advanced-haskell-topics.html>

- **Ends and co-ends**.

- **'Theorems for free!'**

- **'Fast and loose reasoning is morally correct'**
    - $\omega$-CPOs
    - Domain theory
    - Note that `newtype` and bottom cause issues.
    - Note that `seq` messes everything op
    - References
        - About **Hask**: <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf>
        - <http://math.andrej.com/2016/08/06/hask-is-not-a-category/>
        - <https://ro-che.info/articles/2016-08-07-hask-category>
        - <https://wiki.haskell.org/Newtype>
        - <http://blog.sigfpe.com/2009/10/what-category-do-haskell-types-and.html>

- **Homotopy type theory**

- **Quantum computations**. (Bert Jacobs)

- **Haskell tricks and gems**. <https://deque.blog/2016/11/27/open-recursion-haskell/>

# Literature

## Blogs
1. *Bartosz Milewski*: "Category Theory for Programmers", a blog post series that gives a good overview of interesting topics. <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>

## Papers
2. Free theorems: <http://ttic.uchicago.edu/~dreyer/course/papers/wadler.pdf> (also Reynold: <http://www.cse.chalmers.se/edu/year/2010/course/DAT140_Types/Reynolds_typesabpara.pdf>).
3. Recursion as initial objects in F-algebra: <http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt>

## Books
1. Conceptual Mathematics: A first introduction to categories.
2. S. Mac Lane, Category Theory for the working mathematician
3. Barr and Wells, Category Theory for Computer Scientists
4. E. Riehl, Category theory in context,
5. T. Leinster, Basic Category Theory
6. J. van Ooosten, Basic Category Theory


