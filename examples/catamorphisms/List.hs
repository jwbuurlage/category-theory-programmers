module CataList where

import Fix

-- References:
-- <http://comonad.com/reader/2013/algebras-of-applicatives/>
-- <https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/>

data ListF a b = Nil | Cons a b

type List a = Fix (ListF a)

nil :: List a
nil = Fix Nil

(<:>) :: a -> List a -> List a
(<:>) x xs = Fix (Cons x xs)

infixr 5 <:>

instance Functor (ListF a) where
    fmap f Nil = Nil
    fmap f (Cons e x) = Cons e (f x)

type Algebra f a = f a -> a

sum' :: Algebra (ListF Int) Int
sum' Nil = 0
sum' (Cons e acc) = e + acc

sqr' :: Algebra (ListF Int) (List Int)
sqr' Nil = nil
sqr' (Cons e acc) = (e * e) <:> acc

lst :: Fix (ListF Int)
lst = 2 <:> 3 <:> 4 <:> nil

main = do
    print $ (cata sum') lst
    print $ (cata sum') $ (cata sqr') lst
    print $ foldr (\e acc -> e + acc) 0 [2, 3, 4]
