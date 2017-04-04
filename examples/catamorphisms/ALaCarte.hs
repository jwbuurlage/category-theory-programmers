{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module ALaCarte where

-- References:
-- "W. Swierstra; Data types a la carte"
-- <http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf>
--
-- > The goal is to define a data type by cases, where one can add new cases to the data type
-- > and new functions over the data type, without recompiling existing code, and while retaining static type safety
-- 'The expression problem' (Wadler, 1998)

import Fix

data Expr' = Val' Int | Add' Expr' Expr'

eval' :: Expr' -> Int
eval' (Val' x) = x
eval' (Add' e e') = eval' e + eval' e'

render' :: Expr' -> String
render' (Val' x) = show x
render' (Add' e e') = "(" ++ render' e ++ " + " ++ render' e' ++ ")"

data Val e = Val Int
type IntExpr = Fix Val

data Add e = Add e e
type AddExpr = Fix Add

-- > The big challenge, of course, is to combine the ValExpr and AddExpr
-- > types somehow. The key idea is to combine expressions by taking the
-- > coproduct of their signatures

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 5 :+:

addExample :: Fix (Val :+: Add)
addExample = Fix (Inr (Add (Fix (Inl (Val 2))) (Fix (Inl (Val 3)))))

instance Functor Val where
    fmap f (Val x) = Val x

instance Functor Add where
    fmap f (Add e e') = Add (f e) (f e')

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e) = Inl (fmap f e)
    fmap f (Inr e) = Inr (fmap f e)

class Functor f => Eval f where
    evalAlg :: f Int -> Int

instance Eval Val where
    evalAlg (Val x) = x

instance Eval Add where
    evalAlg (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlg (Inl x) = evalAlg x
    evalAlg (Inr x) = evalAlg x

eval :: Eval f => Fix f -> Int
eval = cata evalAlg

-- > The definition of addExample illustrates how messy expressions can easily
-- > become. In this section, we remedy the situation by introducing smart
-- > constructors for addition and values.

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => f :<: f where
    inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = Fix . inj

val :: (Val :<: f) => Int -> Fix f
val x = inject (Val x)

(<+>) :: (Add :<: f) => Fix f -> Fix f -> Fix f
x <+> y = inject (Add x y)

infixl 6 <+>

-- > The type signature of x is very important! We exploit the type signature to
-- > figure out the injection into a coproduct: if we fail to provide the type
-- > signature, a compiler has no hope whatsoever of guessing the right injection

expression :: Fix (Add :+: Val)
expression = (val 30000) <+> (val 200)

-- Now lets add more constructors, e.g. multiplication!

data Mul x = Mul x x

instance Functor Mul where
    fmap f (Mul e e') = Mul (f e) (f e')

instance Eval Mul where
    evalAlg (Mul x y) = x * y

(<#>) :: (Mul :<: f) => Fix f -> Fix f -> Fix f
x <#> y = inject (Mul x y)

infixl 7 <#>

expression2 :: Fix (Val :+: Add :+: Mul)
expression2 = (val 30000) <+> (val 200) <#> (val 300)

-- Lets add more functionality!
-- for example pretty printing
--
-- NOTE: this differs from the implementation in the paper, but I think
-- expressing it as an algebra is nicer
class Functor f => Render f where
    render :: f String -> String

pretty :: Render f => Fix f -> String
pretty = cata render

instance Render Val where
    render (Val x) = show x

instance Render Add where
    render (Add e e') = "(" ++ e ++ " + " ++ e' ++ ")"

instance Render Mul where
    render (Mul e e') = "(" ++ e ++ " * " ++ e' ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr y) = render y

-- Testing environment
main = do
    let e = (Add' (Val' 5) (Val' 1))
    print $ render' e
    print $ eval' e
    print $ eval expression
    print $ eval expression2
    print $ pretty expression2
