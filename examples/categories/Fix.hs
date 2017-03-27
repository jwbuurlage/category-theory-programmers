{-# LANGUAGE RankNTypes #-}

data Fix f = Fix { unFix :: forall a. (f a -> a) -> a }
data Fix' f = Fix' { unFix' :: f (Fix' f) }

cata :: Functor f => (f a -> a) -> (Fix' f) -> a
cata k = k . fmap (cata k) . unFix'

iso :: Functor f => Fix' f -> Fix f
iso u = Fix (flip cata u)

invIso :: Functor f => Fix f -> Fix' f
invIso v = (unFix v) Fix'

data ExprR b = Cst Int | Add (b, b)
type Expr = Fix' ExprR

cst = Fix' . Cst
add = Fix' . Add

instance Functor ExprR where
  fmap _ (Cst c) = Cst c
  fmap f (Add (x, y)) = Add (f x, f y)

eval = cata algebra where
    algebra (Cst c) = c
    algebra (Add (x, y)) = x + y

optimizeLeftUnit :: ExprR Expr -> Expr
optimizeLeftUnit (Add (Fix' (Cst 0), _)) = cst 0
optimizeLeftUnit e = Fix' e

optimizeRightUnit :: ExprR Expr -> Expr
optimizeRightUnit (Add (_, Fix' (Cst 0))) = cst 0
optimizeRightUnit e = Fix' e

-- optimizeRightUnit (Add (Cst _, 0)) = cst 0
-- optimizeRightUnit e = e

comp f g = f . unFix' . g

optimize = cata (optimizeLeftUnit `comp` optimizeRightUnit)

printExpr = cata algebra where
    algebra (Cst c) = show c
    algebra (Add (x, y)) = "(" ++ x ++ " + " ++ y ++ ")"

-- For list, we need 'higher order fixed points'
-- see: <http://comonad.com/reader/2013/algebras-of-applicatives/>
-- data List a b = Empty | Cons (a, b)
-- data FixF f a = ...
-- show' = cata algebra where
--         algebra Empty = "]"
--         algebra (Cons (x, y)) = (show x) ++ y

main = do
    print $ printExpr $ optimize $ add (cst 3, add (cst 0, cst 2))
    print $ eval $ add (cst 3, cst 4)
    print $ printExpr $ add (cst 3, add (cst 3, cst 4))


