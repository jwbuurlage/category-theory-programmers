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

printExpr = cata algebra where
    algebra (Cst c) = show c
    algebra (Add (x, y)) = "(" ++ x ++ " + " ++ y ++ ")"

main = do
    print $ eval $ add (cst 3, cst 4)
    print $ printExpr $ add (cst 3, add (cst 3, cst 4))
