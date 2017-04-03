module Expr where

import Fix

-- References:
-- <https://deque.blog/2017/01/20/catamorph-your-dsl-deep-dive/>

data ExprF b = Cst Int | Add (b, b)
type Expr = Fix ExprF

cst = Fix . Cst
add = Fix . Add

instance Functor ExprF where
  fmap _ (Cst c) = Cst c
  fmap f (Add (x, y)) = Add (f x, f y)

eval = cata algebra where
    algebra (Cst c) = c
    algebra (Add (x, y)) = x + y

leftUnit :: ExprF Expr -> Expr
leftUnit (Add (Fix (Cst 0), e)) = e
leftUnit e = Fix e

rightUnit :: ExprF Expr -> Expr
rightUnit (Add (e, Fix (Cst 0))) = e
rightUnit e = Fix e

comp f g = f . unFix . g
optimize = cata (leftUnit `comp` rightUnit)

render = cata algebra where
    algebra (Cst c) = show c
    algebra (Add (x, y)) = "(" ++ x ++ " + " ++ y ++ ")"

main = do
    print $ render $ optimize $ add (cst 3, add (cst 0, cst 2))
    print $ eval $ add (cst 3, cst 4)
    print $ render $ add (cst 3, add (cst 3, cst 4))
