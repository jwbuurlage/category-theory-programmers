{-# LANGUAGE RankNTypes #-}

module Fix where

data Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> (Fix f) -> a
cata k = k . fmap (cata k) . unFix

data Fix' f = Fix' { unFix' :: forall a. (f a -> a) -> a }

iso :: Functor f => Fix f -> Fix' f
iso u = Fix' (flip cata u)

invIso :: Functor f => Fix' f -> Fix f
invIso v = (unFix' v) Fix
