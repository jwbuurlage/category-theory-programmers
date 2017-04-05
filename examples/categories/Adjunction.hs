{-# Language ExistentialQuantification #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}

import Prelude hiding ((>>=))
import Control.Monad hiding ((>>=))
import Data.Functor

-- TODO
-- make this factor through a 'Category', use (->), (<-)
-- show that we can reobtain Monad from canonical adjunction to its Kleisli
data ComposedFunctor f g a = (Functor f, Functor g) => ComposedFunctor { unCompose :: g (f a) }

instance (Functor f, Functor g) => Functor (ComposedFunctor f g) where
  fmap f (ComposedFunctor x) = ComposedFunctor (fmap (fmap f) x)

data Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
    unit :: Id a -> ComposedFunctor f g a
    counit :: ComposedFunctor g f a -> Id a
    phi :: (a -> g b) -> (f a -> b)
    phi h = unId . counit . ComposedFunctor . fmap h
    psi :: (f a -> b) -> (a -> g b)
    psi h = fmap h . unCompose . unit . Id

-- For reference, these are the functor instance for 'pair' and 'based arrows':
-- instance Functor ((,) c) where
--   fmap f (x, y) = (x, f y)
--
-- instance Functor ((->) c) where
--   fmap f g = g . f

instance Adjunction ((,) c) ((->) c) where
  unit (Id x) = ComposedFunctor (\y -> (y, x))
  counit (ComposedFunctor (y, f) ) = Id (f y)

class Functor t => Monad' t where
  eta :: a -> t a
  mu :: t (t a) -> t a

  (>>=) :: t a -> (a -> t b) -> t b
  x >>= f = mu . fmap f $ x

instance (Adjunction f g) => Monad' (ComposedFunctor f g) where
  eta = unit . Id
  mu = ComposedFunctor . fmap (unId . counit . ComposedFunctor) .  unCompose . fmap unCompose

type State s a = ComposedFunctor ((,) s) ((->) s) a
state :: (s -> (s, a)) -> State s a
state = ComposedFunctor
unstate = unCompose

f :: Int -> State Int Int
f x = state $ \y -> (y + 1, x + y)

g :: Int -> State Int Int
g x = state $ \y -> (y + 1, x * y)

-- TODO: add also comonad, obtain Store

main = print $ unstate (eta 0 >>= f >>= g) 1
