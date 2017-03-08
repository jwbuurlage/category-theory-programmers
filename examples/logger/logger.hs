import Control.Applicative
import Data.Monoid
import Data.Functor

data Logger s a = Logger (a, s) deriving (Show)

instance Functor (Logger s) where
  fmap f (Logger (x, log)) = Logger (f x, log)

instance Monoid s => Applicative (Logger s) where
  pure x = Logger (x, mempty)
  (Logger (f, log)) <*> (Logger (x, log')) = Logger (f x, log <> log')

instance Monoid s => Monad (Logger s) where
  return = pure
  (Logger (x, log)) >>= f = concatLog (f x) log
    where concatLog (Logger (y, l')) l = Logger (y, l <> l')

f :: Int -> Logger String Int
f x = Logger (2 * x, "Called f with " ++ show x)

g :: Int -> Logger String Int
g x = Logger (3 * x, "Called g with " ++ show x)

main = print $ show $ return 2 >>= f >>= g
