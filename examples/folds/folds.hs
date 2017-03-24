import Prelude hiding (foldr, foldl)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.Functor
import Data.Foldable hiding (foldr, foldl)
import qualified Data.Set as S

-- ---------------------------------------------------
-- Folds over lists
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x xs = case xs of
    [] -> x
    (y:ys) -> (foldl f x ys) `f` y

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x xs = case xs of
    [] -> x
    (y:ys) -> y `f` (foldr f x ys)

sumf :: Num a => [a] -> a
sumf = foldr (+) 0

productf :: Num a => [a] -> a
productf = foldr (*) 1

lengthf :: Num b => [a] -> b
lengthf = foldr (\_ c -> c + 1) 0

andf :: [Bool] -> Bool
andf = foldr (&&) True

orf :: [Bool] -> Bool
orf = foldr (||) True

elemf :: Eq a => a -> [a] -> Bool
elemf x = foldr (\a b -> (a == x) || b) False

minf :: (Bounded a, Ord a) => [a] -> a
minf = foldr min maxBound

maxf :: (Bounded a, Ord a) => [a] -> a
maxf = foldr max minBound

allf :: (a -> Bool) -> [a] -> Bool
allf p xs = andf (p <$> xs)

anyf :: (a -> Bool) -> [a] -> Bool
anyf p xs = orf (p <$> xs)

concatf :: [[a]] -> [a]
concatf = foldr (++) []

reversef :: [a] -> [a]
reversef = foldl (flip (:)) []

filterf :: (a -> Bool) -> [a] -> [a]
filterf p = foldr (\x xs -> if p x then x : xs else xs) []

mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x xs -> f x : xs) []

-- use foldMap to implement the following functions
sumfm :: Num a => [a] -> a
sumfm = getSum . foldMap Sum

productfm :: Num a => [a] -> a
productfm = getProduct . foldMap Product

concatfm :: [[a]] -> [a]
concatfm = foldMap id

asString :: Show a => [a] -> String
asString = foldMap show

-- ---------------------------------------------------
-- Folds over other data types

foldMaybe :: (a -> b) -> b -> Maybe a -> b
foldMaybe f c ma = case ma of
    Nothing -> c
    (Just x) -> f x

foldEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
foldEither f g eab = case eab of
    Left x -> Left (f x)
    Right y -> Right (g y)

-- binary tree
data Tree a = Node (Tree a) a (Tree a) | Leaf deriving (Show)

foldt :: (a -> b -> b) -> b -> Tree a -> b
foldt f c Leaf = c
foldt f c (Node t x t') = foldt f (x `f` (foldt f c t)) t'

sumTree :: Num a => Tree a -> a
sumTree = foldt (+) 0

-- ---------------------------------------------------
-- Funky natural number examples from Bird and De Moor
data Nat = Zero | Succ Nat

instance Show Nat where
    show x = show' 0 x
      where
        show' n Zero = show n
        show' n (Succ y) = show' (n + 1) y

zero = Zero
one = Succ Zero
two = Succ one
three = Succ two

foldn :: b -> (b -> b) -> Nat -> b
foldn c _ Zero = c
foldn c f (Succ x) = f (foldn c f x)

plus m = foldn m Succ
mult m = foldn zero (plus m)
expn m = foldn one (mult m)
fact = snd . foldn (zero, one) (\(m, n) -> (plus one m, mult (plus one m) n))
fib = snd . foldn (zero, one) (\(m, n) -> (n, plus m n))

-- ---------------------------------------------------
-- Using State in a fold
filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filtering p = foldr (\x -> (<*>) ((\p' -> if p' then (x :) else id) <$> p x)) (pure [])

distinct :: Ord a => [a] -> [a]
distinct xs = evalState (filtering (\x -> state (\s -> (S.notMember x s, S.insert x s))) xs) S.empty

main = undefined
