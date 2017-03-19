import Data.Monoid
import qualified Data.Set as S
import Control.Monad.State

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

-- database?

-- maybe
-- either
-- binary tree

-- use scan to implement
population :: Int -> Float -> [Int]
population = undefined

fibonnaci :: [Int]
fibonnaci = undefined

filtering :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filtering p = foldr (\x -> (<*>) ((\p' -> if p' then (x :) else id) <$> p x)) (pure [])

-- use State monad for this:
distinct :: Ord a => [a] -> [a]
distinct xs = evalState (filtering (\x -> state (\s -> (S.notMember x s, S.insert x s))) xs) S.empty
