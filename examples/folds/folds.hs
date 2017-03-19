sum :: Num a => [a] -> a
sum = undefined

product :: Num a => [a] -> a
product = undefined

length :: Num b => [a] -> b
length = undefined

and :: [Bool] -> Bool
and = undefined

or :: [Bool] -> Bool
or = undefined

elem :: a -> [a] -> Bool
elem = undefined

min :: Ord a => [a] -> a
min = undefined

max :: Ord a => [a] -> a
max = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

concat :: [[a]] -> [a]
concat = undefined

reverse :: [a] -> [a]
reverse = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter p = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

-- use foldMap to implement the following functions
sum' = undefined
product' = undefined
concat' = undefined

toString :: Show a => [a] -> String

-- database?

-- maybe
-- either
-- binary tree

-- use scan to implement
population :: Int -> Float -> [Int]
population = undefined

fibonacci :: [Int]
fibonnaci = undefined
