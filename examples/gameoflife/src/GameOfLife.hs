module GameOfLife where

-- Reference:
-- <https://kukuruku.co/post/cellular-automata-using-comonads/>
-- <http://blog.emillon.org/posts/2012-10-18-comonadic-life.html>
import Data.Functor
import Control.Comonad

class Functor f =>
      Grab f  where
    grab :: Int -> f a -> [a]

data Stream a =
    Cons a
         (Stream a)

instance Grab Stream where
    grab 0 (Cons x xs) = []
    grab n (Cons x xs) = x : grab (n - 1) xs

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Comonad Stream where
    extract (Cons x _) = x
    duplicate (Cons x xs) = Cons (Cons x xs) (duplicate xs)

data Tape a =
    Tape (Stream a)
         a
         (Stream a)

instance Grab Tape where
    grab n (Tape l x r) = reverse (grab n l) ++ [x] ++ grab n r

iterate' :: (a -> a) -> a -> Stream a
iterate' f x = Cons x (iterate' f (f x))

repeat' :: a -> Stream a
repeat' x = Cons x (repeat' x)

left :: Tape a -> Tape a
left (Tape (Cons x xs) y zs) = Tape xs x (Cons y zs)

right :: Tape a -> Tape a
right (Tape xs y (Cons z zs)) = Tape (Cons y xs) z zs

instance Functor Tape where
    fmap f (Tape xs y zs) = Tape (f <$> xs) (f y) (f <$> zs)

instance Comonad Tape where
    extract (Tape _ x _) = x
    duplicate tape =
        Tape (iterate' left (left tape)) tape (iterate' right (right tape))

repeatT x = Tape (repeat' x) x (repeat' x)

data Universe a = Universe
    { getUniverse :: Tape (Tape a)
    }

instance Functor Universe where
    fmap f = Universe . (fmap . fmap) f . getUniverse

instance Comonad Universe where
    extract = extract . extract . getUniverse
    duplicate = fmap Universe . Universe . shifts . shifts . getUniverse
      where
        shifts :: Tape (Tape a) -> Tape (Tape (Tape a))
        shifts tape =
            Tape
                (iterate' (fmap left) (left <$> tape))
                tape
                (iterate' (fmap right) (right <$> tape))

slice :: Int -> Int -> Universe a -> [[a]]
slice x y = fmap (grab y) . grab x . getUniverse

data Cell
    = Dead
    | Alive
    deriving (Eq, Show)

fromList :: [a] -> Stream a
fromList xs = helper xs xs
  where
    helper xs ys = foldr Cons (fromList ys) xs

setter :: a -> Universe a -> Universe a
setter x (Universe (Tape st1 (Tape s1 _ s2) st2)) =
    Universe (Tape st1 (Tape s1 x s2) st2)

repeatU :: a -> Universe a
repeatU = Universe . repeatT . repeatT

leftU = Universe . fmap left . getUniverse

rightU = Universe . fmap right . getUniverse

topU = Universe . left . getUniverse

bottomU = Universe . right . getUniverse
