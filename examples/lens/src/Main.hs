{-# LANGUAGE RankNTypes #-}

module Main where

data Lens a b s t = Lens { view' :: s -> a, update' :: (b, s) -> t }

fst' (x, (_, z)) = (x, z)

lens1 = Lens fst fst'

sign = Lens view' update'
  where
    view' = (>= 0)
    update' (b, x) = if b then abs x else (-(abs x))

(^..) = flip view'

class Profunctor p where
   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
    dimap f g h  = g . h . f

class Profunctor p => Cartesian p where
    first :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)

cross f g (x, y) = (f x, g y)

instance Cartesian (->) where
    first h = cross h id
    second h = cross id h

class Profunctor p => CoCartesian p where
    left :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)

plus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
plus f _ (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)

instance CoCartesian (->) where
    left h = plus h id
    right h = plus id h

class Profunctor p => Monoidal p where
    par :: p a b -> p c d -> p (a, c) (b, d)
    empty :: p () ()

instance Monoidal (->) where
    par = cross
    empty = id

-- if we can convert a to b, then we convert (a | s) to (b | t)
type Optic p a b s t = p a b -> p s t

fork f g x = (f x, g x)

type LensP a b s t = forall p. Cartesian p => Optic p a b s t

lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens v u) = dimap (fork v id) u . first

_1 = lensC2P lens1

instance Profunctor (Lens a b) where
  dimap f g (Lens v u) = Lens (v . f) (g . u . cross id f)

instance Cartesian (Lens a b) where
  first (Lens v u) = Lens (v . fst)  (fork (u . cross id fst) (snd . snd))
  second (Lens v u) = Lens (v . snd) (fork (fst . snd) (u . cross id snd))

lensP2C :: LensP a b s t -> Lens a b s t
lensP2C l = l (Lens id fst)

view = view' . lensP2C

--(^.) = flip view 

main :: IO ()
main = do
  print $ show $ view' lens1 (2, 3)
  print $ show $ (2, 3)^..lens1
  print $ show $ update' lens1 ("Hi!", (2, 3))
  print $ show $ view' sign 3
  print $ show $ update' sign (False, 3)
  print $ show $ 12000^..sign
  print $ show $ _1 (+1) (2, 3)
  print $ show $ (_1 . _1) (+1) ((2, 4), 3)
