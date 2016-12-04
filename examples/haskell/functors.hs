-- a type constructor `T` constructs a type `T a` from a type `a`.
-- It can have multiple _constructors_, in this case List is either constructed
-- as `Nil` (empty), or as a pair of an `a` along with a `List a`
-- (recursive definition!)
data List a = Nil | Cons a (List a)

-- to turn it into a functor, we have to define `fmap`
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

hed :: List a -> Maybe a
hed Nil = Nothing
hed (Cons x xs) = Just x

main = do
    let xs = Cons 'b' (Cons 'c' Nil)
    putStrLn (show (hed xs))
