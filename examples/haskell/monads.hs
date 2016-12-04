-- say we have a function that does something, lets just wrap
-- the square root
sq :: Float -> Float
sq x = sqrt x

-- now if we have < 0, then this becomes NaN
-- we want exceptions, one way of doing this is using Maybe
sqrt2 :: Float -> Maybe Float
sqrt2 x
    | x >= 0 = Just $ sqrt x
    | otherwise = Nothing

-- next consider
square :: Float -> Float
square x = x * x

-- now what happens when we want to take the square after the 'safe sqrt'?
-- We would have to change square, so that it takes a Maybe Float:
square2 :: Maybe Float -> Maybe Float
square2 Nothing = Nothing
square2 (Just x) = Just (x * x)

-- can't this be done automatically? yes, we know that Maybe is a functor, so we can `fmap` the square
-- but what if we want to combine a number of functions f :: Float -> Float that may or may not introduce Maybe's.
-- We require two things:
-- if f :: Float -> Float, then we may have to lift it either to Maybe Float -> Maybe Float
-- if it is 'in' the chain, or Float -> Maybe Float if it is 'in front' of the chain.
-- this is done by the unit function:
unit :: a -> Maybe a
unit x = Just x

-- this allows us to lift the f
lift f = unit . f

-- furthermore if we have a function Float -> Maybe Float, then we must also be able to transform it into a Maybe Float -> Maybe Float:
-- let us write this a bit differently by looking at it
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

main :: IO ()
main = do
    putStrLn $ show $ square2 $ sqrt2 2.0
    putStrLn $ show $ fmap square $ sqrt2 2.0
    putStrLn $ show $ bind (lift square $ 7) sqrt2
    putStrLn $ show $ (lift square $ 7) `bind` sqrt2
    putStrLn $ show $ return 7 >>= lift square >>= sqrt2

-- I want more examples of monads
-- I want to see how it compares to mathematics.
-- Natural transfomratino from endofunctor to T:
-- n :: a -> T a
-- Natural transfomratino from T T -> T
-- u :: T T a -> T a
-- how does this compare to bind
-- bind: T a -> T b

-- check:
-- - http://www.stephendiehl.com/posts/monads.html
-- - http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
-- - https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/
-- - https://bartoszmilewski.com/2016/11/30/monads-and-effects/
-- - https://bartoszmilewski.com/2013/03/07/the-tao-of-monad/
