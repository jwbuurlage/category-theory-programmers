module Main where

import Control.Comonad

import GameOfLife

pretty :: Universe Int -> String
pretty u = unlines $ slice 5 5 (represent <$> u)
  where
    represent :: Int -> Char
    represent x
      | x == 0 = ' '
      | x /= 0 = 'x'
    
main = do
  let test = Tape (iterate' (\x -> x - 1) (-1)) 0 (iterate' (+1) 1)
  let test' = Universe $ duplicate test
  print $ extract test
  print $ extract (left test)
  putStrLn $ pretty test'
