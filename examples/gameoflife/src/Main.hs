module Main where

import Control.Monad
import Control.Comonad

import GameOfLife

initial = fromList [Dead, Dead, Alive, Dead, Dead, Alive]

glider :: Universe Cell
glider = Universe $ duplicate $ Tape initial Dead initial

pretty :: Universe Cell -> String
pretty u = unlines $ slice 5 5 (represent <$> u)
  where
    represent Dead = ' '
    represent Alive = 'x'

rule :: Universe Cell -> Cell
rule u
  | neighbours == 2 = extract u
  | neighbours == 3 = Alive
  | otherwise = Dead
  where
    neighbours = length (filter (== Alive) $ join (slice 1 1 u)) - status
      where
        status = if extract u == Alive then 1 else 0

evolve u = u =>> rule

play :: Int -> Universe Cell -> IO ()
play 0 _ = print "Done"
play n u = do
  putStrLn $ pretty u
  let u' = evolve u
  putStrLn ""
  play (n - 1) u'

main = play 20 glider
