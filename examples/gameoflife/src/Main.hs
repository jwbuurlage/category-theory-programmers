module Main where

import Control.Monad
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import GameOfLife

type Picture = QDiagram Cairo V2 Double Any

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

photo :: Universe Cell -> Picture
photo u = vcat $ hcat . map
  (\x -> square 0.1 # fc (if x == Alive then cyan else white)) <$> slice 5 5 u

play :: Int -> Universe Cell -> [(Picture, Int)]
play 0 _ = []
play n u = (photo u, 25) : play (n - 1) (evolve u)

-- usage: ./gameoflife -w 500 -o anim.gif
main = gifMain $ play 20 glider
