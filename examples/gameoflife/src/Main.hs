module Main where

import Control.Monad
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import GameOfLife

type Picture = QDiagram Rasterific V2 Double Any

-- Set some arbitrary initial stream
initial = fromList [Dead, Dead, Alive, Dead, Dead, Alive]

-- Construct a universe by extending these streams everywhere
glider :: Universe Cell
glider = Universe $ duplicate $ Tape initial Dead initial

-- Rule and evolving
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

-- Textual representation
pretty :: Universe Cell -> String
pretty u = unlines $ slice 5 5 (represent <$> u)
  where
    represent Dead = ' '
    represent Alive = 'x'

-- Pictorial representation
photo :: Universe Cell -> Picture
photo u = vcat $ hcat . map
  (\x -> square 0.1 # fc (if x == Alive then cyan else white)) <$> slice 10 10 u

-- Generate frames for gif
play :: Int -> Universe Cell -> [(Picture, Int)]
play 0 _ = []
play n u = (photo u, 25) : play (n - 1) (evolve u)

-- Usage: ./gameoflife -w 500 -o anim.gif
main = mainWith $ play 20 glider
