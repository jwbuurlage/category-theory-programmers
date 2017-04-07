module Main where

import Control.Monad
import Control.Comonad
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import GameOfLife

type Picture = QDiagram Rasterific V2 Double Any

-- Set some arbitrary initial stream
initial = fromList [Alive, Dead, Dead, Alive]

-- Construct a universe by extending these streams everywhere
glider :: Universe Cell
glider = Universe $ duplicate $ Tape initial Dead initial

-- Rule and evolving
rule :: Universe Cell -> Cell
rule u
  | alive == 2 = extract u
  | alive == 3 = Alive
  | otherwise = Dead
  where
    alive = length (filter (== Alive) $ join (slice 1 1 (setter u Dead)))

evolve :: Universe Cell -> Universe Cell
evolve universe = universe =>> rule

-- Textual representation
pretty :: Universe Cell -> String
pretty u = unlines $ slice 2 2 (represent <$> u)
  where
    represent Dead = 'o'
    represent Alive = 'x'

-- Pictorial representation
photo :: Universe Cell -> Picture
photo u =
  vcat $
  hcat .
  map
    (\x ->
        square 0.1 #
        fc
          (if x == Alive
             then cyan
             else white)) <$>
  slice 4 4 u

-- Generate frames for gif
play :: Int -> Universe Cell -> [(Picture, Int)]
play 0 _ = []
play n u = (photo u, 40) : play (n - 1) (evolve u)

-- Usage: ./gameoflife -w 500 -o anim.gif
main = mainWith $ play 20 glider
