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
arbitrary :: Universe Cell
arbitrary = Universe $ duplicate $ Tape initial Dead initial

glider =
    topU .
    rightU .
    setter Alive .
    leftU .
    setter Alive .
    leftU .
    setter Alive . bottomU . setter Alive . bottomU . rightU . setter Alive . topU $
    repeatU Dead

-- Rule and evolving
rule :: Universe Cell -> Cell
rule u
    | alive == 2 = extract u
    | alive == 3 = Alive
    | otherwise = Dead
  where
    alive = length (filter (== Alive) $ join (slice 1 1 (setter Dead u)))

evolve :: Universe Cell -> Universe Cell
evolve universe = universe =>> rule

-- Textual representation
pretty :: Universe Cell -> String
pretty u = unlines $ slice 5 5 (represent <$> u)
  where
    represent Dead = 'o'
    represent Alive = 'x'

-- Pictorial representation
photo :: Universe Cell -> Picture
photo u = vcat $ hcat . map (\x -> square 0.1 # fc (color x)) <$> slice 4 4 u
  where
    color Alive = cyan
    color Dead = white

-- Generate frames for gif
play :: Int -> Universe Cell -> [(Picture, Int)]
play 0 _ = []
play n u = (photo u, 40) : play (n - 1) (evolve u)

-- Usage: ./gameoflife -w 500 -o anim.gif
main = mainWith $ play 20 glider
