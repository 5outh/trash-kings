{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1
    # fc blue
    # lw none
    ||| circle 1 #  fc red # lw veryThick # lc purple

example :: Diagram B
example = square 1 # fc aqua `atop` circle 1

tile :: Diagram B
tile = rect (1/3) 1 `atop` square 1
    ||| (square 1 `atop` rect 1 (1/3))

blankTile :: Diagram B
blankTile = square 1

donut :: Diagram B
donut = circle (1/2) `atop` circle 1

curvedRoad :: Diagram B
curvedRoad = translate (r2 (1/2, -1/2)) $ annularWedge (2/3) (1/3) d a # showOrigin
    where
        d :: Direction V2 Double
        d = rotateBy (1/4) xDir

        a :: Angle Double
        a = (tau / 4) @@ rad

main :: IO ()
main = do
    putStrLn "Generating new circle"
    mainWith (curvedRoad `atop` blankTile # showOrigin) 
