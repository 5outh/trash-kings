{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Corner = TL | TR | BL | BR
    deriving (Show, Eq, Enum)

data CDir = T | L | R | B
    deriving (Show, Eq, Enum)

directions :: [CDir]
directions = enumFromTo T B

corners :: [Corner]
corners = enumFromTo TL BR

opCorner :: Corner -> Corner
opCorner = \case
    TL -> BR
    BR -> TL
    TR -> BL
    BL -> TR

blankTile :: Diagram B
blankTile = square 1

curvedRoad :: Corner -> Diagram B
curvedRoad corner = trans corner $ annularWedge (2/3) (1/3) (d corner) a
    where
        d :: Corner -> Direction V2 Double
        d c = flip rotateBy xDir $ case c of 
           BL -> 0
           BR -> 1/4 
           TR -> 2/4
           TL -> 3/4

        trans c = translate . r2 $ case c of 
            BL -> (-1/2, -1/2)
            BR -> (1/2, -1/2)
            TR -> (1/2, 1/2)
            TL -> (-1/2, 1/2)

        a :: Angle Double
        a = (tau / 4) @@ rad

nub :: CDir -> Diagram B
nub c = trans c $ wedge (1/6) (d c) a
    where
        d = flip rotateBy xDir . \case
                B -> 0   
                R -> 1/4
                T -> 1/2
                L -> 3/4
        trans = translate . r2 . \case
                    B -> (0, -1/2) 
                    R -> (1/2, 0) 
                    T -> (0, 1/2)
                    L -> (-1/2, 0) 

        a = (tau / 2) @@ rad

nubTile :: CDir -> Diagram B
nubTile c = nub c `atop` blankTile

curvedRoadTile :: Corner -> Diagram B
curvedRoadTile c = curvedRoad c `atop` blankTile

doubleCurvedRoadTile :: Corner -> Diagram B
doubleCurvedRoadTile c = curvedRoad c <> curvedRoad (opCorner c) <> blankTile

curvedRoadTiles :: Diagram B
curvedRoadTiles = hcat $ map curvedRoadTile (enumFromTo TL BR)

doubleCurvedRoadTiles :: Diagram B
doubleCurvedRoadTiles = hcat $ map doubleCurvedRoadTile [TL, TR]

straightRoad = \case
    T -> rect (1/3) 1 
    B -> rect (1/3) 1
    R -> rect 1 (1/3)
    L -> rect 1 (1/3)

straightRoadTile :: CDir -> Diagram B
straightRoadTile = atop blankTile . straightRoad

straightRoadTiles :: Diagram B
straightRoadTiles = hcat $ map straightRoadTile [T,L]

nubTiles = hcat $ map nubTile [T,L,B,R]

tiles :: Diagram B
tiles = vcat
    [ curvedRoadTiles
    , doubleCurvedRoadTiles
    , straightRoadTiles
    , nubTiles
    ]

straightWithNubsTile :: CDir -> Diagram B
straightWithNubsTile c = blankTile <> straightRoad c <> case c of
    T -> nub L <> nub R
    R -> nub T <> nub B
    B -> nub L <> nub R
    L -> nub T <> nub B

curveWithNubsTile :: Corner -> Diagram B
curveWithNubsTile c = blankTile <> curvedRoad c <> case c of
    TR -> nub B <> nub L
    TL -> nub B <> nub R
    BL -> nub T <> nub R
    BR -> nub T <> nub L

undies :: Diagram B
undies = translate (r2 (0, 1/6)) $ roundedRect' 1 (2/3) opts 
    where 
        opts = with & radiusTL .~ -1/3
                    & radiusTR .~ -1/3

undiesWithNub rot = rotateBy rot $ undies <> nub B <> blankTile

-- todo get all tiles as data
-- todo colors
fullTiles = vcat
    [ mconcat (map nub directions) <> blankTile
    , doubleCurvedRoadTiles
    , hcat (map straightWithNubsTile [T,R]) 
    , hcat (map curveWithNubsTile corners)
    , hcat (map undiesWithNub [0, 1/4, 1/2, 3/4])
    ]

main :: IO ()
main = do
    putStrLn "Generating new circle"
    mainWith fullTiles

