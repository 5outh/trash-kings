{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}

module TrashKings.Tile where

import TrashKings.Types
import Diagrams.TwoD.Size
import Diagrams.Prelude
import Diagrams.Backend.SVG

directions :: [CDir]
directions = enumFromTo T B

corners :: [Corner]
corners = enumFromTo TL BR

-- Possible colors that can fill in the tiles
colors :: (Ord a, Floating a) => [Colour a]
colors = [lightblue, wheat, pink]

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

straightRoad :: CDir -> Diagram B
straightRoad = \case
    T -> rect (1/3) 1 
    B -> rect (1/3) 1
    R -> rect 1 (1/3)
    L -> rect 1 (1/3)

-- Pieces of the tiles
straightWithNubs' = [straightRoad T, nub L, nub R]
curveWithNubs' = [curvedRoad TL, nub B, nub R]
doubleCurvedRoad' = [curvedRoad TL, curvedRoad BR]
undiesWithNub' = [undies <> nub B]

undies :: Diagram B
undies = translate (r2 (0, 1/6)) $ roundedRect' 1 (2/3) opts
    where 
        opts = with & radiusTL .~ -1/3
                    & radiusTR .~ -1/3
data TileType
    = Nubs
    | Straight
    | DoubleCurve
    | CurveNub
    | Undies
      deriving (Show, Eq, Enum)

-- A decomposed tile that just needs to be
-- mconcat'd in order to become a diagram
decomposed :: TileType -> [Diagram B]
decomposed = \case
    Nubs -> map nub directions
    Straight -> straightWithNubs'
    DoubleCurve -> doubleCurvedRoad'
    CurveNub -> curveWithNubs'
    Undies -> undiesWithNub'

