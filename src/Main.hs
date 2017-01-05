{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Control.Monad
import Data.List.Split (chunksOf)
import Control.Monad.State 

data Corner = TL | TR | BL | BR
    deriving (Show, Eq, Enum)

data CDir = T | L | R | B
    deriving (Show, Eq, Enum)

directions :: [CDir]
directions = enumFromTo T B

corners :: [Corner]
corners = enumFromTo TL BR

-- Possible colors that can fill in the tiles
colors :: (Ord a, Floating a) => [Colour a]
colors = [lightblue, wheat, pink]

blankTile :: Diagram B
blankTile = square 1 # fc lightgreen 

onTile = (<> blankTile)

-- Align a diagram to the top, right, left or bottom
alignTile :: CDir -> Diagram B -> Diagram B
alignTile dir diagram = rotate' diagram
    where
        rotate' =
            rotateBy
                $ case dir of
                      T -> 0
                      R -> 1/4
                      B -> 1/2
                      L -> 3/4

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

straightWithNubs' = [straightRoad T, nub L, nub R]

curveWithNubs' = [curvedRoad TL, nub B, nub R]
doubleCurvedRoad' = [curvedRoad TL, curvedRoad BR]

undies :: Diagram B
undies = translate (r2 (0, 1/6)) $ roundedRect' 1 (2/3) opts
    where 
        opts = with & radiusTL .~ -1/3
                    & radiusTR .~ -1/3

undiesWithNub' = [undies <> nub B]

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

-- The poor man's weighting...
tileTypes :: [TileType]
tileTypes =
    [ Nubs
    , Straight, Straight
    , DoubleCurve, DoubleCurve
    , CurveNub, CurveNub
    , Undies
    ]

randomElement :: (RandomGen g, MonadState g m) => [a] -> m a 
randomElement xs = do
    gen <- get
    let (index, g') = randomR (0, length xs - 1 :: Int) gen
    put g'
    pure $ xs !! index

genTile :: (RandomGen g, MonadState g m) => m (Diagram B)
genTile = do
    tileType <- randomElement tileTypes
    let pieces = decomposed tileType 
    alignment <- randomElement directions
    pieces <- forM pieces $ \piece -> do
        color <- randomElement colors
        pure $ fc color piece
    pure $ alignTile alignment $ onTile $ mconcat pieces

genTiles :: (RandomGen g, MonadState g m) => Int -> m [Diagram B]
genTiles n = replicateM n genTile

layoutTiles = vcat . map hcat . chunksOf 8

main :: IO ()
main = do
    putStrLn "Generating new tiles"
    gen <- newStdGen
    let tiles' = evalState (genTiles 64) gen
    mainWith $ layoutTiles tiles'

