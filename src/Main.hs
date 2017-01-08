{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}

module Main where

import Diagrams.TwoD.Size
import Diagrams.Prelude
import Diagrams.Backend.SVG
import System.Random
import Control.Monad
import Data.List.Split (chunksOf)
import Control.Monad.State 
import Control.Monad.Random

import TrashKings.Tile
import TrashKings.Types

-- The poor man's weighting...
tileTypes :: MonadRandom m => m TileType 
tileTypes = fromList
    [ (Nubs, 1)
    , (Straight, 2)
    , (DoubleCurve, 2)
    , (CurveNub, 2)
    , (Undies, 1)
    ]

onTile :: Tile -> Tile
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

genTile :: MonadRandom m => m Tile 
genTile = do
    tileType <- tileTypes
    let pieces = decomposed tileType 
    alignment <- uniform directions
    pieces <- forM pieces $ \piece -> do
        color <- uniform colors
        pure $ fc color piece
    pure $ alignTile alignment $ onTile $ mconcat pieces

-- Make a tile from:
-- - An alignment
-- - A list of colors
-- - A tile type
mkTile :: CDir -> [Colour Double] -> TileType -> Tile
mkTile alignment colors tileType =
    alignTile alignment . onTile . mconcat $ zipWith fc colors pieces
    where pieces = decomposed tileType

genTiles :: MonadRandom m => Int -> m [Tile]
genTiles n = replicateM n genTile

layoutTiles :: [Tile] -> Diagram B 
layoutTiles = vcat . map hcat . chunksOf 4

renderMany :: String -> FilePath -> SizeSpec V2 Double -> [Tile] -> IO ()
renderMany prefix directory spec diagrams = 
    forM_ (zip [1..] diagrams) $ \(i, diagram) -> do
        let filepath = directory ++ "/" ++ prefix ++ "-" ++ show i ++ ".svg"
        renderSVG filepath spec diagram

main :: IO ()
main = do
    putStrLn "Generating new tiles"
    gen <- newStdGen
    let tiles' = evalRand (genTiles 4) gen
    renderMany "tile" "tiles" (mkWidth 600) tiles'
    renderSVG "tiles/all.svg" (mkWidth 1000) $ layoutTiles tiles'

