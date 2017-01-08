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
import TrashKings.Layout

tileTypes :: MonadRandom m => m TileType 
tileTypes = fromList
    [ (Nubs, 1)
    , (Straight, 2)
    , (DoubleCurve, 2)
    , (CurveNub, 2)
    , (Undies, 1)
    ]

genTile :: MonadRandom m => m Tile 
genTile = do
    tileType <- tileTypes
    let pieces = decomposed tileType 
    alignment <- uniform directions
    pieces <- forM pieces $ \piece -> do
        color <- uniform colors
        pure $ fc color piece
    pure $ alignTile alignment $ onTile $ mconcat pieces

genTiles :: MonadRandom m => Int -> m [Tile]
genTiles n = replicateM n genTile

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

