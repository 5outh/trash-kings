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

import TrashKings.Tile
import TrashKings.Types

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
randomElement :: (RandomGen g, MonadState g m) => [a] -> m a 
randomElement xs = do
    gen <- get
    let (index, g') = randomR (0, length xs - 1 :: Int) gen
    put g'
    pure $ xs !! index

genTile :: (RandomGen g, MonadState g m) => m Tile 
genTile = do
    tileType <- randomElement tileTypes
    let pieces = decomposed tileType 
    alignment <- randomElement directions
    pieces <- forM pieces $ \piece -> do
        color <- randomElement colors
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

genTiles :: (RandomGen g, MonadState g m) => Int -> m [Diagram B]
genTiles n = replicateM n genTile

layoutTiles = vcat . map hcat . chunksOf 4

renderMany :: String -> FilePath -> SizeSpec V2 Double -> [Diagram B] -> IO ()
renderMany prefix directory spec diagrams = 
    forM_ (zip [1..] diagrams) $ \(i, diagram) -> do
        let filepath = directory ++ "/" ++ prefix ++ "-" ++ show i ++ ".svg"
        renderSVG filepath spec diagram

main :: IO ()
main = do
    putStrLn "Generating new tiles"
    gen <- newStdGen
    let tiles' = evalState (genTiles 4) gen
    renderMany "tile" "tiles" (mkWidth 600) tiles'
    renderSVG "tiles/all.svg" (mkWidth 1000) $ layoutTiles tiles'

