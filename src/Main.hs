{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}

module Main where

import Diagrams.TwoD.Size
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import System.Random
import Control.Monad
import Data.List.Split (chunksOf)
import Control.Monad.Random

import TrashKings.Generation
import TrashKings.Tile
import TrashKings.Types
import TrashKings.Layout

main :: IO ()
main = do
    putStrLn "Generating new tiles"
    gen <- newStdGen
    let tiles' = evalRand (genTiles 64) gen
    renderMany "tile" "tiles" (mkWidth 675) tiles'
    renderRasterific
        "tiles/all.jpg" (mkWidth 1000) $ layoutTiles tiles'

