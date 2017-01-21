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
import TrashKings.Final

main :: IO ()
main = renderFinal

