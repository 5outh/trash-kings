{-# LANGUAGE FlexibleContexts #-}
module TrashKings.Examples where

import           Control.Monad.Random
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Diagrams.TwoD.Arrow
import           Diagrams.TwoD.Size

import           TrashKings.Generation
import           TrashKings.Layout
import           TrashKings.Tile
import           TrashKings.Types

-- Generate a named example
genExample :: FilePath -> Diagram' -> IO ()
genExample filepath =
    renderSVG
        ("examples/" <> filepath <> ".svg")
        (mkWidth 1000)


-- TODO use connectOutside' and customize
d1 `connect2` d2 = connectOutside "1" "2"
    $ d1 # named "1" # moveTo pA
    <> d2 # named "2" # moveTo pB
        where 
            pA = p2 (0, 0)
            pB = p2 (2, 0)

tile = alignTile R $ mkTile [blue', yellow'] undiesWithNub'
tile2 = alignTile R 
    $ mkTile [blue', yellow', blue', yellow'] nubs'

go = do
    genExample "test"
        $ tile `connect2` (hcat [tile, tile2])
