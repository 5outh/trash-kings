module TrashKings.Layout where

import           TrashKings.Tile
import           TrashKings.Types

import           Data.List.Split
import           Diagrams.Prelude

layoutTiles :: [Tile] -> Diagram'
layoutTiles = vcat . map hcat . chunksOf 4

layoutGrid :: [[Tile]] -> Diagram'
layoutGrid tiles = vcat $ map hcat tiles

layoutFinal :: [Tile] -> [Tile]
layoutFinal ts = 
    map (\ts' ->
            vcat $ map hcat (chunksOf 4 ts')
        ) (chunksOf 16 ts)

-- Align a diagram to the top, right, left or bottom
alignTile :: CDir -> Tile -> Tile
alignTile dir diagram = rotate' diagram
    where
        rotate' =
            rotateBy
                $ case dir of
                      T -> 0
                      R -> 1/4
                      B -> 1/2
                      L -> 3/4

