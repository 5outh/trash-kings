module TrashKings.Generation where

import Control.Monad.Random
import Diagrams.Prelude
import Control.Monad
import Diagrams.Backend.SVG

import TrashKings.Layout
import TrashKings.Tile
import TrashKings.Types

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

