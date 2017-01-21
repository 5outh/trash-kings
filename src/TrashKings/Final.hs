module TrashKings.Final where

import Diagrams.Prelude hiding ((##))
import Diagrams.Backend.Rasterific
import Control.Concurrent.Async
import Control.Monad

import TrashKings.Layout
import TrashKings.Types
import TrashKings.Tile
import TrashKings.Generation

(##) = flip (,)

y = yellow'; b=blue'; r=red';
n=nubs';u=undiesWithNub';s=straightWithNubs'
dc=doubleCurvedRoad';c=curveWithNubs';

allNubs = map (flip mkTile nubs') $ 
    [
      [b,b,y,y]
    --
    , [r,b,b,y] 
    --
    , [r,b,y,b]
    ]

allStraights = map (flip mkTile straightWithNubs') $
    [
    --
      [b,r,b]
      --
    , [b,r,y]
    --
    , [b,y,b]
    --
    , [b,y,y]
    --
    , [r,b,r]
    --
    , [r,y,b]
    , [r,y,b]
    --
    , [r,y,y]
    , [r,y,y]
    --
    , [y,b,r]
    --
    , [y,r,y]
    --
    , [y,y,b]
    --
    , [y,y,y]
    ]

allUndies = map (flip mkTile undiesWithNub') $ 
    [ [b,r]
    , [r,y]
    , [y,b]
    ]

allCurves = map (flip mkTile curveWithNubs') $ 
    [ 
    --
      [b,b,r]
    --
    , [b,b,y]
    --
    , [b,r,r]
    --
    , [b,r,y]
    , [b,r,y]
    --
    , [b,y,y]
    -- 
    , [r,b,b]
    --
    , [r,r,r]
    --
    , [r,r,y]
    , [r,r,y]
    --
    , [r,y,y]
    --
    , [y,r,y]
    --
    , [y,r,b]
    -- 
    , [y,b,b]
    ]

allDoubleCurves = map (flip mkTile doubleCurvedRoad') $
    [ 
      [b,b]
    , [b,b]
      -- 
    , [b,r]
    , [b,r]
    -- 
    , [b,y]
    , [b,y]
    --
    , [r,r]
    , [r,r]
    --
    , [r,y]
    , [r,y]
    --
    , [y,y]
    , [y,y]
    ]

finalTiles :: [Tile]
finalTiles = concat
    [ allNubs
    , allStraights
    , allUndies
    , allCurves
    , allDoubleCurves
    ]

renderFinal :: IO ()
renderFinal = do
    putStrLn $ "rendering " <> show (length finalTiles) <> " tiles"
    void $ concurrently
                (renderMany
                    "final"
                    "final" 
                    (mkWidth 1000)
                    finalTiles)
                (renderMany
                    "pnp_tiles"
                    "pnp_tiles"
                    (mkWidth 4800)
                    (layoutFinal finalTiles))

