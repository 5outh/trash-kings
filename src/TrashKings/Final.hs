{-# LANGUAGE RebindableSyntax #-}
module TrashKings.Final where

import Diagrams.Prelude hiding ((##))
import Diagrams.Backend.Rasterific

import TrashKings.Types
import TrashKings.Tile
import TrashKings.Generation

import Prelude hiding ((>>), return)
(##) = flip (,)

y = yellow'; b=blue'; r=red';
n=nubs';u=undiesWithNub';s=straightWithNubs'
dc=doubleCurvedRoad';c=curveWithNubs';

allNubs = map (flip mkTile nubs') $ 
    [
    --
      [b,b,r,r]
    --
    , [b,b,y,y]
    --
    , [r,b,b,y] 
    --
    , [r,b,y,b]
    --
    , [r,r,y,y]
    --
    , [r,y,y,y]
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
    [ [b,b]
    , [r,r]
    ]

allCurves = map (flip mkTile curveWithNubs') $ 
    [ 
    --
      [b,b,r]
    , [b,b,r]
    --
    , [b,b,y]
    , [b,b,y]
    --
    , [b,r,r]
    --
    , [b,r,y]
    , [b,r,y]
    , [b,r,y]
    , [b,r,y]
    , [b,r,y]
    --
    , [b,y,y]
    , [b,y,y]
    --
    , [r,r,r]
    --
    , [r,r,y]
    , [r,r,y]
    --
    , [r,y,y]
    ]

allDoubleCurves = map (flip mkTile doubleCurvedRoad') $
    [ 
      [b,b]
      -- 
    , [b,r]
    , [b,r]
    -- 
    , [b,y]
    , [b,y]
    --
    , [r,r]
    --
    , [r,y]
    , [r,y]
    , [r,y]
    , [r,y]
    , [r,y]
    --
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
renderFinal =
    renderMany
        "final"
        "final" 
        (mkWidth 675)
        finalTiles

