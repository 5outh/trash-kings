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

finalTiles :: [Tile]
finalTiles = map (uncurry mkTile) $ do
    n ## [y,b,r,r]
    u ##  [y,y]
    s ## [r,b,y]
    dc ## [r,b]
    -- And so on
    []
    where (>>) = (:); return x = [x]

renderFinal :: IO ()
renderFinal =
    renderMany
        "final"
        "final" 
        (mkWidth 675)
        finalTiles
