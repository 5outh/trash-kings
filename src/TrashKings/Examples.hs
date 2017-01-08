{-# LANGUAGE FlexibleContexts #-}
module TrashKings.Examples where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Size
import Diagrams.Prelude
import TrashKings.Layout
import TrashKings.Types
import Control.Monad.Random
import TrashKings.Generation

-- Generate a named example
genExample :: FilePath -> Diagram' -> IO ()
genExample filepath =
    renderSVG
        ("examples/" <> filepath <> ".svg")
        (mkWidth 1000)

example sDot eDot sPt ePt = 
    sDot # named "1" # moveTo sPt
    <> eDot # named "2" # moveTo ePt

doIt :: MonadRandom m => m Diagram'
doIt = do
    let pA = p2 (0, 0)
        pB = p2 (2, 0)
    (t1, t2) <- (,) <$> genTile <*> genTile
    pure $ connectOutside "1" "2" $ example t1 t2 pA pB

go = do
    tile <- evalRandIO doIt
    genExample "test" tile
