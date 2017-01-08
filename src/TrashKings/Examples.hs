module TrashKings.Examples where

import Data.Monoid
import Diagrams.Backend.SVG
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Size
import TrashKings.Layout
import TrashKings.Types

-- Generate a named example
genExample :: FilePath -> Diagram' -> IO ()
genExample filepath =
    renderSVG ("examples/" <> filepath) (mkWidth 1000)

doIt = do
    let p1 = (0, 0)
        p2 = (3, 0)
    undefined

