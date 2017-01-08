module TrashKings.Types where

import Diagrams.Prelude
import Diagrams.Backend.SVG

type Tile = Diagram B

data Corner = TL | TR | BL | BR
    deriving (Show, Eq, Enum)

data CDir = T | L | R | B
    deriving (Show, Eq, Enum)

