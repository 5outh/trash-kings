module TrashKings.Types where

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- Some aliases to easily swap things out
type Diagram' = Diagram B
type Tile = Diagram B

data TileType
    = Nubs
    | Straight
    | DoubleCurve
    | CurveNub
    | Undies
      deriving (Show, Eq, Enum)

data Corner = TL | TR | BL | BR
    deriving (Show, Eq, Enum)

data CDir = T | L | R | B
    deriving (Show, Eq, Enum)

