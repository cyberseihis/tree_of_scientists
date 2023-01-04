module Quad where
import Kdtree
import Data.List.NonEmpty (groupAllWith, toList)
import Data.Map.Strict ( Map )
import Control.Arrow (Arrow((&&&), first), (>>>))

data Qtree = Qempty | Qtree Point (Map Quad Qtree) deriving (Eq,Show)
type Quad = (Bool,Bool)

compQuarts x y = (x <= y, other x <= other y)

splitQuarts x ys =
    let z = map (compQuarts x &&& id) ys
        ff = groupAllWith fst z
        hh = map (toList >>> unzip >>> first head) ff
    in undefined
