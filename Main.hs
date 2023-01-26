module Main where

import Commander
import Dumped (rtree,ktree,qtree,twotree)
import Data.List (sort)

main = do
    print . sort $ execution ktree (Query 'H' 'R' 2 8)
