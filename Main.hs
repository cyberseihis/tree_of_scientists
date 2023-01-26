module Main where

import Commander
import Dumped (ktree,theBands,wordSets)
import Data.List (sort)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))

jac = uncurry sanityJacc . (ws *** ws) where
    ws = (wordSets!!)

main = do
    let query = Query 'H' 'R' 2 8
        exe = execution ktree query
    print . filter jac $ candidates theBands exe
    print . filter jac $ everyPair exe
    print $ candidates theBands exe
