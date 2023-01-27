module Main where

import Commander
import Dumped (twotree,theBands,wordSets,prettys)
import Data.List (sort)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))

jac = uncurry sanityJacc . (ws *** ws) where
    ws = (wordSets!!)

prettyPrint (i,j) = do
    putStrLn $ prettys!!i
    putStrLn $ prettys!!j
    putStrLn ""

main = do
    [[ls],[lm],ss,sm] <- words <$> getLine
    let query = Query ls lm (read ss) (read sm)
        exe = execution twotree query
        couples = filter jac $ candidates theBands exe
    mapM_ prettyPrint couples
