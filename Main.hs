module Main where
import Commander
import Dumped (rtree,theBands,wordSets,prettys)
import Data.List (sort)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))

main = getLine >>= mapM_ prettyPrint .filter jac . candidates theBands.
    execution rtree . parse.words
    where
    jac = uncurry sanityJacc . (ws *** ws)
    ws = (wordSets!!)
    prettyPrint (i,j) = mapM_ putStrLn [prettys!!i,prettys!!j,""]
    parse [[ls],[lm],ss,sm] = Query ls lm (read ss) (read sm)
