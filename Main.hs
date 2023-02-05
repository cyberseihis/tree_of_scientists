module Main where
import Commander
import Dumped (theBands,wordSets,prettys)
import Kdtree
import KdTest
import Data.List (sort, singleton)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))

poinify = (\[c:_,i]->Pointx (alphaOrd c) (read i).singleton).words
ktree = make points :: Kd
points = zipWith poinify prettys [0..]

main = getLine >>= mapM_ prettyPrint .filter jac . candidates theBands.
    execution ktree . parse.words
    where
    jac = uncurry sanityJacc . (ws *** ws)
    ws = (wordSets!!)
    prettyPrint (i,j) = mapM_ putStrLn [prettys!!i,prettys!!j,""]
    parse [[ls],[lm],ss,sm] = Query ls lm (read ss) (read sm)
