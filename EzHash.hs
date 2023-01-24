module Main where

import LshMinhash
import Data.List (group, sort)
import Control.Arrow ((&&&))
import Data.Functor ((<&>))

educate = unwords . drop 2 . words
stories = map educate . lines
rephrase = show . map license . stories

isMinOk x y =
    let gold = jaccardSimilarity x y > 0.2
        dut = sigSimilarity x y > 0.2
    in (dut,gold)

minHashTest xs =
    let ys = [isMinOk x y|x<-xs,y<-xs,x>y]
        ls = group . sort $ ys
        nx = map (head &&& length) ls
    in nx

getAnIdea xs =
    [jaccardSimilarity x y|x<-xs,y<-xs,x>y]

getAnIdea2 xs =
    [jaccardSimilarity x y|x<-xs,y<-xs,x/=y]

secondOpinion xs =
    [sigKinda x y|x<-xs,y<-xs,x/=y]

sortData = readFile "sorteddata.csv"
-- Stats of similarities
-- [(.0,1153),(.1,2784),(.2,672),(.3,44),(.4,3)]
-- max similariry is .44
-- [((False,False),3345),((False,True),49),((True,False),658),((True,True),604)] for signares O.2
main =
    sortData >>= print . minHashTest . lines
