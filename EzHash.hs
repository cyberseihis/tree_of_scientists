module Main where

import LshMinhash
import Data.List (group, sort)
import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.List.Split (chunksOf)

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

checkBins :: [Int] -> [Int] -> Int -> Int -> Bool
checkBins minh1 minh2 r b =
    let swag1 = take b . chunksOf r $ minh1
        swag2 = take b . chunksOf r $ minh2
    in or $ zipWith (==) swag1 swag2

reconsideration xs r b =
    [checkBins x y r b|x<-xs,y<-xs,x/=y]

reportChecks r b t = do
    minhas <- readFile "myHashes.csv"
    let mhas = read minhas :: [[Int]]
        reon = reconsideration mhas r b
    sx <- readFile "jaccardsFull.csv"
    let jac = read sx :: [Double]
        stuc = map (>t) jac
        bro = map (head &&& length) .
            group . sort $ zip reon stuc
    return bro

saveMinHashes = 
    readFile "sorteddata.csv" >>=
    writeFile "myHashes.csv" . rephrase
    
-- For r=2 b=50 (which I will be using)
-- [((False,False),4982),((False,True),210),((True,False),3024),((True,True),1096)]

sortData = readFile "sorteddata.csv"
-- Stats of similarities
-- [(.0,1153),(.1,2784),(.2,672),(.3,44),(.4,3)]
-- max similariry is .44
-- [((False,False),3345),((False,True),49),((True,False),658),((True,True),604)] for signares O.2
getAllJacs =
    sortData >>= writeFile "jaccardsFull.csv". show . getAnIdea2 . lines

main = print "No main here you got mislead"
