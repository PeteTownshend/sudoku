module Main where
import Lib
import System.Environment
import Control.Concurrent
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f
    let 
        puzzles = map parse $ lines file
        solver puzzle = puzzle /= solve puzzle
        solutions = runEval $ do
            solved <- rpar (force $ map solver puzzles)
            rseq solved
            return solved
    print (length solutions)