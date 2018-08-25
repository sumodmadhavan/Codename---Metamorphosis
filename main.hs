module Main where

import System.Environment
import System.IO
import Data.Set (empty)
import Control.Monad.Trans.State.Lazy
import IterativeDeepeningDFS as IFS
import Metamorphis 


-- list of calls to the various search functions
solve :: String -> Lake -> Game -> Moves
solve "bfs" goal start =
    formatA $ runState (BFS.solveM expand (isGoal goal) start) (0, 0, empty)
solve "dfs" goal start =
    formatA $ runState (DFS.solveM expand (isGoal goal) start) (0, 0, empty)
solve "iddfs" goal start =
    formatB $ IFS.solveM (empty, 0, 0) basicExpand (isGoal goal) 2 [start]
solve "astar" goal start = 
    formatA $ runState (ASS.solveM heuristic (isGoal goal) start) (0, 0, empty)


-- main entry point
main :: IO ()
main = do 
        args <- getArgs
        case args of
            (w:x:y:z:[]) -> if supported y
                                then do
                                  start <- openFile w ReadMode
                                  goal  <- openFile x ReadMode
                                  out   <- openFile z WriteMode
                                  mainsub start goal y out
                                  hClose start
                                  hClose goal
                                  hClose out
                                else putStrLn help
            _            -> putStrLn help


-- main subfunction
mainsub :: Handle -> Handle -> String -> Handle -> IO ()
mainsub start goal mode output = do
                    a <- hGetLine start
                    b <- hGetLine start
                    c <- hGetLine goal
                    d <- hGetLine goal
                    let startState = createtuple a b
                    let goalState = createtuple c d
                    let results = if goodBoats startState && supported mode
                                  then solve mode goalState (startState, [show startState])
                                  else [help]
                    mapM_ (hPutStrLn output) results
                    mapM_ putStrLn results


-- formats the goal state for printing
formatA (Nothing, (n, _, _)) =
    reverse $ (show n ++ " nodes expanded") : ["no solution found"]
formatA (Just (_, m), (n, _, _)) =
    reverse $ (show n ++ " nodes expanded") : m

formatB (Nothing, _, n) =
    reverse $ (show n ++ " nodes expanded") : ["no solution found"]
formatB (Just (_, m), _, n) =
    reverse $ (show n ++ " nodes expanded") : m

-- split a String at a given character
split' :: (Char -> Bool) -> String -> [String]
split' p s = case dropWhile p s of "" -> []
                                   s' -> w : split' p s''
                                    where (w, s'') = break p s'

-- groups the input numbers together
createtuple :: String -> String -> Lake
createtuple x y = tuplefy $ split' (==',') x ++ split' (==',') y

-- reads the input numbers to create a Lake type
tuplefy :: [String] -> Lake
tuplefy [a,b,c,x,y,z] = (read a::Int, read b::Int,read c::Int, read x::Int,read y::Int, read z::Int)

-- tests the input mode against the supported list
supported :: String -> Bool
supported y = y `elem` ["bfs","dfs","iddfs", "astar"]

-- ensures the input state is valid
goodBoats :: Lake -> Bool
goodBoats (_,_,c,_,_,z) = c >= 0 && z >= 0 && c + z == 1

