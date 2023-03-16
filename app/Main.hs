module Main (main) where
--https://www.youtube.com/watch?v=tm60fuF5v54
--tsoding daily pogchamp

import Lib
import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file1, file2]  -> hiff file1 file2
        _               -> usage

usage :: IO ()
usage = do
    pName <- getProgName
    putStrLn $ "Usage: " ++ pName ++ " " ++ "<file1> <file2>"

hiff :: String -> String -> IO ()
hiff file1 file2 = do
    d <- diff file1 file2
    case d of
        Right diff -> putStrLn diff
        Left err -> putStrLn $ show $ err

type DiffResult = Either String String

diff :: String -> String -> IO DiffResult 
diff file1 file2 = do
    sFile1 <- ((liftM lines) . readFile) file1
    sFile2 <- ((liftM lines) . readFile) file2

    let lev = levenshtein' sFile1 sFile2
    let path = pathFromLev sFile1 sFile2 lev
    return $ Right $ foldl (\acc add -> acc ++ show add ++ "\n") "" path


levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein a b = levenshtein' a b ! (length a, length b)


levenshtein' :: (Eq a) => [a] -> [a] -> Array (Int, Int) Int
levenshtein' a b =
    let (n, m) = (length a, length b) 
    in runSTArray $ do
        table <- thaw $ array ((0, 0), (n, m)) [((i, j), 0) | i <- [0..n], j <- [0..m]] :: ST s (STArray s (Int, Int) Int)
        
        forM_ [(i, 0) | i <- [0..n]] (\(i, j) -> writeArray table (i, j) i)
        forM_ [(0, j) | j <- [0..m]] (\(i, j) -> writeArray table (i, j) j)

        forM_ [(i, j) | i <- [1..n], j <- [1..m]] (\(i, j) -> do 

            { let subCost = if a !! (i-1) == b !! (j-1) then 0 else 1
            ; left  <- readArray table (i-1, j)
            ; up    <- readArray table (i,   j-1)
            ; diag  <- readArray table (i-1, j-1)
            ; let m = minimum   [ left + 1
                                , up   + 1
                                , diag + subCost ]
            ; writeArray table (i, j) m
            ; return ()
            })

        return table

data Action a   = ADD a Int 
                | REMOVE a Int

instance Show a => Show (Action a) where
    show (ADD    s at) = "+ " ++ show at ++ " " ++ show s
    show (REMOVE s at) = "- " ++ show at ++ " " ++ show s 


pathFromLev :: (Eq a) => [a] -> [a] -> Array (Int, Int) Int -> [Action a]
pathFromLev a b lev = 
    let ((_, _), (height, width)) = bounds lev
    in pathFromLev' a b lev (height, width) []

pathFromLev' :: (Eq a) => [a] -> [a] -> Array (Int, Int) Int -> (Int, Int) -> [Action a] -> [Action a]
pathFromLev' a b lev (0, j) acc = [ADD      (b !! j) 0 | j <- [0..j-1]] ++ reverse acc
pathFromLev' a b lev (i, 0) acc = [REMOVE   (a !! i) i | i <- [0..i-1]] ++ reverse acc
pathFromLev' a b lev (i, j) acc = 
    let l = lev ! (i,   j-1)
        u = lev ! (i-1,   j)
        d = lev ! (i-1, j-1)
        isMatch = a !! (i-1) == b !! (j-1)
    in  if isMatch && d == minimum [l, u, d]
            then pathFromLev' a b lev (i-1, j-1) (acc)
        else if d == minimum [l, u, d]
            then pathFromLev' a b lev (i-1, j-1) (ADD (b !! (j-1)) i : REMOVE (a !! (i-1)) i : acc)
        else if u == minimum [l, u, d]
            then pathFromLev' a b lev (i-1, j) (REMOVE (a !! (i-1)) i : acc)
        else if l == minimum [l, u, d]
            then pathFromLev' a b lev (i, j-1) (ADD (b !! (j-1)) i : acc)
        else undefined
    