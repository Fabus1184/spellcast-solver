{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens (imap)
import Criterion.Measurement (getTime, initializeTime)
import Data.Array (Array, array, inRange, (!))
import Data.Char (toLower, toUpper)
import Data.Foldable (Foldable (toList))
import Data.List.Extra (chunksOf, nubOrd, sortOn, (\\))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both, fst3)
import Formatting (formatToString, int, right, string, (%), (%.))

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Trie as T

type Grid = Array Int Char

-- | Print a 5x5 grid
printGrid :: Grid -> IO ()
printGrid arr = mapM_ (putStrLn . unwords . map (pure . toUpper)) . chunksOf 5 $ toList arr

-- | Get the neighbours of a given row and col in a 5x5 grid
neighbours' :: (Int, Int) -> [(Int, Int)]
neighbours' (row, col) =
    filter
        (uncurry (&&) . both (inRange (0, 4)))
        [ (row - 1, col - 1)
        , (row - 1, col)
        , (row - 1, col + 1)
        , (row, col - 1)
        , (row, col + 1)
        , (row + 1, col - 1)
        , (row + 1, col)
        , (row + 1, col + 1)
        ]

-- | Get the neighbours of a given index in a 5x5 grid
neighbours :: Int -> [Int]
neighbours i = map (\(r, c) -> r * 5 + c) $ neighbours' (i `div` 5, i `mod` 5)

-- | Depth-first search of a 5x5 grid with a given maximum depth
dfs :: Int -> Int -> [[Int]]
dfs maxDepth start =
    dfs' maxDepth start []
  where
    dfs' :: Int -> Int -> [Int] -> [[Int]]
    dfs' 0 _ vt = [vt]
    dfs' depth start' vt =
        let ns = neighbours start' \\ vt
         in nubOrd $ concatMap (\n -> dfs' (depth - 1) n (start' : vt)) ns

-- | The points for each letter
points :: HM.HashMap Char Int
points =
    HM.fromList
        [ ('a', 1)
        , ('d', 3)
        , ('e', 1)
        , ('f', 5)
        , ('g', 3)
        , ('h', 4)
        , ('i', 1)
        , ('k', 6)
        , ('l', 3)
        , ('m', 4)
        , ('n', 2)
        , ('o', 1)
        , ('p', 4)
        , ('r', 2)
        , ('s', 2)
        , ('t', 2)
        , ('u', 4)
        , ('v', 5)
        , ('w', 5)
        , ('x', 7)
        , ('y', 4)
        , ('z', 8)
        ]

main :: IO ()
main = do
    initializeTime
    putStrLn "HIER"
    trie <- T.fromList . map (,()) . BS.lines <$> BS.readFile "words.txt"
    let arr =
            array (0, 24)
                . imap (,)
                . map toLower
                . concat
                $ [ "TSSOK"
                  , "NODOV"
                  , "AMPEN"
                  , "STXEG"
                  , "ZWTYI"
                  ]
    results <- newMVar HS.empty
    let ws i =
            let !rs =
                    HS.fromList
                        . concatMap
                            ( (map fst3 . T.matches trie)
                                . BS.pack
                                . map (arr !)
                            )
                        $ dfs 10 i
             in modifyMVar_ results (pure . HS.union rs)
    t <- getTime
    mapConcurrently_ ws [0 .. 24]
    readMVar results
        >>= mapM_ (\(p, w) -> putStrLn $ formatToString ((right 12 ' ' %. string) % " " % int) w p)
            . sortOn fst
            . map (sum . fmap (\x -> fromMaybe (error (show x)) (HM.lookup x points)) >>= (,))
            . fmap BS.unpack
            . HS.toList
    t' <- getTime
    putStrLn $ "Took " ++ show (t' - t) ++ " seconds"
    printGrid arr
