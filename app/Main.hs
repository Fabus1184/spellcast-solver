{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, replicateM)
import Criterion.Measurement (getTime, initializeTime)
import Data.Char (toUpper)
import Data.Ix (inRange)
import Data.List.Extra (foldl', sortOn)
import Data.Tuple.Extra (both, snd3, thd3)
import Formatting (fixed, formatToString, int, right, shown, string, (%), (%.))
import String.ANSI (bold, brightBlue, brightGreen, red, yellow)
import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Trie as T

-- | Print a 5x5 grid with the given path highlighted
printGrid :: [Int] -> GridByIndex -> IO ()
printGrid path g = do
    forM_ [0 .. 24] $ \i -> do
        if i `elem` path
            then putStr . brightGreen . bold $ [HM.lookupDefault ' ' i g]
            else putStr [HM.lookupDefault ' ' i g]
        putStr $ if i `mod` 5 == 4 then "\n" else " "

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

-- | The points for each letter
points :: HM.HashMap Char Int
points =
    HM.fromList
        [ ('A', 1)
        , ('B', 4)
        , ('C', 5)
        , ('D', 3)
        , ('E', 1)
        , ('F', 5)
        , ('G', 3)
        , ('H', 4)
        , ('I', 1)
        , ('J', 7)
        , ('K', 6)
        , ('L', 3)
        , ('M', 4)
        , ('N', 2)
        , ('O', 1)
        , ('Q', 8)
        , ('P', 4)
        , ('R', 2)
        , ('S', 2)
        , ('T', 2)
        , ('U', 4)
        , ('V', 5)
        , ('W', 5)
        , ('X', 7)
        , ('Y', 4)
        , ('Z', 8)
        ]

-- | The reward for a word
reward :: BS.ByteString -> Int
reward = sum . map (points HM.!) . BS.unpack

type GridByIndex = HM.HashMap Int Char
type GridByChar = HM.HashMap Char [Int]

-- | Get the word for a given path
f :: [Int] -> GridByIndex -> [Char]
f path gridByIndex = map (gridByIndex HM.!) path

-- | Map words to paths
mapWords :: [Int] -> T.Trie () -> (GridByIndex, GridByChar) -> IO (T.Trie [Int])
mapWords [] trie (gridByIndex, gridByChar) = do
    !rs <- mapConcurrently (\i -> mapWords [i] trie (gridByIndex, gridByChar)) [0 .. 24]
    pure $! foldl' T.unionL T.empty rs
mapWords path trie (gridByIndex, gridByChar) = do
    !rs <-
        ( mapM
                ( ( \x ->
                        mapWords
                            x
                            (BS.pack (map (gridByIndex HM.!) x) `T.submap` trie)
                            (gridByIndex, gridByChar)
                  )
                    . fst
                )
                . filter (not . T.null . snd)
            )
            . map
                ( ( \x ->
                        (x, T.submap (BS.pack $ map (gridByIndex HM.!) x) trie)
                  )
                    . (\x -> path ++ [x])
                )
            $ filter (`notElem` path) (neighbours (last path))
    pure
        $ foldl' T.unionL T.empty
            . (:) (T.fromList $ filter ((`T.member` trie) . fst) [(BS.pack $ f path gridByIndex, path)])
        $ rs

main :: IO ()
main = do
    initializeTime
    args <- getArgs
    !trie <- T.fromList . map (,()) . BS.lines . BS.map toUpper <$> BS.readFile (head args)
    -- let !grid =
    --        concat
    --            [ "SSUTD"
    --            , "ATOPA"
    --            , "HIAPI"
    --            , "TOKNT"
    --            , "OSCHW"
    --            ]
    !grid <- replicateM 25 $ randomRIO ('A', 'Z')
    let !gridByIndex = HM.fromList $ zip [0 ..] grid :: HM.HashMap Int Char
    let !gridByChar = HM.fromList $ map (\c -> (c, filter (\i -> HM.lookup i gridByIndex == Just c) [0 .. 24])) ['A' .. 'Z']

    t1 <- getTime
    !rs <-
        sortOn thd3
            . map (\(w, p) -> (w, p, reward w))
            . T.toList
            <$> mapWords [] trie (gridByIndex, gridByChar)
    t2 <- getTime
    print $ length rs
    mapM_
        ( \(w, p, r) ->
            putStrLn
                . (if (w, p, r) == last rs then bold else id)
                . ( case r `div` 10 of
                        0 -> red
                        1 -> yellow
                        2 -> brightGreen
                        _ -> id
                  )
                $ formatToString
                    ((right 25 ' ' %. string) % (right 5 ' ' %. int) % shown)
                    (BS.unpack w)
                    r
                    p
        )
        rs

    putStrLn ""
    printGrid (snd3 $ last rs) gridByIndex
    putStrLn ""

    putStrLn $ brightBlue $ formatToString ("search took " % fixed 3 % "s, found " % int % " words") (t2 - t1) (length rs)
