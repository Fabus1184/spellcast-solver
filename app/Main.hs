{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Criterion.Measurement (getTime, initializeTime)
import Data.Char (toUpper)
import Data.Ix (inRange)
import Data.List.Extra (chunksOf, sortOn)
import Data.Tuple.Extra (both, thd3)
import Formatting (fixed, formatToString, int, right, shown, string, (%), (%.))

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Trie as T

-- | Print a 5x5 grid
printGrid :: [Char] -> IO ()
printGrid = mapM_ (putStrLn . unwords . map (pure . toUpper)) . chunksOf 5

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

reward :: BS.ByteString -> Int
reward = sum . map (points HM.!) . BS.unpack

type GridByIndex = HM.HashMap Int Char
type GridByChar = HM.HashMap Char [Int]

f :: [Int] -> GridByIndex -> [Char]
f path gridByIndex = map (gridByIndex HM.!) path

mapWords :: [Int] -> T.Trie () -> (GridByIndex, GridByChar) -> T.Trie [Int]
mapWords [] trie (gridByIndex, gridByChar) =
    foldl1 T.unionL
        . map (\i -> mapWords [i] trie (gridByIndex, gridByChar))
        $ [0 .. 24]
mapWords path trie (gridByIndex, gridByChar) =
    let xs = filter (`notElem` path) (neighbours (last path))
        xs' = map (\x -> path ++ [x]) xs
        xs'' = map (\x -> (x, T.submap (BS.pack $ map (gridByIndex HM.!) x) trie)) xs'
        xs''' = map fst $ filter (not . T.null . snd) xs''
     in foldl T.unionL T.empty
            . (:) (T.fromList $ filter ((`T.member` trie) . fst) [(BS.pack $ f path gridByIndex, path)])
            . map (\x -> mapWords x (T.submap (BS.pack $ map (gridByIndex HM.!) x) trie) (gridByIndex, gridByChar))
            $ xs'''

main :: IO ()
main = do
    initializeTime
    t1 <- getTime

    trie <- T.fromList . map (,()) . BS.lines . BS.map toUpper <$> BS.readFile "words.txt"
    let grid =
            concat
                [ "AMTVL"
                , "ROLAD"
                , "IINIE"
                , "NIPGU"
                , "KHUSO"
                ]
    let gridByIndex = HM.fromList $ zip [0 ..] grid :: HM.HashMap Int Char
    let gridByChar = HM.fromList $ map (\c -> (c, filter (\i -> HM.lookup i gridByIndex == Just c) [0 .. 24])) ['A' .. 'Z']
    mapM_
        ( \(w, p, r) ->
            putStrLn $
                formatToString
                    ((right 12 ' ' %. string) % (right 5 ' ' %. int) % shown)
                    (BS.unpack w)
                    r
                    p
        )
        . sortOn thd3
        . map (\(w, p) -> (w, p, reward w))
        . T.toList
        $ mapWords [] trie (gridByIndex, gridByChar)

    printGrid grid

    t2 <- getTime
    putStrLn $ formatToString ("took " % fixed 3 % "s") (t2 - t1)
