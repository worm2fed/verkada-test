module Merge where

import System.IO (IOMode (..), hClose, hPrint, openFile)
import Prelude

-- input_files := random_file_0.txt random_file_1.txt random_file_2.txt random_file_3.txt random_file_4.txt

-- ./final_output.txt

-- input data: sorted:  compare the first element of each list
-- input data: not sort:  sort each file and then treat as sorted

-- Merge:
-- Read all into memory
-- Read one line each time (only when input data is sorted)

-- Time complexity:    m file, each file has n elements.
-- (m * n) + n * m * log(m) = m * n (1 + log(m)) = m * n * log(m)
-- Space complexity:
-- m * n + k^m

launchKWayMerge :: IO ()
launchKWayMerge = do
  let fileNames =
        [ "random_file_0.txt"
        , "random_file_1.txt"
        , "random_file_2.txt"
        , "random_file_3.txt"
        , "random_file_4.txt"
        ]
  input <- map (map read . lines) <$> mapM readFile fileNames
  let result = kWayMerge input
  outh <- openFile "./final_output.txt" WriteMode
  mapM_ (hPrint outh) result
  hClose outh

-- [[1,2,3], [2,5,6], ...] ->
kWayMerge :: [[Double]] -> [Double]
kWayMerge [] = []
kWayMerge [x] = x
kWayMerge [a, b] = merge a b
kWayMerge xs = do
  let size = length xs `div` 2
  let firstPart = take size xs
  let secondPart = drop size xs
  merge (kWayMerge firstPart) (kWayMerge secondPart)

-- [1,2,3] -> [2,5,6] -> [1,2,2,3,5,6]
merge :: [Double] -> [Double] -> [Double]
merge [] [] = []
merge a [] = a
merge [] b = b
merge first@(a : as) second@(b : bs)
  | a < b = a : merge as second
  | a == b = a : b : merge as bs
  | otherwise = b : merge bs first
