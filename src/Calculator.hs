module Calculator (calculate) where

import Prelude

import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import Data.List (nub, sortBy)
import Data.Ord (Down (..), comparing)

-- calculate :: String -> IO Int
calculate :: String -> Int
calculate input = do
  let args = parseArgs $ filter (not . isSpace) input
  -- print args
  let (valuesStack, operationsStack) = buildStacks args
  -- print (valuesStack, operationsStack)
  case evaluate valuesStack operationsStack of
    [a] -> a
    _otherwise -> error "there is no or more then 1 result"

parseArgs :: String -> [String]
parseArgs =
  foldl'
    ( \arguments c ->
        if isDigit c
          then case arguments of
            [] -> arguments ++ [[c]]
            mDigit -> do
              let digit = last mDigit
              if all isDigit digit
                then init mDigit ++ [digit ++ [c]]
                else arguments ++ [[c]]
          else arguments ++ [[c]]
    )
    []

buildStacks :: [String] -> ([Int], [String])
buildStacks =
  foldl'
    ( \(values, operations) arg ->
        if all isDigit arg
          then (values ++ [read arg], operations)
          else (values, operations ++ [arg])
    )
    ([], [])

evaluate :: [Int] -> [String] -> [Int]
evaluate valuesStack operationsStack =
  foldl'
    ( \values priority ->
        executePriority priority values operationsStack
    )
    valuesStack
    . sortBy (comparing Down)
    . nub
    $ map getPriority operationsStack

executePriority :: Int -> [Int] -> [String] -> [Int]
executePriority priority =
  foldl'
    ( \values op ->
        if getPriority op == priority
          then case values of
            a : b : rest -> getOperation op a b : rest
            _otherwise -> error "values stack should not be empty"
          else case values of
            a : rest -> rest ++ [a]
            _otherwise -> error "values stack should not be empty"
    )

getOperation :: String -> (Int -> Int -> Int)
getOperation "x" = (*)
getOperation "*" = (*)
getOperation "+" = (+)
getOperation op = error $ "unsupported operation: " <> op

getPriority :: String -> Int
getPriority "x" = 7
getPriority "*" = 7
getPriority "+" = 6
getPriority op = error $ "unsupported operation: " <> op
