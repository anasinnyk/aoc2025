{-# LANGUAGE OverloadedStrings #-}

import Data.Array (Array, Ix, array, assocs, listArray, (!), (//))
import Data.Function (on)
import Data.List (delete, nub, sort, sortBy)
import Data.Text (Text, pack, splitOn, unpack)
import GHC.Arr (fill)

main :: IO ()
main = do
  putStrLn "AOC 2025 #8"

input :: FilePath -> IO String
input = readFile

type Point = (Double, Double, Double)

getX :: Point -> Double
getX (x, _, _) = x

parse :: String -> [Point]
parse s = map ((\(x : y : z : _) -> (x, y, z)) . map (read . unpack) . splitOn "," . pack) $ lines s

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  let ps = parse inp
  let arr = array (0, length ps - 1) $ zip [0 ..] ps
  let arr' = array (0, length ps - 1) $ map (,[]) [0 .. length ps - 1]
  let dists = filter (\(_, _, r) -> r > 0) $ sortBy (\(_, _, d) (_, _, d') -> compare d d') $ concat $ createMatrixWithDistance arr
  let res = map fst $ filter (odd . snd) $ zip dists [1 ..]
  let res10 = take 10 res
  let res1000 = take 1000 res
  let res' = fillTheList arr' res ! 0

  print $ product $ map length $ take 3 $ sortBy (\x y -> length y `compare` length x) $ (\arr'' -> nub [sort (i : ns) | (i, ns) <- assocs arr'']) $ fillTheList arr' res1000
  print $ product $ map length $ take 3 $ sortBy (\x y -> length y `compare` length x) $ (\arr'' -> nub [sort (i : ns) | (i, ns) <- assocs arr'']) $ fillTheList arr' res10
  print $ getX (arr ! head res') * getX (arr ! last res')

fillTheList :: (Ix i) => Array i [i] -> [(i, i, b)] -> Array i [i]
fillTheList arr [] = arr
fillTheList arr ((i, j, _) : xs) =
  let
    groupI = i : arr ! i
    groupJ = j : arr ! j
    alreadyConnected = j `elem` groupI
   in
    if alreadyConnected
      then fillTheList arr xs
      else
        let
          group = nub (groupI ++ groupJ)
          updates = [(x, delete x group) | x <- group]
         in
          fillTheList (arr // updates) xs

createMatrixWithDistance :: (Ix i) => Array i Point -> [[(i, i, Double)]]
createMatrixWithDistance ps = [[(i, j, dist x y) | (i, x) <- assocs ps] | (j, y) <- assocs ps]
 where
  dist (x', y', z') (x'', y'', z'') = sqrt $ (x' - x'') ^ 2 + (y' - y'') ^ 2 + (z' - z'') ^ 2
