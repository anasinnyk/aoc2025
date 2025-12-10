{-# LANGUAGE TupleSections #-}

import Data.Map as M (Map, filter, fromList, insert, map, size, (!))

main :: IO ()
main = do
  putStrLn "AOC 2025 #4"

input :: FilePath -> IO String
input = readFile

parse :: String -> Map Int (Map Int Char)
parse s = fromList . zip [1 ..] $ Prelude.map (fromList . zip [1 ..]) $ lines s

emptyRow :: Int -> Map Int Char
emptyRow s = fromList $ take s $ Prelude.map (,emptyElement) [1 ..]

emptyElement :: Char
emptyElement = '.'

xElement :: Char
xElement = 'x'

addEmptySquer :: Map Int (Map Int Char) -> Map Int (Map Int Char)
addEmptySquer m =
  let
    withZeroRow m = insert 0 (emptyRow $ size $ m ! 1) m
    withLatestRow m = insert (size m) (emptyRow $ size $ m ! 1) m
    withZeroCol = fmap (insert 0 emptyElement)
    withLatestCol m = fmap (insert (size $ m ! 1) emptyElement) m
   in
    withLatestCol $ withZeroCol $ withLatestRow $ withZeroRow m

updateWithX :: Map Int (Map Int Char) -> Map Int (Map Int Char)
updateWithX sm = updateWithXBy (cSize - 1) (rSize - 1) sm
 where
  cSize = size sm
  rSize = size $ sm ! 1

updateWithXBy :: Int -> Int -> Map Int (Map Int Char) -> Map Int (Map Int Char)
updateWithXBy 0 0 m = m
updateWithXBy x 0 m = updateWithXBy (x - 1) (size (m ! 1) - 1) m
updateWithXBy x y m | m ! x ! y /= '@' = updateWithXBy x (y - 1) m
updateWithXBy x y m =
  let
    rollNeigbors =
      [ m ! (x - 1) ! (y - 1)
      , m ! x ! (y - 1)
      , m ! (x + 1) ! (y - 1)
      , m ! (x - 1) ! y
      , m ! x ! y
      , m ! (x + 1) ! y
      , m ! (x - 1) ! (y + 1)
      , m ! x ! (y + 1)
      , m ! (x + 1) ! (y + 1)
      ]
    rollsWithNeigborsSize = length $ Prelude.filter (/= emptyElement) rollNeigbors
   in
    updateWithXBy x (y - 1) $ if 5 <= rollsWithNeigborsSize then m else insert x (insert y xElement $ m ! x) m

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ countX $ updateWithX $ addEmptySquer $ parse inp

countX :: Map Int (Map Int Char) -> Int
countX = foldr (\m acc -> acc + size (M.filter (== xElement) m)) 0

removeX :: Map Int (Map Int Char) -> Map Int (Map Int Char)
removeX =
  let
    changeXToEmpty a | a == xElement = emptyElement
    changeXToEmpty a = a
   in
    fmap (M.map changeXToEmpty)

solveAllRolls :: Int -> Map Int (Map Int Char) -> Int
solveAllRolls acc m | countX (updateWithX m) == 0 = acc
solveAllRolls acc m =
  let
    m' = updateWithX m
   in
    solveAllRolls (acc + countX m') (removeX m')

solve' :: FilePath -> IO ()
solve' f = do
  inp <- input f
  print $ solveAllRolls 0 $ addEmptySquer $ parse inp
