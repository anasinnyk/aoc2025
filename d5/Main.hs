import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  putStrLn "AOC 2025 #5"

input :: FilePath -> IO String
input = readFile

parse :: String -> ([(Int, Int)], [Int])
parse s =
  let
    ls = lines s
    getTuple :: String -> String -> (Int, Int)
    getTuple acc (x : xs)
      | x == '-' = (read $ reverse acc, read xs)
      | otherwise = getTuple (x : acc) xs
   in
    ( makeTupleMinimize (0, 0) $ sortBy (comparing fst) $ map (getTuple "") $ filter (elem '-') ls
    , map read $ filter (\s -> mempty /= s && notElem '-' s) ls
    )

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ length $ filterFresh $ parse inp

makeTupleMinimize :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
makeTupleMinimize r [] = [r]
makeTupleMinimize (s, e) ((s', e') : xs) =
  if e < s'
    then (s, e) : makeTupleMinimize (s', e') xs
    else makeTupleMinimize (s, max e e') xs

filterFresh :: ([(Int, Int)], [Int]) -> [Int]
filterFresh d =
  let
    availableIDs = snd d
    rangeFreshIDs = fst d
    isFresh :: Int -> [(Int, Int)] -> Bool
    isFresh _ [] = False
    isFresh x ((s, e) : ss) = (x >= s && x <= e) || isFresh x ss
   in
    filter (`isFresh` rangeFreshIDs) availableIDs

genRanges :: [(Int, Int)] -> Int
genRanges [] = 0
genRanges ((s, e) : xs) = e - s + 1 + genRanges xs

solve' :: FilePath -> IO ()
solve' f = do
  inp <- input f
  print $ genRanges $ tail $ fst $ parse inp
