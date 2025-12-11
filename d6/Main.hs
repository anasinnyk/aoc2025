import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, transpose)

main :: IO ()
main = do
  putStrLn "AOC 2025 #6"

input :: FilePath -> IO String
input = readFile

parse :: String -> [([Int], Char)]
parse s =
  let
    ls = lines s
    lss = length ls
    digits = transpose $ map takeNums $ take (lss - 1) ls
    ops = filter (not . isSpace) $ last ls
   in
    zip digits ops

parse' :: String -> [(Char, [Int])]
parse' s =
  let
    ls = lines s
    lss = length ls
    ops = last ls
    clearOps = filter (not . isSpace) ops

    takeSignAndSize :: String -> Int -> [(Char, Int)]
    takeSignAndSize [] _ = []
    takeSignAndSize (x : xs) c =
      if isSpace x
        then takeSignAndSize xs (c + 1)
        else (x, c) : takeSignAndSize xs 0

    instr = reverse (takeSignAndSize (reverse ops) 1)

    processNumLine :: String -> [Int] -> [String]
    processNumLine _ [] = []
    processNumLine str (x : xs) = take x str : processNumLine (drop (x + 1) str) xs
   in
    zip clearOps $ map (map (\s -> read s :: Int) . transpose) $ transpose $ map (\s -> processNumLine s (map snd instr)) (take (lss - 1) ls)

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ sum $ map (\(is, c) -> solveByChar c is) $ parse inp

solve' :: FilePath -> IO ()
solve' f = do
  inp <- input f
  print $ sum $ map (uncurry solveByChar) $ parse' inp

takeNums :: String -> [Int]
takeNums [] = []
takeNums s = whileIsNotDigit "" $ dropWhileEnd isSpace $ dropWhile isSpace s
 where
  whileIsNotDigit :: String -> String -> [Int]
  whileIsNotDigit r [] = [read (reverse r)]
  whileIsNotDigit r (x : xs) =
    if isDigit x
      then whileIsNotDigit (x : r) xs
      else read (reverse r) : whileIsNotDigit "" (dropWhile isSpace xs)

solveByChar :: Char -> [Int] -> Int
solveByChar '+' = sum
solveByChar '*' = product
