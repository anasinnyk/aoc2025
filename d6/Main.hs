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
    ops = filter (/= ' ') $ last ls
   in
    zip digits ops

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ sum $ map (\(is, c) -> solveByChar c is) $ parse inp

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
