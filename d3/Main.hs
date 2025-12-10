import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = do
  putStrLn "AOC 2025 #3"

input :: FilePath -> IO String
input = readFile

parse :: String -> [[Int]]
parse f = map (map digitToInt) $ lines f

findMaxAndSecondMax :: (Int, Int) -> [Int] -> (Int, Int)
findMaxAndSecondMax r [] = r
findMaxAndSecondMax (m, sm) [x] = (m, max x sm)
findMaxAndSecondMax (m, sm) (s : ss) =
  ( if s > m
      then findMaxAndSecondMax (s, 0)
      else
        if s > sm
          then findMaxAndSecondMax (m, s)
          else findMaxAndSecondMax (m, sm)
  )
    ss

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ sum $ map ((\(x, y) -> x * 10 + y) . findMaxAndSecondMax (0, 0)) $ parse inp

size = 12

findLastMax :: [Int] -> (Int, [Int], [Int]) -> (Int, [Int], [Int])
findLastMax [] r = r
findLastMax (x : xs) (y, ys, yss) = findLastMax xs (if x >= y then (x, yss ++ (y : ys), []) else (y, ys, x : yss))

findXLastMaxs :: Int -> [Int] -> [Int]
findXLastMaxs 0 r = []
findXLastMaxs x xs
  | x >= length xs = xs
  | otherwise =
      let
        rxs = reverse xs
        checkMaxOn = drop (x - 1) rxs
        protectTail = reverse $ take (x - 1) rxs
        (i, j, _) = findLastMax (tail checkMaxOn) (head checkMaxOn, [], [])
       in
        i : findXLastMaxs (x - 1) (j ++ protectTail)

solve' :: FilePath -> IO ()
solve' f = do
  inp <- input f
  print $ sum $ map (read . map intToDigit . findXLastMaxs size) $ parse inp
