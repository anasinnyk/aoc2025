import Data.Char (digitToInt)

main :: IO ()
main = do
  putStrLn "AOC 2025 #3"

input :: FilePath -> IO String
input = readFile

parse :: String -> [[Int]]
parse f = map (map digitToInt) $ lines f

findMaxAndSecondMax :: (Int, Int) -> [Int] -> (Int, Int)
findMaxAndSecondMax r      []     = r
findMaxAndSecondMax (m,sm) (x:[]) = (m, if x > sm then x else sm)
findMaxAndSecondMax (m,sm) (s:ss) = (if s > m
                                    then findMaxAndSecondMax (s, 0)
                                    else if s > sm
                                    then findMaxAndSecondMax (m, s)
                                    else findMaxAndSecondMax (m, sm))
                                    ss

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  putStrLn $ show $ sum $ map ((\(x,y) -> x * 10 + y) . findMaxAndSecondMax (0,0)) $ parse inp

