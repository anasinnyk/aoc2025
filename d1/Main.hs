main :: IO ()
main = do
  putStrLn "AOC 2025 #1"

data Action = L Int | R Int

input :: FilePath -> IO String
input = readFile

parse :: String -> [Action]
parse str = map lineToAction (lines str)
 where
  lineToAction :: String -> Action
  lineToAction (c : cs) | c == 'L' = L $ read cs
  lineToAction (c : cs) | c == 'R' = R $ read cs
  lineToAction _ = error "Parse error"

findThePoint :: Action -> Int -> (Int, Int)
findThePoint (L x) p = (mod (p - x) 100, div x 100 + (if p <= (mod x 100) && p /= 0 then 1 else 0))
findThePoint (R x) p = (mod (p + x) 100, div (p + x) 100)

findTheCode :: Int -> [Action] -> [(Int, Int)]
findTheCode _ [] = []
findTheCode p (a : as) = (findThePoint a p) : findTheCode (fst $ findThePoint a p) as

testFindTheCode :: FilePath -> IO ()
testFindTheCode f = do
  inp <- input f
  putStrLn $ show $ length $ filter (0 ==) $ map fst $ findTheCode 50 $ parse inp

testFindTheCode' :: FilePath -> IO ()
testFindTheCode' f = do
  inp <- input f
  putStrLn $ show $ sum $ map snd $ findTheCode 50 $ parse inp
