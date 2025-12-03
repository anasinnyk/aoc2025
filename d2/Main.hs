main :: IO ()
main = do
  putStrLn "AOC 2025 #2"

input :: FilePath -> IO String
input = readFile

parse :: String -> [(Int,Int)]
parse str = map (\(x:y:_) -> (read x, read y)) $ map (splitOn '-') $ splitOn ',' str

splitOn :: Char -> String -> [String]
splitOn d s = splitOnAcc d s []
  where
    splitOnAcc :: Char -> String -> [String] -> [String]
    splitOnAcc _ []     acc             = reverse $ map reverse acc
    splitOnAcc d (s:ss) acc | s == d    = splitOnAcc d ss ("":acc)
                            | otherwise = splitOnAcc d ss ((s:(if length acc > 0 then (head acc) else [])):drop 1 acc)

invalidIDs :: [Int] -> [(String, Bool)]
invalidIDs []     = []
invalidIDs (i:is) = check : invalidIDs is
  where
    cid = show i
    lengthCid = length cid
    check | odd lengthCid = (cid, False)
          | otherwise     = (cid, take (div lengthCid 2) cid == drop (div lengthCid 2) cid)

chancksOf :: Int -> String -> [String]
chancksOf _ [] = []
chancksOf c s = take c s : chancksOf c (drop c s)

invalidID :: String -> Bool
invalidID s = let
    tries = length s `div` 2
    isAllEq :: [String] -> Bool
    isAllEq (s:ss) = all (== s) ss
    checkAllTries :: Int -> [Bool] -> Bool
    checkAllTries 0 res = any id res
    checkAllTries t res = checkAllTries (t-1) ((isAllEq $ chancksOf t s):res)
  in
    checkAllTries tries []

testSolution :: FilePath -> IO ()
testSolution f = do
  inp <- input f
  putStrLn $ show $ sum $ map (sum . map (read . fst) . filter (\(_,i) -> id i) . invalidIDs) $ map (\(s,e) -> [s..e]) $ parse inp

testSolution' :: FilePath -> IO ()
testSolution' f = do
  inp <- input f
  putStrLn $ show $ sum $ map (sum . map fst . filter (\(_,i) -> id i) . map (\x -> (x, invalidID $ show x))) $ map (\(s,e) -> [s..e]) $ parse inp
