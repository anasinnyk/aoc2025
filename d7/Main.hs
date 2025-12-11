import Data.Array as A (Array, array, assocs, bounds, listArray, range, (!), (//))

main :: IO ()
main = do
  putStrLn "AOC 2025 #6"

input :: FilePath -> IO String
input = readFile

techyonBeam :: Char
techyonBeam = '|'

emptySpace :: Char
emptySpace = '.'

entersBeam :: Char
entersBeam = 'S'

splitter :: Char
splitter = '^'

parse :: String -> Array (Int, Int) Char
parse s = listArray ((0, 0), (h - 1, w - 1)) (concat ls)
 where
  ls = lines s
  h = length ls
  w = length (head ls)

walkMatrix :: Array (Int, Int) Char -> Array (Int, Int) Char
walkMatrix m =
  let
    (_, (rowSize, colSize)) = bounds m

    res :: Array (Int, Int) Char -> (Int, Int) -> Array (Int, Int) Char
    res m' (x, _) | x == rowSize = m'
    res m' (x, y) | y == colSize = res m' (x + 1, 0)
    res m' i@(x, y)
      | m' ! i == entersBeam =
          res (m' // [((x + 1, y), techyonBeam)]) (x + 1, y)
      | m' ! i == techyonBeam && x < (rowSize - 1) && m' ! (x + 1, y) /= splitter =
          res (m' // [((x + 1, y), techyonBeam)]) (x, y + 1)
      | m' ! i == techyonBeam && x < (rowSize - 1) =
          res (m' // [((x + 1, y - 1), techyonBeam)] // [((x + 1, y + 1), techyonBeam)]) (x, y + 1)
      | otherwise = res m' (x, y + 1)
   in
    res m (0, 0)

solve :: FilePath -> IO ()
solve f = do
  inp <- input f
  print $ countRes $ walkMatrix $ parse inp

countRes :: Array (Int, Int) Char -> Int
countRes m =
  let
    (_, (rowSize, colSize)) = bounds m

    res :: Array (Int, Int) Char -> (Int, Int) -> Int -> Int
    res m' (x, _) r | x == rowSize = r
    res m' (x, y) r | y == colSize = res m' (x + 1, 0) r
    res m' (x, y) r
      | m' ! (x, y) == splitter && m' ! (x - 1, y) == techyonBeam = res m' (x, y + 1) (r + 1)
      | otherwise = res m' (x, y + 1) r
   in
    res m (0, 0) 0

countRes' :: Array (Int, Int) Char -> Int
countRes' m =
  let
    ((minR, minC), (maxR, maxC)) = bounds m
    (idx, _) = head $ Prelude.filter (\(_, r) -> r == entersBeam) $ assocs m

    memo :: A.Array (Int, Int) Int
    memo =
      A.array
        (bounds m)
        [(i, fpd i) | i <- A.range (bounds m)]

    fpd :: (Int, Int) -> Int
    fpd i@(x, y)
      | x == maxR = 1
      | m ! i == entersBeam = memo ! (x + 1, y)
      | m ! i == splitter = memo ! (x + 1, y - 1) + memo ! (x + 1, y + 1)
      | m ! i == techyonBeam = if x < maxR then memo ! (x + 1, y) else 1
      | otherwise = 0
   in
    fpd idx

solve' :: FilePath -> IO ()
solve' f = do
  inp <- input f
  print $ countRes' $ walkMatrix $ parse inp
