main :: IO()
main = do
  m <- readLn :: IO Int
  numbers <- Prelude.map read . words <$> getLine :: IO [Int]
  print (increasing 0 numbers)

increasing :: Int -> [Int] -> Int
increasing n [m] = n
increasing n m = do
  let [a, b] = take 2 m
  increasing ((if a > b then a - b else 0) + n) ((max a b):(tail.tail) m)
