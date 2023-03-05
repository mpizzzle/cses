main :: IO()
main = do
  m <- readLn :: IO Int
  numbers <- Prelude.map read . words <$> getLine :: IO [Int]
  print ((div (m * (m + 1)) 2) - (sum numbers))
