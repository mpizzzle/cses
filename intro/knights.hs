import Data.List

knight :: Integer -> Integer
knight n = do
 let q = n * n
 (div (q * (q - 1)) 2) - 4 * (n - 1) * (n - 2)

main :: IO ()
main = do
  a <- readLn :: IO Integer
  putStrLn $ intercalate " " $ map show $ map knight [1..a]
