import Data.List
import Data.Function

knight :: Integer -> Integer
knight n = div ((n^4) - (9 * n^2) + (n * 24) - 16) 2

main :: IO ()
main = do
  a <- readLn :: IO Integer
  putStrLn $ intercalate " " $ map show $ map knight [1..a]
