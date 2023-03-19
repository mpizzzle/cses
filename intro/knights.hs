import Data.List
import Data.Function

knight :: Integer -> Integer
knight n = do
  let q = n * n
  let m = n - 4
  (on (-) $ \x -> div x 2) (q * (q - 1)) (sum [(8 * m * m), (40 * m), 48])

main :: IO ()
main = do
  a <- readLn :: IO Integer
  putStrLn $ intercalate " " $ map show $ map knight [1..a]
