import Data.List

perm :: Int -> [Int]
perm 1 = [1]
perm 4 = [2,4,1,3]
perm n = [x | x <- [5..n], odd x] ++ [2,4,1,3] ++ [x | x <- [5..n], even x]

main :: IO()
main = do
  n <- readLn :: IO Int
  putStrLn (if not (n == 3 || n == 2)
            then (intercalate " " [show r | r <- perm n])
            else "NO SOLUTION")
