import Data.List

collatz :: [Int] -> [Int]
collatz c = do
  let h = head c
  if h == 1 then c else collatz ((if even h then div h 2 else 1 + 3 * h):c)

main :: IO()
main = do
  line <- readLn
  putStrLn (intercalate " " [show r | r <- (reverse.collatz) [line]])
