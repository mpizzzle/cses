import Data.List

collatz :: (Integral a) => [a] -> [a]
collatz c = do
  let h = head c
  if h == 1 then c else collatz ((if 0 == mod h 2 then div h 2 else 1 + 3 * h):c)

main :: IO()
main = do
  line <- readLn
  putStrLn (intercalate " " [show r | r <- (reverse.collatz) [line]])
