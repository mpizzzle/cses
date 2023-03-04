next :: (Integral a) => a -> a
next n = if 0 == mod n 2 then div n 2 else 1 + 3 * n

collatz :: (Integral a) => [a] -> [a]
collatz n = do
  let h = head n
  if (h == 1) then n else collatz (next h:n)

parse :: String -> Int
parse s = read s

main :: IO()
main = do
  line <- getLine
  let result = collatz [parse line]
  let format = foldl (\ x y -> y ++ " " ++ x) []
  (putStrLn.format) [show r | r <- result]
