import Data.List

phi :: (b -> c -> d) -> (a -> x -> b) -> (a -> x -> c) -> a -> x -> d
phi h f g x y = h (f x y) (g x y)

as :: (a -> a -> b) -> [a] -> b
as f xy = do
  let [x, y] = xy
  f x y

coins :: Int -> Int -> Bool
coins = phi (&&) (\x y -> 0 == mod (x + y) 3) $ phi (phi (>=) (\_ y->y) (-)) max min

main :: IO ()
main = do
  _ <- readLn :: IO Integer
  interact $ \l -> intercalate "\n"
           $ map (\s -> if s then "YES" else "NO")
           $ map (\xy -> as coins $ map read xy)
           $ map words
           $ lines l
