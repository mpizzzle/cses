import Data.List
import Data.Function

phi :: (b -> c -> d) -> (a -> x -> b) -> (a -> x -> c) -> a -> x -> d
phi h f g y x = h (f y x) (g y x)

b1 :: (b -> c) -> (a -> x -> b) -> a -> x -> c
b1 f g y x = f (g y x)

as :: (a -> a -> a) -> [a] -> a
as f yx = do
  let [y, x] = yx
  f y x

spiral :: Integer -> Integer -> Integer
spiral = phi (+) (b1 (\m -> 1 + (m^2) - m) max) (phi (*) (b1 abs (-)) (phi (on (*) $ \x -> if x then 1 else -1) (>) (b1 even max)))

main :: IO ()
main = do
  _ <- readLn :: IO Integer
  interact $ \l -> intercalate "\n" $ map show $ map (\yx -> as spiral $ map read yx) $ map words $ lines l
