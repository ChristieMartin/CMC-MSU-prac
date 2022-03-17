data Stream a = StreamC a (Stream a) deriving Show

ones :: Stream Int
ones = StreamC 1 ones

streamFrom :: Float -> Stream Float
streamFrom n = StreamC n (streamFrom (n + 1))

takeStream :: Int -> Stream a -> [a]
takeStream n (StreamC x xs)
  | n > 0 = x : (takeStream (n - 1) xs)
  | otherwise = []
  
zipStream :: Stream a -> Stream b -> Stream (a, b)
zipStream (StreamC x xs) (StreamC y ys) = StreamC ((x, y)) (zipStream xs ys)

zipStreamWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreamWith f (StreamC x xs) (StreamC y ys) = StreamC (f x y) (zipStreamWith f xs ys)

fibStream :: Stream Int
fibStream = StreamC 0 (StreamC 1 ( zipStreamWith (+) fibStream (tailStream fibStream)))

tailStream :: Stream a -> Stream a
tailStream (StreamC x xs) = xs

main :: IO ()
main =  do
print(takeStream 5 (streamFrom 5))
print(takeStream 5 (streamFrom 10))
print(takeStream 5 (zipStream (streamFrom 5) (streamFrom 10)))
print(takeStream 5 (zipStreamWith (+) (streamFrom 5) (streamFrom 10)))
print(takeStream 10 fibStream)