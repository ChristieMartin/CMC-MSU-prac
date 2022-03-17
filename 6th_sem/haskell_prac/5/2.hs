pythTriples :: [(Int, Int, Int)]
pythTriples = [(x,y,z) | z <- [1..], y <- [1..z], x <- [1..y] , x^2 + y^2 == z^2] 

primes :: [Int]
primes = [x | x <- [2..], length ([y | y <-[1..x], x `mod` y == 0]) == 2]

main :: IO ()
main =  do
print(take 10 primes)
print(take 5 pythTriples)