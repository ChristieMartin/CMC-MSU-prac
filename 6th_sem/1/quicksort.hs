qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

main :: IO ()
main = print(qsort [1000,993,986,979])
