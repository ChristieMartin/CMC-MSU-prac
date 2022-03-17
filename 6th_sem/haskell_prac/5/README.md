**Реализовать следующие функции для работы с бесконечными потоками:**  
*1.hs*  
--- Аналоги zip и zipWith  
zipStream :: Stream a -> Stream b -> Stream (a, b)  
zipStreamWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c  

 -- Бесконечный поток чисел Фибоначчи.  
fibStream :: Stream Int  

**Реализовать следующие функции, используя абстракцию списков (list comprehensions)**  
*2.hs*  
-- Пифагоровы тройки:  
pythTriples :: [(Int, Int, Int)]  
-- Простые числа:  
primes :: [Int]  
