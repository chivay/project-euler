module Utils where

-- List of primes
primes = 2 : [ i | i <- [3..],
                    and [ mod i p  /= 0 | p <- takeWhile (\p -> p^2 <= i)  primes ] ]
-- List of Fibonacci numbers
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

-- List of factors of n
factor 1 = []
factor n = minDiv : factor (n `div` minDiv)
    where minDiv = head (dropWhile (\p -> mod n p > 0) primes)
