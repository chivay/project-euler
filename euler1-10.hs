module Euler1to10 where
-- List of primes
primes = 2 : [ i | i <- [3..],
                    and [ mod i p  /= 0 | p <- takeWhile (\p -> p^2 <= i)  primes ] ]
-- List of Fibonacci numbers
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

-- List of factors of n
factor 1 = []
factor n = minDiv : factor (n `div` minDiv)
    where minDiv = head (dropWhile (\p -> mod n p > 0) primes)


-- SOLUTIONS

-- Sum of natural numbers below 1000 that are multiples of 3 or 5.
problem1 = sum [ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]

-- Sum of even valued fibonacci numbers below 4kk
problem2 = sum [ x | x <- takeWhile (<= 4000000) fibo, even x]

-- Largest prime factor of 600851475143
problem3 = last (factor 600851475143)

-- Largest palindrome made from the product of two 3-digit numbers
problem4 = foldl max 1 [ x*y | x <- [100..999], y <- [x..999], let s = show (x*y) in s == reverse s ]

-- Smallest positive number evenly divisible by all numbers from 1 to 20
problem5 = foldl lcm 1 [1..20]
    where lcm x y = (x * y) `div` (gcd x y)

-- Difference between sum of first 100 squares and square of the sum
problem6 = sum (map (\x -> x^2) [1..100]) - (sum [1..100])^2
                                                                   --
-- 10 001st prime
problem7 = primes !! 10000

-- Maximum product of 13 consecutive digits in given number
problem8 =  maximum [ foldl (*) 1 $ map (read::String->Int) (map (: []) (take 13 (drop i nStr)))  | i <- [0..(length nStr)]]
    where nStr = show n
          n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

-- Pythagorean triplet with sum of numbers = 1000
problem9 = head [ a*b*c | c <- [1..1000],
                          b <- [1..c],
                          let a = 1000 - b - c,
                          a^2 + b^2 == c^2,
                          a > 0]


-- Sum of primes < 2kk
problem10 = sum (takeWhile (<=2*10^6) primes)
