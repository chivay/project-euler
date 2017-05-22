module Euler21to30 where
import Data.List
import Data.Array
import Data.Char
import Utils

problem21 = sum [x + y | x <- [2..9999], let y = divSum ! x, areValid x y]
    where divSum = array (1,9999) [ (x, sum (divisors x)) | x <- [1..9999]]
          divisors x = [d | d <- [1..x `div` 2], x `mod` d == 0]
          areValid x y = y < 10000 && x < y && divSum ! y == x

problem22 = do
        text <- readFile "p022_names.txt"
        let list = sort $ (read :: String -> [String] ) $ "[" ++ text ++ "]"
        return $ sum (zipWith score list [1..])
    where score name i = i * sum (map (\ c -> ord c - ord 'A' + 1) name)