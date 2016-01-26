module Codewars.Kata.Collatz where

import           Control.Monad             (return, (>>))
import           Control.Monad.Trans.State (State, execState, modify)

f :: Integer -> [Integer]
f 1 = [1]
f n | odd n  = n : f (3 * n + 1)
    | otherwise = n : f (div n 2)

collatz :: Integer -> Int
collatz = length  . f

collatz' :: Integer -> Int
collatz' n = let doC :: Integer -> State Int Integer
                 doC 1 = return 1
                 doC k = modify succ >> doC nk
                        where nk = if k `mod` 2 == 0 then k `div` 2 else k * 3 + 1
             in execState (doC n) 1

collatz'' :: Integer -> Int
collatz'' = succ . length . takeWhile (/= 1) . iterate collatz'
  where collatz' n = if even n then div n 2 else succ $ n * 3


collatz''' :: Integer -> Int
collatz''' n
  | n == 1 = 1
  | n `mod` 2 == 0 = 1 + collatz (quot n 2 )
  | otherwise = 1 + collatz (n*3 + 1)
