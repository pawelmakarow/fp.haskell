module Main where

import List

main :: IO ()
main = print (flatMap' list f) where
  list = Cons (Cons 'a' (Cons 'b' (Cons 'c' Nil))) (Cons (Cons '1' (Cons '2' (Cons '3' Nil))) Nil)
  f :: List Char -> List String 
  f Nil = Nil
  f (Cons x xs) = Cons [x] (f xs)
