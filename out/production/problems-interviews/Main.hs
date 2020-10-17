module Main where

import List

main :: IO ()
main = print (rotate' list 3) where
--  list = Cons 'a' (Cons 'b' (Cons 'a' (Cons 'a' (Cons 'a' (Cons 'b' (Cons 'b' (Cons 'b' (Cons 'a' Nil))))))))
--  list = Cons '1' (Cons '1' (Cons '1' (Cons '2' (Cons '2' (Cons '3' (Cons '4' (Cons '4' (Cons '4' (Cons '4' (Cons '5' Nil))))))))))
--  list = Cons 'a' (Cons 'b' (Cons 'c' Nil))
  list = Cons '1' (Cons '2' (Cons '3' (Cons '4' (Cons '5' (Cons '6' (Cons '7' (Cons '8' (Cons '9' Nil))))))))