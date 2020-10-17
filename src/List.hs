module List(
  List(..),
  length', nth', reverse', concat', dropth',
  filter', map', flatMap',
  rle', duplicateEach', rotate'
) where
import System.Random

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show xs = "[" ++ toStr xs "" ++ "]" where 
    toStr Nil acc           = acc
    toStr (Cons y Nil) acc  = acc ++ show y
    toStr (Cons y ys)  acc  = acc ++ show y ++ ", " ++ toStr ys acc
    
length'             :: List a -> Int
length' Nil         = 0
length' (Cons _ xs) = 1 + length' xs

prepend'      :: List a -> a -> List a
prepend' xs x = Cons x xs

append'       :: List a -> a -> List a
append' Nil x = Cons x Nil
append' xs x  = reverse' (prepend' (reverse' xs) x)

nth'                  :: List a -> Int -> a 
nth' Nil _            = error "No such element"
nth' _ idx | idx < 0  = error "Incorrect index"
nth' (Cons x _) 0     = x
nth' (Cons _ xs) n    = nth' xs (n - 1)

reverse'    :: List a -> List a
reverse' xs = reverseRec xs Nil where
  reverseRec Nil ax         = ax
  reverseRec (Cons y ys) ax = reverseRec ys (Cons y ax)

concat'     :: List a -> List a -> List a
concat' xs  = concatRec (reverse' xs) where
  concatRec Nil ax          = ax
  concatRec (Cons z zs) ax  = concatRec zs (prepend' ax z)

dropth'       :: List a -> Int -> List a
dropth' xs n  = dropthRec xs n Nil where
  dropthRec Nil _ _           = error "Incorrect index"
  dropthRec _ idx _ | idx < 0 = error "Negative index"
  dropthRec (Cons _ rs) 0 as  = concat' (reverse' as) rs
  dropthRec (Cons r rs) m as  = dropthRec rs (m - 1) (prepend' as r)
  
filter'       :: List a -> (a -> Bool) -> List a
filter' xs f  = filterRec xs f Nil where
  filterRec Nil _ as          = reverse' as
  filterRec (Cons y ys) g as  = filterRec ys g (if g y then prepend' as y else as)
  
map'      :: List a -> (a -> b) -> List b
map' xs f = mapRec xs f Nil where
  mapRec Nil _ as         = reverse' as
  mapRec (Cons y ys) g as = mapRec ys g (prepend' as (f y))
  
flatMap'      :: List a -> (a -> List b) -> List b
flatMap' xs f = flatMapRec xs f Nil where
  flatMapRec Nil _ as         = as
  flatMapRec (Cons y ys) g as = flatMapRec ys g (concat' as (g y))
  
rle'              :: Eq a => List a -> List (a, Int)
rle' Nil          = Nil
rle' (Cons x xs)  = rleRec xs (x, 1) Nil where
  rleRec Nil (z, n) as          = reverse' (prepend' as (z, n))
  rleRec (Cons y ys) (z, n) as
    | y == z                    = rleRec ys (z, n + 1) as
    | otherwise                 = rleRec ys (y, 1) (prepend' as (z, n))
    
duplicateEach'                :: List a -> Int -> List a
duplicateEach' _ n | n <= 0   = error "Incorrect count"
duplicateEach' Nil _          = Nil
duplicateEach' xs n           = flatMap' xs (\x -> duplicateOne x n Nil) where
  duplicateOne _ 0 as         = as
  duplicateOne x m as         = duplicateOne x (m - 1) (prepend' as x)
  
rotate'               :: List a -> Int -> List a            
rotate' _ n | n < 0   = error "Incorrect count" 
rotate' Nil _         = Nil
rotate' xs n          = rotateRec xs (n `mod` length' xs) Nil where
  rotateRec ys 0 as           = concat' ys (reverse' as)
  rotateRec (Cons y ys) m as  = rotateRec ys (m - 1) (prepend' as y)