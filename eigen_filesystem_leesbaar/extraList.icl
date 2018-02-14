implementation module extraList

import	StdClass, StdMisc, StdEnum, StdInt, StdChar, StdBool, StdArray, StdString, StdReal, StdTuple
import StdList

index :: a [a] -> Int | == a
index a l = index` a l 0
    where
        index` a [] i = -1
        index` a [x:y] i
        | a == x = i
        | otherwise = index` a y (i+1)

//j is the length
subset :: Int Int [a] -> [a]
subset i j a = snd (splitAt i (fst (splitAt (j+i) a)))

before :: Int [a] -> [a]
before i a = fst (splitAt i a)

//also contains the object at location i
after :: Int [a] -> [a]
after i a = snd (splitAt i a)

//adds the interspersed element also as head
intersperse :: a [a] -> [a]
intersperse el [] = []
intersperse el l = foldr (\x y.[el,x: y]) [] l

//given two lists and a comparator, gives (els only in list 1, els in both lists, els only in list 2)
compare :: [a] [b] (a b -> Bool) -> ([a],[a],[b])
compare l1 l2 pred
# inl1 = filter (\x1. not (or (map (\x2. pred x1 x2) l2))) l1
# inl2 = filter (\x2. not (or (map (\x1. pred x1 x2) l1))) l2
# inboth = filter (\x1. or (map (\x2. pred x1 x2) l2)) l1
= (inl1,inboth,inl2)
