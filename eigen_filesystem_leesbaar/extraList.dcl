definition module extraList

import StdList

index :: a [a] -> Int | == a

//j is the length
subset :: Int Int [a] -> [a]

before :: Int [a] -> [a]

//also contains the object at location i
after :: Int [a] -> [a]

//adds the interspersed element also as head
intersperse :: a [a] -> [a]

//given two lists and a comparator, gives (els only in list 1, els in both lists, els only in list 2)
compare :: [a] [b] (a b -> Bool) -> ([a],[a],[b])
