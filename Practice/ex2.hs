import Data.Char



transpose ([]: _) = []
transpose m =
    map head m : transpose (map tail m)


unionLists list1 list2 =
    list1 ++ [ x | x <- list2, not (x `elem` list1)]

intersectLists list1 list2 =
    [ x | x <- list1, y <- list2, x == y]

intToString 0 = []
intToString n =
    intToString (n `div` 10) ++ [chr (n `rem` 10 + 48)]

len [] = 0
len (h : t) = 
    1 + len t

intsFromString (e: []) = [ord(e) - 48]
intsFromString (h : t) =
    [10  ^ len(t) * (ord(h) - 48)] ++ intsFromString t

intFromString list =
    sum (intsFromString list)
