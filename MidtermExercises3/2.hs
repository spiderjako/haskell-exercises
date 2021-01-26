import Data.List

map' :: (Integral a) => (a -> a) -> [a] -> [a]
map' f lst
  | null lst = []
  | otherwise = (f (head lst)) : (map' f (tail lst))

filter' :: (Integral a) => (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred lst
  | pred (head lst) = (head lst) : filter' pred (tail lst)
  | otherwise = filter' pred (tail lst)

reverse' :: (Integral a) => [a] -> [a]
reverse' lst
  | null (tail lst) = [(head lst)]
  | otherwise = (reverse' (tail lst)) ++ [(head lst)]

length' :: (Integral a) => [a] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

null' :: [a] -> Bool
null' [] = True
null' _ = False

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t)
  | h == x = True
  | otherwise = elem' x t

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (h:t) = h : (take' (n-1) t)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 lst = lst
drop' n (_:t) = drop' (n-1) t

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f lst1 lst2 = (f (head lst1) (head lst2)) : zipWith' f (tail lst1) (tail lst2)

zip' :: [a] -> [b] -> [(a,b)]
zip' lst1 lst2 = zipWith' (\a b -> (a, b)) lst1 lst2

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred lst
  | pred (head lst) = (head lst) : (takeWhile' pred (tail lst))
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' pred (h:t)
  | pred h = (dropWhile' pred t)
  | otherwise = (h:t)

minimum' :: (Ord a, Num a) => [a] -> a
minimum' [] = 0
minimum' lst = foldr min (head lst) lst

maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = 0
maximum' lst = foldr max (head lst) lst

reverse'' :: [a] -> [a]
reverse'' lst = foldl (\ res el -> el:res) [] lst

length'' :: [a] -> Int
length'' lst = foldl (\acc _ -> acc + 1) 0 lst

all' :: (a->Bool) -> [a] -> Bool
all' pred lst =
  foldr (\a acc -> pred a && acc) True lst

any' :: (a->Bool) -> [a] -> Bool
any' pred lst =
  foldr (\a acc -> pred a || acc) False lst

append' :: [a] -> [a] -> [a]
append' lst1 lst2 =
  foldr (\a acc -> a:acc) lst2 lst1

replicate' :: Int -> a -> [a]
replicate' n x =
  foldr (\_ res -> x:res) [] [1..n]

divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], (x `mod` y == 0)]

countDivisors :: Int-> Int
countDivisors x = length $ divisors x

isPrime :: Int -> Bool
isPrime x = length (divisors x) == 2

descartes :: [a] -> [a] -> [(a,a)]
descartes lst1 lst2 = [ (x,y) | x <- lst1, y <- lst2]

primes :: [Int]
primes = [ x | x<- [1..], isPrime x]

sieve :: [Int] -> [Int]
sieve lst = (head lst) : sieve [y | y <- (tail lst), y `mod` (head lst) /= 0]

allDescartes :: [(Int, Int)]
allDescartes =[(d-y,y) | d<-[0..], y<-[0..d]]

pythagorean :: [(Int, Int, Int)]
pythagorean = [ (a,b,c) | c <- [1..], a <- [1..c], b <- [1..c], a^2 + b^2 == c^2 ]

compress :: (Eq a) => [a] -> [(a, Int)]
compress [] = []
compress lst =
  (head lst, length heads) : compress rest
  where (heads, rest) = span (== (head lst)) lst

maxRepeated :: (Eq a) => [a] -> Int
maxRepeated lst =
  maximum' (map snd (compress lst))

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet lst =
  (head lst) : makeSet (filter (/= (head lst)) (tail lst))

histogram :: (Eq a) => [a] -> [(a, Int)]
histogram [] = []
histogram lst =
  (head lst, count) : histogram (filter (/= (head lst)) (tail lst))
  where count = length (filter (== (head lst)) lst)

maxDistance :: [(Float, Float)] -> Float
maxDistance [] = 0
maxDistance lst =
  let x = fst (head lst)
      y = snd (head lst)
      distances = map (\a -> sqrt((x - (fst a))**2 + (y - snd a)**2)) (tail lst)
  in maximum' (distances ++ [maxDistance (tail lst)])

maxDistance'' :: [(Float, Float)] -> Float
maxDistance'' lst = maximum' [ sqrt ((fst a - fst b)**2 + (snd a - snd b)**2) | a <- lst, b <- lst ]

-- specialMaximum [] = 0
-- specialMaximum lst = foldr (\ a acc -> if snd a > snd acc then fst a else fst acc) (head lst) lst

specialMax :: (Ord a ) => (a, Int) -> (a, Int) -> (a, Int)
specialMax a b = if (snd a) > (snd b) then a else b

specialSort :: (Ord a) => [[a]] -> [a]
specialSort lst =
  let histograms = map histogram lst
      maxes = map (\x -> foldl (\acc a -> specialMax acc a) ((fst (head x)), 0) x) histograms
      maxFirsts = map fst maxes
  in sortBy compare maxFirsts
