-- Task 1
even' :: Integral a => a -> Bool
even' n =
    mod n 2 == 0

-- Task 2
factorial :: (Ord p, Num p) => p -> p
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

-- Task 3
pow :: (Eq t, Num t, Num p) => p -> t -> p
pow x n =
    if n == 0 then 1
    else
        x * pow x (n - 1)

-- Task 4
fastPow x n =
    if n == 0 then 1
    else
        if even' n then fastPow x (n `div` 2) ^ 2
        else x * fastPow x (n `div` 2) ^ 2

-- Task 5
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Task 6
isPrime n =
    if n <= 1 then False
    else
        mod (factorial (n - 1)) n == n - 1

-- Task 6.1
isPrime2 n =
    if n == 1 then False
    else
        and [n `mod` x /= 0 | x <- [2 .. n - 1] ]

-- Task 7
gcd' a b =
    if b == 0 then a
    else gcd' b (a `mod` b)

