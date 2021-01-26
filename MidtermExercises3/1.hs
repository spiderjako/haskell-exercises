fibonacci :: Int -> Int
fibonacci n
  | n <= 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

fib :: Int -> Int
fib n = case n of 0 -> 0
                  1 -> 1
                  _ -> fib (n-1) + fib (n-2)

feelbonacci :: Int -> Int
feelbonacci 0 = 0
feelbonacci 1 = 1
feelbonacci n = feelbonacci (n - 1) + feelbonacci (n - 2)

fastPow :: Int -> Int -> Int
fastPow _ 0 = 1
fastPow x 1 = x
fastPow x n
  | even n = half * half
  | otherwise = half * half * x
  where half = fastPow x (n `div` 2)

compl :: (Integral a) => (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
compl op x y = (op (fst x) (fst y), op (snd x) (snd y))

complAdd :: (Integral a) => (a, a) -> (a, a) -> (a, a)
complAdd x y = compl (+) x y

complSub :: (Integral a) => (a, a) -> (a, a) -> (a, a)
complSub x y = compl (-) x y

complMul :: (Integral a) => (a, a) -> (a, a) -> (a, a)
complMul x y = compl (*) x y

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance x y =
  sqrt $ a ^ 2 + b ^ 2
  where a = fst y - fst x
        b = snd y - snd x

repeated :: (Integral a) => (a -> a) -> a -> a -> a
repeated f n
  | n <= 0 = \x -> x
  | otherwise = \x -> f ((repeated f (n - 1)) x)
