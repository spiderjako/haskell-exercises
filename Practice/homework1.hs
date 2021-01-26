even' n =
    mod n 2 == 0

factorial n =
    if n <= 1 then 1
    else
        n * factorial (n - 1)

pow x n =
    if n == 1 then x
    else
        x * pow x (n - 1)

-- fast_pow x n =
--     if n == 1 then x
--     else
--         if mod n 2 == 0 then fast_pow (fast_pow x n/2) 2
--         else x * fast_pow x (n - 1)

fib n =
    if n == 1 then 1
    else
        if n < 1 then 0
        else
            fib (n - 1) + fib (n - 2)

fib2 n a b =
    if n == 0 then a
    else
        if n == 1 then b
        else
            fib2 (n - 1) b (a + b)


is_prime_helper n a =
    if a <= 1 then True
    else
        is_prime_helper n a - 1
    

is_prime_helper n =
    is_prime_helper n n - 1
