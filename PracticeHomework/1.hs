upperLine :: Int -> String
upperLine n =
    "┌" ++ foldl (++) "" (take n (repeat "─")) ++ "┐"

lowerLine :: Int -> String
lowerLine n =
    "└" ++ foldl (++) "" (take n (repeat "─")) ++ "┘"

leftWalls :: Int -> String
leftWalls n =
    foldl (++) "" (take n (repeat "│ "))

rightWalls :: Int -> String
rightWalls n =
    foldl (++) "" (take n (repeat " │"))

upperFormula :: Int -> Int -> Int
upperFormula n x = ((2 * (n * 2) - 2) + 3 - (4 * x))

lowerFormula :: Int -> Int -> Int
lowerFormula n x = ((2 * (n * 2) - 2) + 3 - (4* (n - x + 1)))

upperHelper :: Int -> Int -> String
upperHelper n x
    | n < x = ""
    | otherwise = (leftWalls (x - 1) ++ upperLine (upperFormula n x) ++ rightWalls (x - 1)) ++ "\n" ++ upperHelper n (x + 1)    

lowerHelper :: Int -> Int -> String
lowerHelper n x
    | n < x = ""
    | otherwise = (leftWalls (n - x) ++ lowerLine (lowerFormula n x) ++ rightWalls (n - x)) ++ "\n" ++ lowerHelper n (x + 1)

squares :: Int -> IO ()
squares n =
    putStr (upperHelper n 1 ++ lowerHelper n 1)
