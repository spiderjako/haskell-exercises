generateSolutions result arr length left index 
  | index == length - 1 = [(changeIthElement arr index left)]
  | left == 0 = [arr]
  | otherwise = 
        let ithElement = arr !! index
        in  result ++
            (concat [ 
                        generateSolutions [] (changeIthElement arr index x) length (left - x) (index + 1) 
                        | x  <- [ithElement .
                        . left]
                    ]
            )

changeIthElement arr index withValue = 
    (take index arr) ++ [withValue] ++ (drop (index + 1) arr)


generate 0 m = []
generate n m = generateSolutions [] [0 | x <- [1.. n]] n m 0
