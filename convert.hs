convertNum2Binary :: Int -> String
convertNum2Binary x
    | x < 0 = "Only non-negative integers!!"
    | x == 0 = "0"
    | x == 1 = "1"
    | otherwise = (convertNum2Binary (x `div` 2)) ++ if (x `mod` 2 == 0) then "0" else "1"
    

convertFraction2Binary :: Float -> String
convertFraction2Binary x =
    let limit = 23
        cB :: Float -> Int-> String
        cB x count
            | x == 0.0 = ""
            | x > 0  = (if(floor(x*2) > 0) then "1" else "0")  ++ if(count < limit) then (cB (if(floor(x*2) > 0) then ((x*2) - (fromIntegral (floor(x*2)))) else (x*2)) (count+1)) else "" 
    in "." ++ cB x 1