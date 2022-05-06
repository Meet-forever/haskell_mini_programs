singleDigit :: Int -> Int
singleDigit x = if x > 9 then singleDigit (x `mod` 10 + x `div` 10) else x