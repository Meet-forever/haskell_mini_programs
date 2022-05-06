import Data.List (sort)

-- O(n)
extractDigits :: Int -> [Int]
extractDigits x 
    | x `div` 10 /= 0 =  extractDigits(x `div` 10) ++ [x `mod` 10]
    | otherwise = [x]
-- extractDigits x = [] ++ if (x `div` 10 /= 0) then extractDigits (x `div` 10) ++ [x `mod` 10] else [x]


-- O(n log n) by sorting
noRepeatingDigits :: Int -> Bool
noRepeatingDigits x = check (sort (extractDigits x)) 
    where
        check [] = True
        check (x:xs)
            | xs /= [] && x == (head xs) = False
            | otherwise = check xs

-- Alternative
    -- let check [] = True
        -- check (x:xs)
        --     | xs /= [] && x == (head xs) = False
        --     | otherwise = check xs
        -- in
        -- check (sort (extractDigits x))

-- O(1)
numLen :: Int -> Int
numLen x = ceiling ( logBase (fromIntegral 10) (fromIntegral (x+1)) )

-- O(n log n)
numBulls :: Int -> Int -> Int
numBulls x y
    | x < 0 || y < 0 = -1
    | (numLen x == numLen y) && noRepeatingDigits x && noRepeatingDigits y = matchNum x y    
    | otherwise = -1

-- O(n)
matchNum :: Int -> Int -> Int
matchNum x y
    | (x `div` 10 == 0) && (y `div` 10 == 0) = if x == y then 1 else 0
    | (x `mod` 10 == y `mod` 10) = 1 + matchNum (x `div` 10) (y `div` 10)
    | (x `mod` 10 /= y `mod` 10) = matchNum (x `div` 10) (y `div` 10)

-- Alternative
-- nB x y = 
--     let check x y =
--             if (numLen x == numLen y) && noRepeatingDigits x && noRepeatingDigits y then
--                 let loop m n = if (m `div` 10 == 0) then if(m `mod` 10 == n `mod` 10) then 1 else 0  
--                         else if(m `mod` 10 == n `mod` 10) then (1 + loop (m `div` 10) (n `div` 10)) else loop (m `div` 10) (n `div` 10)
--                     in loop x y
--             else -1
--         in check x y


-- O(n^2)
numCows :: Int -> Int -> Int
numCows x y
    | x < 0 || y < 0 = -1
    | (numLen x == numLen y) && noRepeatingDigits x && noRepeatingDigits y = totalMatch (extractDigits x) (extractDigits y) - matchNum x y
    | otherwise = -1

-- O(n^2)
totalMatch :: [Int] -> [Int] -> Int 
totalMatch (x:xs) y
    | xs == [] = if (x `elem` y) then 1 else 0
    | (x `elem` y) = 1 + totalMatch xs y
    | otherwise = totalMatch xs y