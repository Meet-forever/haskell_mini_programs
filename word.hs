import Data.Char(ord, toLower)

wordValue :: String -> Int
wordValue "" = 0
wordValue (c:xc) = (ord (toLower c) - 96)  + wordValue xc