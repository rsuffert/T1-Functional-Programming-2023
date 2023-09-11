import Control.Exception

-- Exception for invalid parameters
data InvalidParameterException = InvalidParameterException {message :: String} deriving (Show)
instance Exception InvalidParameterException

-- Exercicio 1
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (b:bs) | b==1      = b*2^(length bs) + bin2dec bs
               | b==0      = bin2dec bs
               | otherwise = throw (InvalidParameterException "Non-binary value (0 or 1) provided in the list")

-- Exercicio 2

-- Exercicio 3

-- Exercicio 4

-- Exercicio 5

-- Exercicio 6