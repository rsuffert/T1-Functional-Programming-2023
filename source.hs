import Control.Exception

-- Exception for invalid parameters
data InvalidParameterException = InvalidParameterException {message :: String} deriving (Show)
instance Exception InvalidParameterException

-- Exercicio 1
-- example usage: 
-- ghci> bin2dec [1, 0, 1]
-- 5
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (b:bs) | b==1      = b*2^(length bs) + bin2dec bs
               | b==0      = bin2dec bs
               | otherwise = throw (InvalidParameterException "Non-binary value provided in the list")

-- Exercicio 2
-- example usage:
-- ghci> dec2bin 5 5
-- [0, 0, 1, 0, 1]
dec2bin :: Integral a => a -> a -> [a]
dec2bin decn size | decn==0   = fillZeros [0] (size-1)
                  | decn<0    = throw (InvalidParameterException "Function only takes positive integers as input")
                  | otherwise = fillZeros (dec2binHelper decn) size
                  where
                    fillZeros xs n = replicate (fromIntegral n - length xs) 0 ++ xs
                    dec2binHelper 0      = []
                    dec2binHelper decnum = dec2binHelper (decnum `div` 2) ++ [decnum `mod` 2]

-- Exercicio 3
-- example usage:
-- ghci> bincompl2dec [1, 1, 0, 1, 0, 0, 1]
-- -23
bincompl2dec :: [Int] -> Int
bincompl2dec [] = throw (InvalidParameterException "Cannot convert empty list")
bincompl2dec (b:bs) | b==0      = bin2dec bs
                    | b==1      = (-1) * bin2dec unsignedBinary
                    | otherwise = throw (InvalidParameterException "Non-binary value provided in the list")
                    where
                        unsignedBinary = incrementBinaryList negate'
                        negate' = [0^bit | bit <-bs]

-- Exercicio 4
-- example usage:
-- ghci> dec2bincompl (-10) 7
-- [1,1,1,1,0,1,1,0]
dec2bincompl :: Int -> Int -> [Int]
dec2bincompl d n | n<=0 = []
                 | d>=0 = 0:(dec2bin d n)
                 | otherwise  = 1:(incrementBinaryList (negate' unsignedBinary)) -- negative d
                 where
                    unsignedBinary = (dec2bin ((-1)*d) n)
                    negate' bs = [0^bit | bit <-bs]

-- Exercicio 5


-- Exercicio 6


-- Helper functions
incrementBinaryList :: (Eq a, Num a) => [a] -> [a]
incrementBinaryList bs = trimLeadingZero(reverse(incrementHelper(reverse(0:bs))))
    where
        incrementHelper []     = []
        incrementHelper (0:xs) = 1:xs
        incrementHelper (1:xs) = 0:incrementHelper xs
        incrementHelper (_:_)  = throw (InvalidParameterException "Cannot increment non-binary list")
        trimLeadingZero []    = []
        trimLeadingZero(0:xs) = xs
        trimLeadingZero(x:xs) = x:xs