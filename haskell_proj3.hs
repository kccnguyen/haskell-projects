{-|
The Greek mathematician Nicomachus devised a 3-way classification of all natural numbers based on their aliquot sum. The aliquot sum of any integer n > 0 is defined as the sum of all factors of n, not including n itself. For example, the aliquot sum of 15 is (1 + 3 + 5) = 9, and the aliquot sum of 28 is (1 + 2 + 4 + 7 + 14) = 28. The number n is called deficient if its aliquot sum is smaller than n; it is called perfect if its aliquot sum is n, and is called abundant if its aliquot sum is larger than n.

Write a function classify n, which takes an integer n as input, and returns its classification. The classification of any integer smaller than 1 can be stated as illegal.

The Haskell interaction may look like: 
> classify 12 
"Abundant" 
> classify 0 
"Illegal"
> classify 28
"Perfect"
> classify 8 
"Deficient"
-}
-- classify n 


{-|
Write a function bin2dec s, which takes as input a string s containing a binary number, and returns that number (in decimal). As a special case, it returns 0 for the empty string.

HINT: You may use recursion and split the given string by the built-in functions init and last.

The Haskell interaction may look like:
> bin2dec "1110" 
14
> bin2dec "0" 
0
> bin2dec (take 4 (cycle "01")) 
5
> bin2dec " " 
0
> bin2dec "1"
1
-}

-- bin2dec s = undefined


{-|
USE RECURSION to design a calculator that computes combinations. An example is avaiable at https://www.calculatorsoup.com/calculators/discretemathematics/combinations.php

The Combinations Calculator will find the number of possible combinations that can be obtained by taking a sample of items from a larger set. Basically, it shows how many different possible subsets can be made from the larger set. For this calculator, the order of the items chosen in the subset does not matter.

HINT: If division is used in your codes, make you're using compatible data types in the Fractional type class. You can simply use a float type or double type (make sure you change the IO part of the program as well). 

The Haskell interaction may look like:
    > nCr 0 0  
        1
    > nCr 3 2  
        3
    > nCr 8 3  
        56
-}

-- nCr n r = undefined

classify :: Int -> String
classify n
 | n < 1            = "Illegal"
 | aliquotForm < n  = "Deficient"
 | aliquotForm > n  = "Abundant"
 | otherwise        = "Perfect"
 where
    aliquotForm = sum factors
    factors = [ i | i <- [1 .. (n-1) ], n `mod` i == 0 ]

bin2dec :: String -> Int
bin2dec i
 | i == "" = 0
 | i == "0" = 0
 | otherwise = 2 * bin2dec (show (div (read i :: Int) 10 ) ) + ( mod (read i :: Int) 10)

nCr :: Float -> Float -> Float
nCr _ 0 = 1
nCr 0 _ = 0
nCr n r = nCr (n-1) (r-1) + nCr (n-1) r

main:: IO()

main = do
    putStrLn "Please type an integer:"
    num <- getLine
    putStrLn $ "> The classification of the number is: " ++ classify (read num::Int) 

    putStrLn "Please type a string containing a binary number:"
    str <- getLine
    putStrLn $ "> The corresponding binary number is: "++ (show $ bin2dec str)

    putStrLn "Please provide two whole numbers (the 1st number must be no less than the 2nd one):"
    num1 <- getLine
    num2 <- getLine
    putStrLn $ "> The number of combinations is: "++ ( show $ nCr (read num1::Float) (read num2::Float)) 