{- Q1: Define a function that computes the sum of all integers between two numbers (the two numbers included). 
    The Haskell interaction may look like:
    > sumInts 0 1  --  0 + 1 =1
    1
    > sumInts 3 6  --  3 + 4 + 5 + 6 = 18
    18
    > sumInts 6 3
    0
-}
-- sumInts


{- Q2: Define a function that computes the sum of the squares of all integers between two numbers. 
    The Haskell interaction may look like:
    > sumSquares 0 1  --  0*0 + 1*1 =1
    1
    > sumSquares 3 6  --  3*3 + 4*4 + 5*5 + 6*6 = 86
    86
    > sumSquares 6 3
    0
    > sumSquares 6 6
    36
-}
-- sumSquares


{- Q3: Define a higher order sum function, higherOrderSum, which accepts an (Int -> Int) function to apply to all integers between two values (the two numbers included), such that sumSquares' and sumInts' defined below have the same functions as sumSquares (in Q2) and sumInts (in Q1) respectively.
-}

sumInts :: Int -> Int -> Int
sumInts x y
 | x == y = y
 | x > y = 0
 | otherwise = x + sumInts (x + 1) y
 
sumSquares :: Int -> Int -> Int
sumSquares a b
 | a > b = 0
 | a == b = a * a
 | otherwise = a * a + sumSquares (a + 1) b
 
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum fun c d =
 sum $ map fun [c..d]
sumInts' = higherOrderSum (*1)
sumSquares' = higherOrderSum (^2)


main:: IO()
main = do
    putStrLn "Please type two integers:"
    num1st <- getLine
    num2nd <- getLine

    let num1 = read num1st::Int
    let num2 = read num2nd::Int

    putStrLn $ "> Q1: The sum of all integers between the two numbers is: " 
    print $ sumInts num1 num2
    
    putStrLn $ "> Q2: The sum of the squares of all integers between the two numbers is: "   
    print $ sumSquares num1 num2

    putStrLn $ "> Q3: The sum of all integers between the two numbers is: "
    print $ sumInts' num1 num2

    putStrLn $ "> Q3: The sum of the squares of all integers between the two numbers is: "
    print $ sumSquares' num1 num2
    
    