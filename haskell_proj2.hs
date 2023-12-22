{-|
Design the function which should print a single digit number as English text, or "unknown" if it's out of the range 0-9. The function takes in an "Int" and outputs a "String".

Hint: You may use a list of strings for your outputs. 
 
The Haskell interaction may look like:
    > readDigit 2
        "two"
    > readDigit 9
        "nine"
    > readDigit 25
        "unknown"
-}

-- readDigit n = undefined

{-|
* Please implement the funtion by using either a "guards" or "case" structure. You will get 0 points if you implement the function in other ways, such as if-then-else.

The function leap n, which takes an integer n as input, and returns True if the year n is a leap year, and False otherwise.

Leap years are those that are evenly divisible by 4, except any year that is also evenly divisible by 100 unless that year is also evenly divisible by 400. So, for example, 1996, 2012, and 2020 are all leap years, but 2100, 2200, and 2300 are not leap years, because although they are all evenly divisible by 4, they are also evenly divisible by 100. However, 1600, 2000, and 2400 are leap years, because although they are divisible by 100, they are also divisible by 400.

The Haskell interaction may look like:
    > leap 1996 
        True
    > leap 2000
        True
    > leap 2100 
        False
-}

-- leap n = undefined

{-|
The function pangram s, which takes a string s as input, and returns True if s is a pangram, and False otherwise.

Pangrams are strings that contain at least one occurrence of each letter of the English alphabet. They are not case-sensitive, so a letter may appear in either the upper case, or lower, or both.

The Haskell interaction may look like:

    > pangram "The quick brown fox jumps over a lazy dog."
    True
    > pangram "Pack my box with five dozen liquor jugs!!"
    True
    > pangram "Watch JEOPARDY!, Alex Trebek’s fun TV quiz game!"
    True
    > pangram "I wanna be a Pangram, can I? :)"
    False
    > pangram "Amazingly few discotheques provide jukeboxes." 
    True
    > pangram "Back in June, we delivered oxygen equipment of the same size."
    True
    > pangram "My girl wove six dozen plaid jackets before she quit :("
    True
-}

import Data.Char (toLower)
import Data.List ((\\))

main:: IO()
main = do
    putStrLn "Please give me a number:"
    num <- getLine
    print ("The number reads: " ++ readDigit (read num::Int))

    putStrLn "Please type in a year:"
    year <- getLine
    print ("Whether the year is a leap year: "++ (show $ leap $ (read year::Int)))

    putStrLn "Please type in a string:"
    id <- getLine
    print ("Whether the string is a pangram: "++ ( show $ pangram id))

readDigit :: (Integral a) => a -> String
readDigit 0 = "Zero"
readDigit 1 = "One"
readDigit 2 = "Two"
readDigit 3 = "Three"
readDigit 4 = "Four"
readDigit 5 = "Five"
readDigit 6 = "Six"
readDigit 7 = "Seven"
readDigit 8 = "Eight"
readDigit 9 = "Nine"
readDigit x = "Unknown"

leap :: Int -> Bool
leap year
 | isDivisBy 400 = True
 | isDivisBy 100 = True
 | isDivisBy 4 = True
 | otherwise = False
 where
 isDivisBy :: Int -> Bool
 isDivisBy n = year `mod` n == 0
 
pangram :: String -> Bool
pangram = null . (['a' .. 'z'] \\) . map toLower
