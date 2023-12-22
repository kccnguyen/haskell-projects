main :: IO ()
main = do 
    putStrLn "Please type in a year:"
    year <- getLine
    putStrLn ("Whether the year is a leap year: "++ year)
    print (show $ leap $ (read year::Int))
    
    putStrLn "Please type in your WSU ID:"
    studentid <- getLine
    putStrLn ("The regrouped string of " ++ studentid)
    print (show $ parse studentid)

leap :: Int -> Bool
leap year = 
    if year `mod` 400 == 0 
        then True
     else if year `mod` 100 == 0 
        then False
    else if year `mod` 4 == 0  
        then True
    else False
  
isNum :: Char -> Bool
isChar :: Char -> Bool
parse studentid = (filter isChar studentid ++ filter isNum studentid)
isNum x = 
    if x `elem` ['0'..'9'] 
        then True
    else False
isChar x = 
    if x `elem` ['0'..'9'] 
        then False
    else True