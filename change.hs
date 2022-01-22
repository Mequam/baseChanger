defaultKey = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" 

--gets the value of a char with a given key in the most general form
valueGeneral :: Int -> String -> Char -> Int
valueGeneral val (k:kl) l
 | k == l = val
 | otherwise = valueGeneral (val+1) kl l
--gets the value of a char with some pre-defined defaults
valueChar = (valueGeneral 0 defaultKey)

--inversion of the valueGeneral function
valueIntGeneral :: Int -> String -> Int -> Char
valueIntGeneral firstSymbolVal symbolList numericVal = (symbolList !! (numericVal-firstSymbolVal))
valueInt = (valueIntGeneral 0 defaultKey)

--convert a string from a given base to the arcitectural implimentation
--NOTE: this works with least significat digit on the left!
--you need to reverse the incoming string before feeding it into this function 
--to get expected results
fromBaseStrRev :: Int -> String -> Int
fromBaseStrRev base str = foldr1 (+) [(valueChar (snd ele))*(base^(fst ele))|ele<-(zip [0..((length str)-1)] str)]

fromBaseStr :: Int -> String -> Int
fromBaseStr base str = fromBaseStrRev base (reverse str)

--displays a number in a given base
--DESIGN NOTE:
--    this works because modulus with the base in ANY base returns the first digit of a number
--    then division by the base in any base will remove the zero on the end of the number if it has it
--    so we can use that to "ask questions" about what the last digits of a given base will be until the
--    number that we are left with is zero, indicating that we are done
showBase :: Int -> Int -> String
showBase _ 0 = []
showBase base x = (showBase base ((x-digit) `div` base))++[(valueInt digit)] where digit = (x `mod` base)
