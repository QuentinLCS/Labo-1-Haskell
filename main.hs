import Data.List
import Data.Char
import System.IO

nextLetter :: Char -> Char
nextLetter letter
    | letter == 'Z' = 'A' 
    | letter == 'z' = 'a' 
    | isDigit letter = '-'
    | otherwise = chr (ord (letter) + 1)

rSum(x:y:list) = x:rSum(x + y : list)
rSum x = x

rPro(x:y:list) = x:rPro(x * y : list)
rPro x = x

biggest :: Int -> Int -> Int
biggest number1 number2 
    | number1 > number2 = number1
    | number2 > number1 = number2
    | otherwise = -1

fact :: Int -> Int
fact number = last(rPro([1..number]))

rFact(x:y:list) = x:rFact(fact(y) : list)
rFact x = x

sumFact :: Int -> Int
sumFact number = last(rSum(rFact([0..number])))

sumPairs :: [(Int, Int)] -> [Int]
sumPairs list = map (\(x,y) -> x+y) list

firstElem :: [Int] -> Int
firstElem list = head(list)

lastElem :: [Int] -> Int
lastElem list = last(list)

dividersOf :: Int -> [Int]
dividersOf number = [ x | x <- [1..number], (mod number x) == 0]

isPartOf :: Int -> [Int] -> Bool
isPartOf number list = elem number list

replaceBy :: Char -> Char -> [Char] -> [Char]
replaceBy old new string = map (\x -> if (x == old) then new else x) string

countChar :: Char -> [Char] -> Int
countChar c string = length (filter (\x -> x == c) string)

multOf5_a = [ x | x <- [1..50], (mod x 5) == 0]
multOf5_b = filter (\x -> (mod x 5) == 0) [1..50]

extractUpper :: [Char] -> [Char]
extractUpper string = filter (\x -> (isUpper x)) string

main = do
    --
    -- -
    --
    putStrLn ("------- 3 -------")
    putStrLn ("--- QUESTION 1 ---")
    putStrLn ("Next letter to L is " ++ [nextLetter('L')])
    putStrLn ("Next letter to z is " ++ [nextLetter('z')])
    putStrLn ("Next letter to 6 is " ++ [nextLetter('6')])
    putStrLn ("--- QUESTION 2 ---")
    putStr ("Recursive sum of [2..5] is ")
    print(rSum([2..5]))
    putStrLn ("--- QUESTION 3 ---")
    putStr ("Recursive product of [1..6] is ")
    print(rPro([1..6]))
    putStrLn ("--- QUESTION 4 ---")
    putStr ("The biggest between 12 and 55 is ")
    print (biggest 12 55)
    putStrLn ("--- QUESTION 5 ---")
    putStr ("The factorial of 7 is ")
    print (fact 7)
    putStrLn ("--- QUESTION 6 ---")
    putStr ("The sum of the factorials of [0..4] is ")
    print (sumFact(4))
    putStrLn ("")
    -- 
    -- Lists
    --
    putStrLn ("------- 5 -------")
    putStrLn ("--- QUESTION 1 ---")
    putStr ("The sum of (1,2) & (5,5) is ")
    print (sumPairs([(1,2), (5,5)]))
    putStrLn ("--- QUESTION 2 ---")
    putStr ("The first elem of [1,2,3] is ")
    print (firstElem([1..3]))
    putStr ("The last elem of [1,2,3] is ")
    print (lastElem([1..3]))
    putStrLn ("--- QUESTION 3 ---")
    putStr ("20 can be divided by ")
    print (dividersOf(20))
    putStrLn ("--- QUESTION 4 ---")
    if isPartOf 2 [1,2,3] 
        then putStrLn ("2 is part of [1,2,3] ")
        else putStrLn ("2 is not part of [1,2,3] ")
    if isPartOf 4 [1,2,3] 
        then putStrLn ("4 is part of [1,2,3] ")
        else putStrLn ("4 is not part of [1,2,3] ")
    putStrLn ("--- QUESTION 5 ---")
    putStrLn ("Replace 0 by 2 in I am 20y/o -> " ++ replaceBy '0' '2' "I am 20y/o")
    putStrLn ("--- QUESTION 6 ---")
    putStr ("How many 2 are in I am 22y/o ? ")
    print(countChar '2' "I am 22y/o")
    putStr ("How many a are in I am 22y/o ? ")
    print(countChar 'a' "I am 22y/o")
    putStr ("How many z are in I am 22y/o ? ")
    print(countChar 'z' "I am 22y/o")
    putStrLn ("--- QUESTION 7 ---")
    putStrLn ("Mult of 5 between 0 < x <= 50")
    print(multOf5_a)
    print(multOf5_b)
    putStrLn ("--- QUESTION 8 ---")
    putStrLn ("Uppercase from the string : Je suis Quentin LECHASLES")
    print(extractUpper "Je suis Quentin LECHASLES")

