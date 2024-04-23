import Control.Applicative (Alternative(empty))
import Data.Binary.Get (Decoder(Fail))
-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

saca4 :: [([Int], [Int])] -> Int
saca4 [(_,[_,a]),_] = a
saca4 _ = error "No esta"

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
remover3 :: [a] -> [a]
remover3 (_:_:_:rest) = rest
remover3 x = x

remover3' :: [a] -> [a]
remover3' x = case x of
    (_:_:_:rest) -> rest
    x -> x

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
sum3 :: (Int,Int,Int) -> Int
sum3 (a,b,c) = a+b+c



-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
isVacia :: [a] -> Bool
isVacia [] = True
isVacia _ = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [] = []

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
addEven :: Int -> Int
addEven x = case even x of
    True -> x+1
    False -> x
