-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`
funcFlip :: (a -> a -> a) -> a -> a -> a
funcFlip f x y = f y x

-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).
funcUnCurry :: (a -> b -> c) -> (a,b) -> c
funcUnCurry func (x,y) = func x y

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
funcCurry :: ((a,b) -> c) -> a -> b -> c
funcCurry func x y = func (x,y)


-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 
isCharUpper :: Char -> Bool
isCharUpper c = c >= 'A' && c <= 'Z'

isUpper' :: (Char -> Bool) -> String -> Bool
isUpper' = any

isUpperRedy :: String -> Bool
isUpperRedy = isUpper' isCharUpper

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count' :: String -> Int
count' t = length $ filter (==t) votes

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

anyCarsLeft :: String -> Bool
anyCarsLeft brand = any (\(b, num) -> b == brand && num > 0) cars
