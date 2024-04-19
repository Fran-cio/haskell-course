-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
{-# LANGUAGE BlockArguments #-}
import GHC.Float (leDouble)

consumoMensual :: Int -> Int -> Int -> String
consumoMensual c h m
    | c * h * 30 > m = "Ta pasao"
    | c * h * 30 < m = "Ta sobrao"
    | c * h * 30 == m = "Ta justo"

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
consumoMensual' :: Int -> Int -> Int -> String
consumoMensual' c h m
    | c * h * 30 > m = "Ta pasao por " ++ show (c * h * 30 - m)
    | c * h * 30 < m = "Ta sobrao por " ++ show (m - c * h * 30)
    | c * h * 30 == m = "Ta justo"

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
ventajasDeLet :: (Floating a, Ord a) => a -> a -> String
ventajasDeLet r h =
    let
        areaCirculo r = pi * r^2
        volumenCilindro r h = areaCirculo r * h
    in
        if volumenCilindro r h >= 42 then "Es mas grande" else "Es mas chico"

-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
division :: (Floating a, Ord a, Show a) => a -> a -> String
division a b =
    if a>b && a/=0 then  show (b/a) else if b>a && b/=0 then show (a/b) else "No se puede dividir por 0 loco, no aprendes mas"

division' :: (Floating a, Ord a, Show a) => a -> a -> String
division' a b
    | a>b && a/=0 = show (b/a)
    | b>a && b/=0 = show (a/b)
    | otherwise = "No se puede dividir por 0 loco, no aprendes mas"

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
sumOfSquareRoots :: Floating a => a -> a -> a
sumOfSquareRoots x y = let
    product' = x * y
    quotient' = x / y
  in
    squareRoot product' + squareRoot quotient'
  where
    squareRoot z = sqrt z