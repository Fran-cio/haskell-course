-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

-- The Bounded type class in Haskell provides a way to work with types that have an upper and lower bound.

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
 {-
    Both Int and Word are instances of the Bounded type class, but they represent different ranges of integer values.

        Int:
        The Int type represents signed integers.
        The range of values for Int is platform-dependent. On most platforms, it covers at least the range from -2^63 to 2^63 - 1.
        The minBound and maxBound values for Int represent the minimum and maximum values that can be represented by the Int type on the current platform.
        Word:
        The Word type represents unsigned integers.
        The range of values for Word is platform-dependent as well. On most platforms, it covers at least the range from 0 to 2^64 - 1.
        The minBound value for Word is always 0, as Word cannot represent negative values.
        The maxBound value for Word represents the maximum value that can be represented by the Word type on the current platform.
 -}

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

--  The Enum type class in Haskell provides functionality for sequentially ordered types.
--  It allows types to be enumerated, meaning they can be treated as sequences of values that can be incremented and decremented.
--  The Enum type class is defined in the Prelude module.

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

f1 :: (Show a, Fractional a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x / y) ++ z

f2 :: (Enum a, Eq a, Bounded a) => a -> a
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

--  The numeric type classes in Haskell provide a range of behaviors to work with numeric types and to convert between them.
--  These type classes include Num, Integral, Fractional, and RealFrac.