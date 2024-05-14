{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where
import Data.Maybe (fromJust,isJust)
import System.IO.Error (catchIOError)
import Data.Char (isDigit, isUpper, isLower)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe (a:_) = Just a
headMaybe _ = Nothing

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes :: [Maybe a] -> [a]
catMaybes list = map fromJust (filter isJust list)

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fn =do
  e <- readFile fn `catchIOError` handleFileNotFound
  case e of
    "Nothing" -> return Nothing
    _ -> return $ Just e
  where
    handleFileNotFound e = do
      putStrLn $ "{-# WARNING:"++ show e ++ " #-}"
      return "Nothing"


-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError = WrongConstructor deriving Show

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough str 
  | length str >= 10 = Right str
  | otherwise = Left WrongConstructor

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit str
  | any isDigit str = Right str
  | otherwise = Left WrongConstructor

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase str
  | any isUpper str = Right str
  | otherwise = Left WrongConstructor
 

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase str
  | any isLower str = Right str
  | otherwise = Left WrongConstructor
 
passwordRequirements :: String -> Either PasswordError String
passwordRequirements str = 
  passwordLongEnough str >>= passwordHasDigit >>= passwordHasUppercase >>= passwordHasLowercase
