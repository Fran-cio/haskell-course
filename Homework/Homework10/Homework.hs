{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

class Container c where
  isEmpty   ::  c a -> Bool
  contains  ::  (Eq a) => c a -> a -> Bool
  replace   ::  c a -> b -> c b
  unWrap    ::  c a -> a

data Box a       = Empty          | Has a            deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a   deriving (Show)

instance Container Box where

  isEmpty Empty = True
  isEmpty _     = False

  contains Empty _    = False
  contains (Has a) b  = a == b

  replace _ = Has 

  unWrap (Has a)  = a 

instance Container (Present t) where

  isEmpty (EmptyPresent _)  = True
  isEmpty _                 = False

  contains (EmptyPresent _) _   = False
  contains (PresentFor _ a) b   = a == b

  replace (EmptyPresent t) = PresentFor t 
  replace (PresentFor t _) = PresentFor t 


  unWrap (PresentFor _ a) = a 

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

instance Container (MailedBox t d) where

  isEmpty (EmptyMailBox _ _)  = True
  isEmpty _                 = False

  contains (EmptyMailBox _ _) _   = False
  contains (MailBoxTo _ _ a) b   = a == b

  replace (EmptyMailBox t d) = MailBoxTo t d 
  replace (MailBoxTo t d _) = MailBoxTo t d


  unWrap (MailBoxTo _ _ a) = a 

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double deriving (Show)

instance Eq Salary where
  (==) (USD  x) (USD  y) = x == y
  (==) (EUR x) (EUR y) = x == y
  (==) (USD  x) (EUR y) = x == 0.93 * y
  (==) (EUR x) (USD  y) = x * 0.93 == y

instance Ord Salary where
  USD x `compare` USD y = x `compare` y
  EUR x `compare` EUR y = x `compare` y


data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address deriving (Show, Eq, Ord)

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }deriving (Show)

instance Eq Pokemon where
  (==) (Pokemon x _ _ _) (Pokemon y _ _ _) = x == y

instance Ord Pokemon where
  (Pokemon _ _ _ x) `compare` (Pokemon _ _ _ y)  = x `compare` y

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Num)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
