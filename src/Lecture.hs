module Lecture where

--LEARNING THE FORM "x@pat"
--- /show
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

----------------------------------------------------------------------------------
--I will do a example to test my idea
data Lism = Empty
          | Cons Int Lism
  deriving Show

----------------------------------------------------------------------------------
  --This is my function
insert :: Int -> Lism -> Lism
insert a Empty = Cons a Empty
insert a f@(Cons b value)
  | a > b = Cons a f
  | a < b = Cons b (insert a value)
  | a == b = Cons b value
-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 30 SealingWax

-- show
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

--main = putStrLn (baz brent)

---------------------------------------------------
-- /show
data FailableDouble = Failure
                    | OK Double
  deriving Show

-- show
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d

main = print (failureToZero' Failure, failureToZero' (OK 3.4))
