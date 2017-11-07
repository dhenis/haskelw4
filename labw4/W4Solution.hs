
{- IMPORTANT: 
BEFORE DOING ANYTHING ELSE THIS WEEK LOG INTO LINUX AND RUN
stack-check
AS PER EMAIL ON FORUM
-}

module W4S
where

{-
HAVE YOU RUN stack-check???
-}

-- Question 1: Recursion

-- a. Generalised counting
-- Use recursion to define a function that counts the number of elements of a list that 
-- satisfy a given predicate
count' :: (a -> Bool) -> [a] -> Int
count' p [] = 0
count' p (x:xs) | p x = (count' p xs) + 1
count' p (x:xs) | otherwise = count' p xs

-- b. Lookup'
-- Use recursion to define a function that looks up a variable name in a table and 
-- returns the value of the variable. A table is a list of type [(String,a)] containing
-- variable name-value pairs. 
-- Examples: 
-- lookup' [("x",3),("y",4)] "y" == 4 
-- lookup' [("x",3),("y",4)] "z" == error "no such variable: z"
lookup' :: [(String,a)] -> String -> a
lookup' [] s = error $ "no such variable: "++s
lookup' ((s1,a):rest) s | s==s1 = a
lookup' ((s1,a):rest) s | otherwise = lookup' rest s


-- Question 2: Map

-- Note that these declarations do not always include the list that the function 
-- is to be applied to. 

-- a. Implement a function mapAdd1 that adds 1 to each element of a list
-- (use a section to do this)
mapAdd1 :: [Int] -> [Int]
mapAdd1 = map (+1)
-- check
-- mapAdd1 [] == []
-- mapAdd1 [1,2] == [2,3]

-- b. Implement a function that indents text by adding a fixed number of spaces 
-- at the start of each string (this can then be applied to a list of lines). 
-- Use a lambda expression. You may find replicate useful. 
mapAddSpace :: Int -> [String] -> [String]
mapAddSpace n = map (\s -> (replicate n ' ')++s)
-- check
-- mapAddSpace 2 [] == []
-- mapAddSpace 2 ["cat","dog"] == ["  cat","  dog"]

-- c. A student mark is given as a pair of Student Marks are given as a list of pairs
-- of type (String,Int) consisting of the student's name and their mark. 
-- Letter grades are given by the datatype
data Grade = F | P | M | D deriving (Ord,Eq,Show,Read)
-- D is 70+
-- M is 60-69
-- P is 50-59
-- F is 0-49
-- Implement a function that takes a numeric mark and returns the corresponding letter 
-- grade. 
grade :: Int -> Grade
grade n | n >= 70           = D
grade n | n >= 60 && n < 70 = M
grade n | n >= 50 && n < 60 = P
grade n | n < 50            = F


-- Implement a function that takes a list of Student Marks and adds the grades:
addGrades :: [(String,Int)] -> [(String,Int,Grade)]
addGrades = map (\ (s,n) -> (s,n,grade n))
-- check
-- addGrades [] == []
-- addGrades [("janet",75),("jill",63)] == [("janet",75,D),("jill",63,M)]
-- Implement a function that takes a list of Student Marks (name,mark) 
-- and outputs a list of Strings containing feedback messages: 
-- example
-- messages [("janet",75),("jill",63)] 
-- == ["To janet: you got 75, which is grade D","To jill: you got 63, which is grade M"]
-- Note that the function "show" will convert data to String's. 
messages :: [(String,Int)] -> [String]
messages = map (\ (s,n) -> "To "++s++": you got "++(show n)++", which is grade "++(show $ grade n))


-- Question 3: fold

-- Using the solutions given and the compilation method shown in lectures, implement 
-- the following functions from week 3 using a single fold definition. 

-- Calculate the length of a list
length' :: [a] -> Int
--Q length' [] = undefined
--Q length' (x:xs) = undefined
--length' [] = 0
--length' (x:xs) = 1 + length' xs
length' = foldr (\a b -> 1+b) 0

-- Count the number of zeros in a list
-- countZeros [1,3,0,4,0,1] = 2
countZeros :: [Int] -> Int
--Q countZeros [] = undefined
--Q countZeros (x:xs) = undefined
-- countZeros [] = 0
-- countZeros (x:xs) = if x==0 then 1 + countZeros xs else countZeros xs
countZeros = foldr (\a b -> if a==0 then 1+b else b) 0

-- Count the number of even elements (you may want to define an auxiliary function
-- even). 
-- countEven [1,3,0,4,0,1] = 3
countEven :: [Int] -> Int
--Q countEven [] = undefined
--Q countEven (x:xs) = undefined
even' x = mod x 2 == 0
--countEven [] = 0
--countEven (x:xs) = if even' x then 1 + countEven xs else countEven xs
countEven = foldr (\a b -> if even' a then 1+b else b) 0

-- Sum the elements of the list
sum' :: [Int] -> Int
--Q sum' [] = undefined
--Q sum' (x:xs) = undefined
-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs
sum' = foldr (\a b -> a + b) 0
-- or just sum' = foldr (+) 0

-- Sum the odd elements of the list
sumOdd :: [Int] -> Int
--Q sumOdd [] = undefined
--Q sumOdd (x:xs) = undefined
-- sumOdd [] = 0
-- sumOdd (x:xs) = if (odd x) then x + sumOdd xs else sumOdd xs
sumOdd = foldr (\a b -> if odd a then a+b else b) 0

-- Calculate the product of the elements of the list
product' :: [Int] -> Int
--Q product' [] = undefined
--Q product' (x:xs) = undefined
--product' [] = 1
--product' (x:xs) = x * product' xs
product' = foldr (\a b -> a*b) 1
-- or just foldr (*) 1

-- Calculate the product of the non-zero elements of the list
productNonZero :: [Int] -> Int
--Q productNonZero [] = undefined
--Q productNonZero (x:xs) = undefined
-- productNonZero [] = 1
-- productNonZero (x:xs) = if x /= 0 then x * productNonZero xs else productNonZero xs
productNonZero = foldr (\a b -> if a /= 0 then a * b else b) 1

-- Count the number of spaces in a String
spaces :: String -> Int
--Q spaces [] = undefined
--Q spaces (x:xs) = undefined
-- spaces [] = 0
-- spaces (x:xs) = if x == ' ' then 1 + spaces xs else spaces xs
spaces = foldr (\a b -> if a == ' ' then 1 + b else b) 0

-- Take the substring of a String up to the first space character
takeToSpace :: String -> String
--Q takeToSpace [] = undefined
--Q takeToSpace (x:xs) = undefined
-- takeToSpace [] = []
-- takeToSpace (x:xs) = if x == ' ' then [] else x:(takeToSpace xs)
takeToSpace = foldr (\a b -> if a==' ' then [] else a:b) []

-- Take the substring of a String after the first space character
dropToSpace :: String -> String
--Q dropToSpace [] = undefined
--Q dropToSpace (x:xs) = undefined
-- dropToSpace [] = []
-- dropToSpace (x:xs) = if x == ' ' then xs else dropToSpace xs
dropToLastSpace xs =  foldr (\a b -> if a==' ' then b else tail b) xs xs
dropToSpace xs 
  = let (f,b) = foldl 
                  (\(f,b) a -> 
                     if f then (True,b) else 
                     if a==' ' then (True, tail b) else (False,tail b))    
                  (False,xs) xs 
     in b
     



-- Question 4: Arithmetic Expressions

-- The code below implements a datatype for arithmetic expressions, an evaluation 
-- function and a crude print function (crude because it puts outer expressions in 
-- parentheses). 

-- Modify the datatype so that it includes variables as Var String, so 
-- Var "x" :: ArithExp

-- Modify show so that it will print expressions including variables. 

-- The new expressions need to be evaluated in the context of an environment, a table 
-- of variable name-value pairs as in Q1. 
type Env = [(String,Int)]

-- Extend eval so that it will handle expressions that include variables. All 
-- expressions now need to be evaluated in the context of an environment. 
-- eval :: Env -> ArithExp -> Int

-- Further modify show so that it does not include the outermost parentheses. 
-- Suggestion: divide this into two functions - show handles the outermost call and 
-- innerShow handles any inner expressions. 
{-
ORIGINAL  
data ArithExp = 
  Const Int
 | Plus ArithExp ArithExp
 | Minus ArithExp ArithExp
 | Times ArithExp ArithExp
    deriving Eq
    
eval :: ArithExp -> Int
eval (Const n) = n
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)

instance Show ArithExp where 
  show (Const n) = show n
  show (Plus e1 e2)  = "("++(show e1)++" + "++(show e2)++")"
  show (Minus e1 e2) = "("++(show e1)++" - "++(show e2)++")"
  show (Times e1 e2) = "("++(show e1)++" * "++(show e2)++")"
    
--(3+4)*5  
exp1 = Times (Plus (Const 3) (Const 4)) (Const 5)
-}


data ArithExp = 
  Const Int
 | Var String
 | Plus ArithExp ArithExp
 | Minus ArithExp ArithExp
 | Times ArithExp ArithExp
    deriving Eq
       
    
eval :: Env -> ArithExp -> Int
eval e (Const n) = n
eval e (Var j) = lookup' e j
eval e (Plus e1 e2) = (eval e e1) + (eval e e2)
eval e (Minus e1 e2) = (eval e e1) - (eval e e2)
eval e (Times e1 e2) = (eval e e1) * (eval e e2)

innerShow (Const n) = show n
innerShow (Var x) = x
innerShow (Plus e1 e2)  = "("++(innerShow e1)++" + "++(innerShow e2)++")"
innerShow (Minus e1 e2) = "("++(innerShow e1)++" - "++(innerShow e2)++")"
innerShow (Times e1 e2) = "("++(innerShow e1)++" * "++(innerShow e2)++")"
  

instance Show ArithExp where 
  show (Const n) = show n
  show (Var x) = x
  show (Plus e1 e2)  = (innerShow e1)++" + "++(innerShow e2)
  show (Minus e1 e2) = (innerShow e1)++" - "++(innerShow e2)
  show (Times e1 e2) = (innerShow e1)++" * "++(innerShow e2)
  

-- Question 5: A basic imperative programming language

-- If you are ambitious, you can now extend this even further to a basic 
-- imperative programming language. 
type Exp = ArithExp
type Var = String
-- for this example we can take a state to be a list of variable name-value pairs 
-- as before with the environment. 
type State = Env

-- we now define the commands available
data Comm = Declare Var       -- declares new variable with value 0. 
          | Assign Var Exp    -- assign the result of the expression to the variable 
          | Seq [Comm]        -- perform these commands in sequence
          | If Exp Comm Comm  -- if statement (branch on Exp non-zero)
          | While Exp Comm    -- while loop
          deriving (Eq,Show)
          
-- The exercise is to define a function, that runs a given command.  
run :: Comm -> State -> State
run = undefined
          

    
