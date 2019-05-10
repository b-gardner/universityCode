
---------------------------
---------------------------
--                       --
--     Bryce Gardner     --
--       COMP3258        --
--     Assignment 2      --
--    UID: 3035426784    --
--                       --
---------------------------
---------------------------
import    Data.Char
import    Data.List
import    Parsing
import    System.IO
data Binop   = Add | Sub | Mul | Div | Mod deriving (Eq, Show)
data Expr    = Bin Binop Expr Expr
             | Val Int
             | Var String
             deriving (Eq,Show)

type Env     = [(String, Int)]

-- Problem 1, evaluate expression trees.

eval :: Env -> Expr -> Maybe Int
eval vars (Var s)        | s `elem` (fst$unzip vars) = Just$ [y | (x,y) <- vars, x == s] !! 0
                         | otherwise                 = Nothing
eval vals (Val c)        = Just c
eval vars (Bin op e1 e2) | ee1 == Nothing || ee2 == Nothing = Nothing
                         | op == Add                        =Just$ (toInt ee1) + (toInt ee2)
                         | op == Sub                        =Just$ (toInt ee1) - (toInt ee2)
                         | op == Mul                        =Just$ (toInt ee1) * (toInt ee2)
                         | op == Mod                        =if (toInt ee2) == 0 then Nothing else Just$ (toInt ee1) `mod` (toInt ee2)                         
                         | op == Div                        = if (toInt ee2) == 0 then Nothing else Just$ (toInt ee1) `div` (toInt ee2)
                             where ee1 = eval vars e1
                                   ee2 = eval vars e2

toInt :: Maybe Int -> Int -- fnction for transforming something I know is a Just Int into an Int 
toInt (Just x) = x
toInt (Nothing) = error "You shouldn't have a Nothing in here"

-- Problem 2, fn for Parsing Exprs 
-- This solution does not respect the left associativity of operators that was requested, sorry!
pTerm :: Parser Expr
pTerm = pOp_factor +++ pFactor

pExpr :: Parser Expr
pExpr = pOp_term +++ pTerm

pOp_term :: Parser Expr
pOp_term = pPlus +++ pMinus +++ failure
 where 
  pPlus = do 
        x <- token $ pTerm -- grab a number
        sat ('+' ==) -- ensure the operator
        y <- (pOp_term +++ pTerm) -- get 2nd half of expression
        return (Bin Add x y) 
  pMinus = do
        x <- pTerm
        token $ sat ('-' ==)
        y <- (pOp_term +++ pTerm)
        return (Bin Sub x y)

pFactor :: Parser Expr -- Factors are more loose, + -
pFactor = pPara +++ pInt +++ pIdent
 where
  pPara = do
     token $ char '('
     e <- pExpr
     token $ char ')'
     return e

  pInt  = do 
     x <- integer
     return $ Val x 

  pIdent = do
     x <- identifier
     return (Var x) 

pOp_factor :: Parser Expr -- "Factors" Are the sticky parts of the equation, * / or %
pOp_factor = pMul +++ pDiv +++ pMod +++ failure
 where
  pMul = do
      x <- pFactor
      sat ('*' ==)
      y <- pOp_factor +++ pFactor
      return (Bin Mul x y)
  pDiv = do
      x <- pFactor
      sat ('/' ==)
      y <- pOp_factor +++ pFactor
      return (Bin Div x y)
  pMod = do
      x <- pFactor
      sat ('%' ==)
      y <- pOp_factor +++ pFactor
      return (Bin Mod x y)
      
-- problem 3 make a front -end for this

runParser :: Parser a -> String -> Maybe a
runParser psr str | null results = Nothing
                  | otherwise    = if null $ snd $ results!!0 -- if parser could parse everything
                                   then Just $ fst $ results!!0 -- then return the result
                                   else Nothing
 where results = parse psr str

 --Problem 4
data Instr = IVal Int | IBin Binop | IVar String deriving (Eq,Show)
type Stack = [Int]
type Prog = [Instr]


runProg :: Prog -> Env -> Maybe Int
runProg [] e = error "You have an empty program"
runProg p e = if length stack == 1
              then
               Just (stack!!0)
              else
               Nothing
 where stack = runProg' p e []


runProg' :: Prog -> Env -> Stack -> Stack
runProg' ((IVal x):ps) e st = runProg' ps e (x:st) -- put a straight value onto the stack
runProg' ((IBin o):ps) e st | null $ drop 1 st = [] -- if the stack is too small to evaluate operator o then break
                            | otherwise        = runProg' ps e (st2) -- else evaluate and put onto stack
                             where st2 = (toInt $ eval e (Bin (o) (Val $ st!!0) (Val $ st!!1))):(drop 2 st)

runProg' ((IVar var):ps) e st | var `elem` ( fst $ unzip e) = runProg' ps e ((toInt (eval e (Var var))):st) 
                              | otherwise                = [] -- if var is not an elem, then it doesn't exist in the ENV
runProg' [] e st = st


-- Problem 5 compiler!
compile :: Expr -> Prog
compile (Val v)       = [IVal v]
compile (Var v)       = [IVar v]
compile (Bin o e1 e2) = (compile e2) ++ (compile e1) ++ [IBin o]

-- Problem 6 optimizer
-- Bryce Gardner 17/11/17 
optimize :: Expr -> Maybe Expr
optimize (Val x)                 = Just (Val x) -- basic cases
optimize (Var v)                 = Just (Var v)
optimize (Bin o e1 e2)           | o `elem` [Div,Mod] && (oe2 == zero ) = Nothing -- special cases
                                 | o == Sub && oe2 == zero = Just e1
                                 | o == Mul && (oe1 == zero || oe2 == zero) = Just (Val 0)
                                 | o == Add && (oe1 == zero || oe2 == zero) = if oe1 == zero
                                                                              then oe2
                                                                              else oe1
                                 where 
                                  oe1 = optimize e1
                                  oe2 = optimize e2
                                  zero = Just (Val 0)

optimize (Bin o (Val x) (Val y)) = Just $ Val $ toInt $ eval [] (Bin o (Val x) (Val y)) -- evaluatable case

optimize (Bin o e1 e2)           = if oe1 == Just e1 && oe2 == Just e2 -- normal case, keep looping until nothing changes when we try to optimize the constituents`
                                   then Just (Bin o (toExpr oe1) (toExpr oe2))
                                   else optimize $ (Bin o (toExpr oe1) (toExpr oe2))
                                 where
                                  oe1 = optimize e1
                                  oe2 = optimize e2

toExpr :: Maybe Expr -> Expr
toExpr (Just e) = e

-- Question 7
-- Ran out of time folks! Looks like a fun question though.

repl :: Env -> IO()
repl env = do
  putStr "\n "
  line <- getLine
  dispatch env line

quit :: IO ()
quit = return ()

loop :: String -> Env -> IO ()
loop str env  = do 
               putStrLn str
               repl env


dispatch :: Env -> String -> IO ()
dispatch e s | fpe == "let" = fLet e 
             | null $ spe = 
             where pe = parse pExpr s
              fpe = fst(pe!!0)
              spe = snd(pe!!0)



pInp :: Parser Expr -> env 
pInp e = pLet +++ error 

error :: Parser Expr
return "Error"

pLet :: Parser Expr
PLet = do
    sat ("let" ==)
    s <- pIdent
    char '='
    e <- pExpr
    return [(s,e)]

{-
plus :: Int -> Int -> Int
plus x y = x + y

t_o :: Int -> (Int,Int)
t_o x = (x,x)-}
