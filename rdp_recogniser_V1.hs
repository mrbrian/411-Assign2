module Main where
import MLexer

data Exp = Add Exp Exp
		| Mul Exp Exp
		| Num Integer

{-     
Grammar                            Haskell Code
=======                            ============
exp -> term more_exp               exp ts = more_exp (term ts)

more_exp -> + term more_exp        more_exp (ADD:ts) = more_exp (term ts)
          |.                       more_exp ts = ts

term -> factor more_term           term ts = more_term (factor ts)

more_term -> * factor more_term    more_term (MUL:ts) = more_term (factor ts)
           |.                      more_term ts = ts

factor -> NUM.                     factor ((NUM n):ts) = ts

-}

{-
data Stmt = If Exp Stmt Stmt                        -- datatype for statements  starting with an if ... then ... else statement
			| While Exp Stmt
			| Assign String Exp
			| Block [Stmt]
			| Print Expr
			| Input Expr
				
data Exp = Add Exp Exp  
		   | Mul Exp Exp
		   | Div Exp Exp
		   | Id String
		   | Num Integer



prog :: [Lexeme] -> Either String [Lexeme]
prog ts = stmt ts
	
---------------------------------------------

stmt :: [Lexeme] -> Either String [Lexeme]
stmt (LEX IF _:ts) = do
	rem <- expr ts
	thenpart rem
stmt (LEX WHILE _:ts) = do
	rem <- expr ts
	dopart rem
stmt (LEX INPUT _: LEX (ID str) _:ts) = Right ts
stmt (LEX (ID str) _: LEX ASSIGN _:ts) = expr ts
stmt (LEX WRITE _:ts) = expr ts
stmt (LEX BEGIN _:ts) = do
	rem <- stmtlist ts
	endpart ts
stmt toks = doError toks
	
---------------------------------------------
	
doError :: [Lexeme] -> Either String [Lexeme]
doError toks = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a number got " ++ show (head toks) 

---------------------------------------------

thenpart :: [Lexeme] -> Either String [Lexeme]
thenpart (LEX THEN _:ts) = do
	rem <- stmt ts
	elsepart rem

---------------------------------------------

elsepart :: [Lexeme] -> Either String [Lexeme]
elsepart (LEX ELSE _:ts) = stmt ts

---------------------------------------------

dopart :: [Lexeme] -> Either String [Lexeme]
dopart (LEX DO _:ts) = stmt ts

---------------------------------------------

endpart :: [Lexeme] -> Either String [Lexeme]
endpart (LEX END _:ts) = Right ts

---------------------------------------------

stmtlist :: [Lexeme] -> Either String [Lexeme]
stmtlist ts = stmtlist2 ts

---------------------------------------------

stmtlist2 :: [Lexeme] -> Either String [Lexeme]
stmtlist2 (LEX IF _:ts) = do
	rem <- expr ts
	rem <- thenpart rem
	semipart rem	
stmtlist2 (LEX WHILE _:ts) = do
	rem <- expr ts
	dopart rem
stmtlist2 (LEX INPUT _: LEX (ID str) _:ts) = Right ts
stmtlist2 (LEX (ID str) _: LEX ASSIGN _:ts) = expr ts
stmtlist2 (LEX WRITE _:ts) = expr ts
stmtlist2 (LEX BEGIN _:ts) = do
	rem <- stmtlist ts
	endpart ts
stmtlist2 ts = Right ts

---------------------------------------------

semipart :: [Lexeme] -> Either String [Lexeme]
semipart (LEX SEMICOLON _:ts) = stmtlist2 ts

---------------------------------------------

expr :: [Lexeme] -> Either String [Lexeme]
expr ts = do
	rem <- term ts
	expr2 rem	

---------------------------------------------

expr2 :: [Lexeme] -> Either String [Lexeme]
expr2 (LEX ADD _:ts) = do
	rem <- term ts
	expr2 rem
expr2 (LEX SUB _:ts) = do
	rem <- term ts
	expr2 rem
expr2 ts = Right ts

---------------------------------------------
	
term :: [Lexeme] -> Either String [Lexeme]
term ts = do
	rem <- factor ts
	term2 rem
	
---------------------------------------------

term2 :: [Lexeme] -> Either String [Lexeme]
term2 (LEX MUL _:ts) = do
	rem <- factor ts
	term2 rem
term2 (LEX DIV _:ts) = do
	rem <- factor ts
	term2 rem
term2 ts = Right ts

---------------------------------------------

factor :: [Lexeme] -> Either String [Lexeme]
factor (LEX LPAR _:ts) = do
	rem <- expr ts
	rparpart rem
factor (LEX (ID str) _:ts) = Right ts
factor (LEX (NUM i) _:ts) = Right ts
factor (LEX SUB _ : LEX (NUM i) _ : ts) = Right ts

---------------------------------------------
	
rparpart :: [Lexeme] -> Either String [Lexeme]
rparpart (LEX RPAR _:ts) = Right ts

-}
exp1:: [Lexeme] -> Either String (Exp, [Lexeme])
exp1 ts = (, rem2) where
    rem1 <-  term ts  
    rem2 <- more_exp rem1
	
	
---------------------------------------------

more_exp :: Exp -> [Lexeme] ->  Either String (Exp, [Lexeme])
more_exp e (LEX ADD p:ts)  = do 
        rem <- term ts
        more_exp rem 
more_exp ts        = Right ts 
----------------------------------------------

term :: [Lexeme] -> Either String (Exp, [Lexeme])
term ts           = do
       rem <- factor ts
       more_term rem 

-----------------------------------------------

more_term :: Exp -> [Lexeme] -> Either String (Exp, [Lexeme])
more_term e (LEX MUL p:ts) = do 	-- multiply 
    rem <- factor ts
    more_term rem 
more_term e ts       = Right ts		-- blank, what is the exp now?

-----------------------------------------------

factor :: [Lexeme] ->  Either String (Exp, [Lexeme])
factor (LEX (NUM i) n:ts)  = Right (Num i, ts)
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 


term1:: Exp -> [Lexeme] -> (Exp,[Lexeme])
term1 e (LEX MUL _:ts) = (MUL e e',ts')  where
		 (e1,ts1) = factor ts
		 (e',ts') = term1 e1 ts1
term1 e ts = (e,ts)


main = do
  output <- mlex
  case output of 
    Left lexStr -> putStrLn lexStr
    Right tokList -> do
      let parseRes = exp1 tokList
      case parseRes of
        Left str -> putStrLn str
        Right [] -> putStrLn "Parse Successful.\n"

     
    
