module Main where
import MLexer


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




prog -> stmt.						
stmt -> IF expr thenpart.
stmt -> WHILE expr dopart.
stmt -> INPUT ID.
stmt -> ID ASSIGN expr.
stmt -> WRITE expr.
stmt -> BEGIN stmtlist endpart.
thenpart -> THEN stmt elsepart.
elsepart -> ELSE stmt.
dopart -> DO stmt.
endpart -> END.
*stmtlist -> *stmtlist2.
*stmtlist2 -> stmt semipart.
*stmtlist2 -> .
semipart -> SEMICOLON stmtlist2.
expr -> term *expr2.
*expr2 -> addop term *expr2.
*expr2 -> .
addop -> ADD.
addop -> SUB.
term -> factor *term2.
*term2 -> mulop factor *term2.
*term2 -> .
mulop -> MUL.
mulop -> DIV.
factor -> LPAR expr rparpart.
factor -> ID.
factor -> NUM.
factor -> SUB NUM.
rparpart -> RPAR.

-}
{-

prog :: [Lexeme] -> Either String [Lexeme]
prog ts = stmt ts	

stmt :: [Lexeme] -> Either String [Lexeme]
stmt (LEX IF 		_:ts) = thenpart (expr ts)
stmt (LEX WHILE 	_:ts) = dopart (expr ts)
stmt (LEX INPUT ID 	_:ts) = ts  
stmt (LEX ID ASSIGN _:ts) = expr ts
stmt (LEX WRITE 	_:ts) = expr ts
stmt (LEX BEGIN 	_:ts) = endpart (stmtlist ts)

thenpart :: [Lexeme] -> Either String [Lexeme]
thenpart ts = elsepart (stmt ts)

elsepart :: [Lexeme] -> Either String [Lexeme]
elsepart ts = stmt ts

dopart :: [Lexeme] -> Either String [Lexeme]
dopart ts = stmt ts

endpart :: [Lexeme] -> Either String [Lexeme]
endpart ts = ts
	
stmtlist :: [Lexeme] -> Either String [Lexeme]
stmtlist ts = stmtlist2 ts

stmtlist2 :: [Lexeme] -> Either String [Lexeme]
stmtlist2 ts = semipart (stmt ts)
stmtlist2 _ = ts

semipart :: [Lexeme] -> Either String [Lexeme]
semipart (LEX SEMICOLON _:ts) = stmtlist2 ts 

expr :: [Lexeme] -> Either String [Lexeme]
expr ts = expr2 (term ts)

expr2 :: [Lexeme] -> Either String [Lexeme]
expr2 -> addop term *expr2.
expr2 -> .

addop :: [Lexeme] -> Either String [Lexeme]
addop (LEX ADD _:ts) = ts
addop (LEX SUB _:ts) = ts

term :: [Lexeme] -> Either String [Lexeme]
term ts =  term2 (factor ts)

term2 :: [Lexeme] -> Either String [Lexeme]
term2 -> mulop factor *term2.
term2 -> .

mulop :: [Lexeme] -> Either String [Lexeme]
mulop (LEX MUL _:ts) = ts
mulop (LEX DIV _:ts) = ts

factor :: [Lexeme] -> Either String [Lexeme]
factor (LEX LPAR _:ts) = rparpart (expr ts)
factor (LEX ID _:ts) = ts
factor (LEX (NUM i) _:ts) = ts
factor (LEX SUB _: LEX (NUM i) _ : ts)  = ts

rparpart :: [Lexeme] -> Either String [Lexeme]
rparpart -> RPAR.
	
	-}
	{-
exp1:: [Lexeme] -> Either String [Lexeme]
exp1  ts = more_exp rem
	where
		(Right rem) = term ts
---------------------------------------------

more_exp :: [Lexeme] ->  Either String [Lexeme]
more_exp (LEX ADD p:ts)  = more_exp rem
more_exp ts        = Right ts 
	where
		(Right rem) = term ts
----------------------------------------------

term :: [Lexeme] -> Either String [Lexeme]
term  ts           = more_term rem
	where
		(Right rem) = factor ts

-----------------------------------------------

more_term :: [Lexeme] -> Either String [Lexeme]
more_term (LEX MUL p:ts) = more_term rem
more_term ts       = Right ts
	where
		(Right rem) = factor ts

-----------------------------------------------

factor :: [Lexeme] ->  Either String [Lexeme]
factor ((LEX (NUM i) n):ts)  = Right ts
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 
-}



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

{-	
exp1:: [Lexeme] -> Either String [Lexeme]
exp1  ts = do
    rem <-  term ts  
    more_exp rem 
	
---------------------------------------------

more_exp :: [Lexeme] ->  Either String [Lexeme]
more_exp (LEX ADD p:ts)  = do 
        rem <- term ts
        more_exp rem 
more_exp ts        = Right ts 
----------------------------------------------

term :: [Lexeme] -> Either String [Lexeme]
term  ts           = do
       rem <- factor ts
       more_term rem 

-----------------------------------------------

more_term :: [Lexeme] -> Either String [Lexeme]
more_term (LEX MUL p:ts) = do 
    rem <- factor ts
    more_term rem 
more_term ts       = Right ts

-----------------------------------------------

factor :: [Lexeme] ->  Either String [Lexeme]
factor ((LEX (NUM i) n):ts)  = Right ts
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 

-}
							  
main = do
  output <- mlex
  case output of 
    Left lexStr -> putStrLn lexStr
    Right tokList -> do
      let parseRes = prog tokList
      case parseRes of
        Left str -> putStrLn str
        Right [] -> putStrLn "Parse Successful.\n"

     
    
