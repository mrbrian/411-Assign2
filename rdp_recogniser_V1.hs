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


prog :: [Lexeme] -> Either String [Lexeme]
prog ts = do 
	stmt ts	


stmt :: [Lexeme] -> Either String [Lexeme]
stmt (LEX IF p:ts)  = do 
	stmt ts		


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


main = do
  tokList <- mlex
  let parseRes = exp1 tokList
  case parseRes of
    Left str -> putStrLn (str ++ "fart")
    Right [] -> putStrLn "Parse Successful.\n"

     
    
