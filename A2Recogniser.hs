module Main where
import A2Lexer
import System.Environment
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

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
*stmtlist2 -> IF expr thenpart semipart
*stmtlist2 -> WHILE expr dopart semipart
*stmtlist2 -> INPUT ID semipart
*stmtlist2 -> ID ASSIGN expr semipart
*stmtlist2 -> WRITE expr semipart
*stmtlist2 -> BEGIN stmtlist endpart semipart
*stmtlist2 -> .
semipart -> SEMICOLON stmtlist2.
expr -> term *expr2.
*expr2 -> ADD term *expr2.
*expr2 -> SUB term *expr2.
*expr2 -> .
term -> factor *term2.
*term2 -> MUL factor *term2.
*term2 -> DIV factor *term2.
*term2 -> .
factor -> LPAR expr rparpart.
factor -> ID.
factor -> NUM.
factor -> SUB NUM.
rparpart -> RPAR.

-}

data Stmt a = If (Exp a) (Stmt a) (Stmt a) 		-- datatype for statements  starting with an if ... then ... else statement
			| While (Exp a) (Stmt a)
			| Assign String (Exp a)
			| Block [Stmt a]
			| Print (Exp a)
			| Input (Exp a)
           deriving (Eq,Show,Read)
				
data Exp a = Add (Exp a) (Exp a)
		   | Mul (Exp a) (Exp a)
		   | Div (Exp a) (Exp a)
		   | Neg (Exp a)
		   | Id String
		   | Num Int
           deriving (Eq,Show,Read)

		   
prog :: [Lexeme] -> Either String ([Lexeme], Stmt String)
prog ts = stmt ts
	
---------------------------------------------

stmt :: [Lexeme] -> Either String ([Lexeme], Stmt String)
stmt (LEX IF _:ts) = do
	(rem, e1) <- expr ts
	(rem2, s1, s2) <- thenpart rem	
	Right (rem2, If e1 s1 s2)
stmt (LEX WHILE _:ts) = do
	(rem, e1) <- expr ts
	(rem2, s1) <- dopart rem
	Right (rem2, While e1 s1)
stmt (LEX INPUT _: LEX (ID str) _:ts) = Right (ts, Input (Id str))
stmt (LEX (ID str) _: LEX ASSIGN _:ts) = do
	(rem, e1) <- expr ts
	Right (rem, Assign str e1)
stmt (LEX WRITE _:ts) = do
	(rem, e1) <- expr ts
	Right (rem, Print e1)
stmt (LEX BEGIN _:ts) = do
	(rem, s1) <- stmtlist [] ts
	rem2 <- endpart rem
	Right (rem2, Block s1)
stmt toks = Left $ "Error:In stmt: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a STMT got " ++ show (head toks) 

---------------------------------------------

thenpart :: [Lexeme] -> Either String ([Lexeme], Stmt String, Stmt String)
thenpart (LEX THEN _:ts) = do
	(rem1, s1) <- stmt ts
	(rem2, s2) <- elsepart rem1
	Right (rem2, s1, s2)
thenpart toks = Left $ "Error:In thenpart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a THEN got " ++ show (head toks) 

---------------------------------------------

elsepart :: [Lexeme] -> Either String ([Lexeme], Stmt String)
elsepart (LEX ELSE _:ts) = stmt ts
elsepart toks = Left $ "Error:In elsepart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting an ELSE got " ++ show (head toks) 
			  
---------------------------------------------

dopart :: [Lexeme] -> Either String ([Lexeme], Stmt String)
dopart (LEX DO _:ts) = stmt ts
dopart toks = Left $ "Error:In dopart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a DO got " ++ show (head toks) 

---------------------------------------------

endpart :: [Lexeme] -> Either String [Lexeme]
endpart (LEX END _:ts) = Right ts
endpart toks = Left $ "Error:In endpart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting an END got " ++ show (head toks) 

---------------------------------------------

stmtlist :: [Stmt String] -> [Lexeme] -> Either String ([Lexeme], [Stmt String])
stmtlist e ts = stmtlist2 e ts

---------------------------------------------

stmtlist2 :: [Stmt String] -> [Lexeme] -> Either String ([Lexeme], [Stmt String])
stmtlist2 e (LEX IF _:ts) = do
	(rem1, e1) <- expr ts
	(rem2, s1, s2) <- thenpart rem1
	rem3 <- semipart rem2
	Right (rem3, If e1 s1 s2 : e)
stmtlist2 (LEX WHILE _:ts) = do
	rem1 <- expr ts
	rem2 <- dopart rem1
	semipart rem2	
stmtlist2 (LEX INPUT _: LEX (ID str) _:ts) = semipart ts	
stmtlist2 (LEX (ID str) _: LEX ASSIGN _:ts) = do
	rem1 <- expr ts
	semipart rem1
stmtlist2 (LEX WRITE _:ts) = do
	rem1 <- expr ts
	semipart rem1
stmtlist2 (LEX BEGIN _:ts) = do
	rem1 <- stmtlist ts
	rem2 <- endpart rem1
	semipart rem2	
stmtlist2 ts = Right ts

---------------------------------------------

semipart :: [Lexeme] -> Either String [Lexeme]
semipart (LEX SEMICOLON _:ts) = stmtlist2 ts
semipart toks = Left $ "Error:In semipart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a SEMICOLON got " ++ show (head toks) 

---------------------------------------------

expr :: [Lexeme] -> Either String ([Lexeme], Exp String)
expr ts = do
	(rem, e) <- term ts
	expr2 rem	

---------------------------------------------

expr2 :: [Lexeme] -> Either String ([Lexeme], Exp String)
expr2 (LEX ADD _:ts) = do
	rem <- term ts
	expr2 rem
expr2 (LEX SUB _:ts) = do
	rem <- term ts
	expr2 rem
expr2 ts = Right ts

---------------------------------------------
	
term :: [Lexeme] -> Either String ([Lexeme], Exp String)
term ts = do
	rem <- factor ts
	term2 rem
	
---------------------------------------------

term2 :: [Lexeme] -> Either String ([Lexeme], Exp String)
term2 (LEX MUL _:ts) = do
	rem <- factor ts
	term2 rem
term2 (LEX DIV _:ts) = do
	rem <- factor ts
	term2 rem
term2 ts = Right ts

---------------------------------------------

factor :: [Lexeme] -> Either String ([Lexeme], Exp String)
factor (LEX LPAR _:ts) = do
	(rem1, e) <- expr ts
	rem2 <- rparpart rem1
	Right (rem2, e)
factor (LEX (ID str) _:ts) = Right (ts, Id str)
factor (LEX (NUM i) _:ts) = Right (ts, Num i)
factor (LEX SUB _ : LEX (NUM i) _ : ts) = Right (ts, Neg (Num i))
factor toks = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a NUM got " ++ show (head toks) 

---------------------------------------------
	
rparpart :: [Lexeme] -> Either String [Lexeme]
rparpart (LEX RPAR _:ts) = Right (ts)
rparpart toks = Left $ "Error:In rparpart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a RPAR got " ++ show (head toks) 

---------------------------------------------
{-
main = do
  output <- mlex
  case output of 
    Left lexStr -> putStrLn lexStr
    Right tokList -> do
      let parseRes = prog tokList
      case parseRes of
        Left str -> putStrLn str
        Right [] -> putStrLn ("Parse Successful.\n")
        Right t -> putStrLn ("weird Parse Successful??\n" ++ show t)
	-}

main = do 
	args <- getArgs
	case length args == 0 of
		True  -> do 
			output <- mlex
			case output of 
				Left lexStr -> putStrLn lexStr
				Right tokList -> do
					let parseRes = prog tokList
					case parseRes of
						Left str -> putStrLn str
						Right ([], s) -> do
							putStrLn ("Parse Successful.\n")
							pp s						
						Right (t, _) -> errorMsg t
		False -> do
			output <- mlex2 args
			case output of 
				Left lexStr -> putStrLn lexStr
				Right tokList -> do
					let parseRes = prog tokList
					case parseRes of
						Left str -> putStrLn str
						Right ([], s) -> do
							putStrLn ("Parse Successful.\n")
							pp s
						Right (t, _) -> errorMsg t
errorMsg t = putStrLn ("Parse finished with tokens left?\n" ++ show t)

{-
main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let etok = tokens conts 
            case etok of
               Right tok -> do
                   putStrLn "\n**************************************\n"
                   putStrLn "The List of tokens are as follows.\n"
                   mapM_ (putStrLn.show) tok
               Left msg -> do  
                  putStrLn msg    -}