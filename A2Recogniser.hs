module Main where
import System.Environment
import A2Lexer

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
	endpart rem
stmt toks = Left $ "Error:In stmt: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a STMT got " ++ show (head toks) 

---------------------------------------------

thenpart :: [Lexeme] -> Either String [Lexeme]
thenpart (LEX THEN _:ts) = do
	rem <- stmt ts
	elsepart rem
thenpart toks = Left $ "Error:In thenpart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a THEN got " ++ show (head toks) 

---------------------------------------------

elsepart :: [Lexeme] -> Either String [Lexeme]
elsepart (LEX ELSE _:ts) = stmt ts
elsepart toks = Left $ "Error:In elsepart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting an ELSE got " ++ show (head toks) 
			  
---------------------------------------------

dopart :: [Lexeme] -> Either String [Lexeme]
dopart (LEX DO _:ts) = stmt ts
dopart toks = Left $ "Error:In dopart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a DO got " ++ show (head toks) 

---------------------------------------------

endpart :: [Lexeme] -> Either String [Lexeme]
endpart (LEX END _:ts) = Right ts
endpart toks = Left $ "Error:In endpart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting an END got " ++ show (head toks) 

---------------------------------------------

stmtlist :: [Lexeme] -> Either String [Lexeme]
stmtlist ts = stmtlist2 ts

---------------------------------------------

stmtlist2 :: [Lexeme] -> Either String [Lexeme]
stmtlist2 (LEX IF _:ts) = do
	rem1 <- expr ts
	rem2 <- thenpart rem1
	semipart rem2	
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
factor toks = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a NUM got " ++ show (head toks) 

---------------------------------------------
	
rparpart :: [Lexeme] -> Either String [Lexeme]
rparpart (LEX RPAR _:ts) = Right ts
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
						Right [] -> putStrLn ("Parse Successful.\n")
						Right t -> errorMsg t
		False -> do
			output <- mlex2 args
			case output of 
				Left lexStr -> putStrLn lexStr
				Right tokList -> do
					let parseRes = prog tokList
					case parseRes of
						Left str -> putStrLn str
						Right [] -> putStrLn ("Parse Successful.\n")
						Right t -> errorMsg t
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