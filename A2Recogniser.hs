--
-- CPSC411 Assignment2
-- Brian Yee
-- 00993104
-- T02
-- 

module Main where
import A2Lexer
import System.Environment
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty


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
	semipart (e++[If e1 s1 s2]) rem2
stmtlist2 e (LEX WHILE _:ts) = do
	(rem1, e1) <- expr ts
	(rem2, s1) <- dopart rem1
	semipart (e++[While e1 s1]) rem2	
stmtlist2 e (LEX INPUT _: LEX (ID str) _:ts) = semipart (e++[Input (Id str)]) ts	
stmtlist2 e (LEX (ID str) _: LEX ASSIGN _:ts) = do
	(rem1, e1) <- expr ts
	semipart (e++[Assign str e1]) rem1
stmtlist2 e (LEX WRITE _:ts) = do
	(rem1, e1) <- expr ts
	semipart (e++[Print e1]) rem1
stmtlist2 e (LEX BEGIN _:ts) = do
	(rem1, e1) <- stmtlist e ts
	rem2 <- endpart rem1
	semipart (e1++e) rem2	
stmtlist2 e ts = Right (ts, e)

---------------------------------------------

semipart :: [Stmt String] -> [Lexeme] -> Either String ([Lexeme], [Stmt String])
semipart es (LEX SEMICOLON _:ts) = stmtlist2 es ts
semipart es toks = Left $ "Error:In semipart: Couldn't parse\n" ++ show toks
              ++ "\nExpecting a SEMICOLON got " ++ show (head toks) 

---------------------------------------------

expr :: [Lexeme] -> Either String ([Lexeme], Exp String)
expr ts = do
	(rem1, e1) <- term ts
	expr2 e1 rem1	

---------------------------------------------

expr2 :: Exp String -> [Lexeme] -> Either String ([Lexeme], Exp String)
expr2 e (LEX ADD _:ts) = do
	(rem1, e1) <- term ts
	expr2 (Add e e1) rem1	
expr2 e (LEX SUB _:ts) = do
	(rem1, e1) <- term ts
	expr2 (Add e (Neg e1)) rem1
expr2 e ts = Right (ts, e)

---------------------------------------------
	
term :: [Lexeme] -> Either String ([Lexeme], Exp String)
term ts = do
	(rem, e) <- factor ts
	term2 e rem
	
---------------------------------------------

term2 :: Exp String -> [Lexeme] -> Either String ([Lexeme], Exp String)
term2 es (LEX MUL _:ts) = do
	(rem1, e1) <- factor ts
	term2 (Mul es e1) rem1
term2 es (LEX DIV _:ts) = do
	(rem1, e1) <- factor ts
	term2 (Div es e1) rem1
term2 es ts = Right (ts, es)

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
 
instance (Out a) => Out (Stmt a) where
  doc (If a b c) = parens $ text "If" $$ nest 2 (doc a) 
                                                    $$ nest 2 (doc b)
                                                    $$ nest 2 (doc c)
  doc (While a b) = parens $ text "While" $$ nest 2 (doc a) 
                                                    $$ nest 2 (doc b)                                                  
  doc (Assign a b) = parens $ text "Assign" $$ nest 2 (doc a)
                                                    $$ nest 2 (doc b)
  doc (Block a) = parens $ text "Block" $$ nest 2 (doc a)
  doc (Print a) = parens $ text "Print" $$ nest 2 (doc a)
  doc (Input a) = parens $ text "Input" $$ nest 2 (doc a)

  docPrec _ = doc

---------------------------------------------
  
instance (Out a) => Out (Exp a) where
  doc (Add a b) = parens $ text "Add" $$ nest 2 (doc a) 
                                                    $$ nest 2 (doc b)
  doc (Mul a b) = parens $ text "Mul" $$ nest 2 (doc a) 
                                                    $$ nest 2 (doc b)
  doc (Div a b) = parens $ text "Div" $$ nest 2 (doc a) 
                                                    $$ nest 2 (doc b)
  doc (Neg a) = parens $ text "Neg" <+> doc a
  doc (Id a) = parens $ text "Id" <+> doc a
  doc (Num a) = parens $ text "Num" <+> doc a

  docPrec _ = doc  

{----------------------------------------------------------------------------------------------  
	showExp - translates an Exp to a machine code string 
-----------------------------------------------------------------------------------------------}

showExp ::  Exp String -> String
showExp (Id s) = "\trPUSH "++ s ++"\n"
showExp (Add a b) = (showExp a)++(showExp b)++"\tOP2 +\n"
showExp (Mul a b) = (showExp a)++(showExp b)++"\tOP2 *\n"
showExp (Div a b) = (showExp a)++(showExp b)++"\tOP2 /\n"
showExp (Neg a) = (showExp a)++"\tOP1 -\n"
showExp (Num i) = "\tcPUSH "++(show i)++"\n"

{----------------------------------------------------------------------------------------------  
	showStmt - translates an Stmt to a machine code string 
-----------------------------------------------------------------------------------------------}

showStmt ::  Int  ->  Stmt String -> (Int, String)
showStmt n (If e s1 s2) = ( m, (showExp e) ++"\tcJUMP L"++(show n)++"\n"
						 ++ code1
						 ++"\tJUMP L"++(show (n+1))++"\n"
						 ++"L"++(show n)++":\n"
						 ++code2
						 ++"L"++(show (n+1))++":\n")   
	  where
	  (n',code1) = showStmt (n+2) s1
	  (m, code2) = showStmt n' s2
showStmt n (While e s) = (n', "L"++(show n)++":\n"
						 ++ (showExp e)
						 ++ "\tcJUMP L"++(show (n+1))++"\n"
						 ++ code
						 ++ "\tJUMP L" ++ (show n)++ "\n"
						 ++ "L"++ (show (n+1)) ++ ":\n" )  
      where
	  (n', code) = showStmt (n+2) s
showStmt n (Block []) = (n, "")
showStmt n (Block (s:rem)) = (n2, str1 ++ str2)
	  where 
	  (n1, str1) = showStmt n s
	  (n2, str2) = showStmt n1 (Block rem)
showStmt n (Assign s e) = (n, (showExp e)++"\tLOAD " ++ s ++ "\n" )
showStmt n (Input (Id s)) = ( n, "\tREAD " ++ s ++ "\n" )   
showStmt n (Print e) = ( n, (showExp e)++"\tPRINT\n" )   
	   
  
main = do 
	args <- getArgs
	case length args == 2 of
		False  -> do 
			let usage = "\nExpecting of the form < ./A2Recogniser inputfile outputfile >.\n\nTry again. :(\n"
			error $ "\n****************Error: Expecting input and output filenames as arguments." ++ usage
		True -> do
			output <- mlex args
			let fname  = args !! 1 
			case output of 
				Left lexStr -> putStrLn lexStr
				Right tokList -> do
					let parseRes = prog tokList
					case parseRes of
						Left str -> putStrLn str
						Right ([], ast) -> do
							putStrLn ("Parse Successful.\n")
							putStrLn $ "AST is :\n" 
							pp ast
							let (n, outStr) = showStmt 1 ast							
							putStrLn ("Exporting results to: \""++ fname ++"\".\n")
							writeFile fname outStr
						Right (t, _) -> errorMsg t
errorMsg t = putStrLn ("Parse finished with tokens left?\n" ++ show t)

