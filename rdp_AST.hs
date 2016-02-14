module Main where
import MLexer
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

data Expr a = EAdd (Expr a) (Expr a)
           | EMul (Expr a) (Expr a)
           | ENum Int 
           deriving (Eq,Show,Read)

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
exp1:: [Lexeme] -> Either String ([Lexeme],Expr Int)
exp1 ts = do
    (rem,e) <- term ts  
    more_exp e rem 

---------------------------------------------

more_exp :: Expr Int -> [Lexeme] ->  Either String ([Lexeme],Expr Int)
more_exp e (LEX ADD _:ts)  = do 
        (rem,e') <- term ts
        more_exp (EAdd e e') rem 
more_exp e ts        = Right (ts,e) 
----------------------------------------------

term :: [Lexeme] -> Either String ([Lexeme],Expr Int)
term  ts           = do
       (rem,e) <- factor ts
       more_term e rem 

-----------------------------------------------

more_term :: Expr Int -> [Lexeme] -> Either String ([Lexeme],Expr Int)
more_term e (LEX MUL _:ts) = do 
    (rem,e') <- factor ts
    more_term (EMul e e') rem 
more_term e ts       = Right (ts,e)

-----------------------------------------------

factor :: [Lexeme] ->  Either String ([Lexeme],Expr Int)
factor ((LEX (NUM n) _):ts)  = Right (ts,ENum n)
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 


instance (Out a) => Out (Expr a) where
  doc (ENum a) =  parens $ text "Num" <+> doc a
  doc (EMul a b) = parens $ text "Mul" $$ nest 1 (doc a) 
                                                    $$ nest 1 (doc b)
  doc (EAdd a b) = parens $ text "Add" $$ nest 1 (doc a) 
                                                    $$ nest 1 (doc b)
  docPrec _ = doc
  
main = do
	output <- mlex
	case output of 
		Left lexStr -> putStrLn lexStr
		Right tokList -> do
			let parseRes = exp1 tokList
			case parseRes of
				Left str -> putStrLn str 
				Right ([],ast) -> do
					putStrLn "Parse Successful.\n"
					putStrLn $ "AST is :\n" 
					pp ast

     
    
