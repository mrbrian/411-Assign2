
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

showExp ::  Exp String -> String
showExp (Id s) = "rPUSH "++(show s)++"\n"
showExp (Add a b) = (showExp a)++(showExp b)++"\nOP2 +\n"
showExp (Mul a b) = (showExp a)++(showExp b)++"\nOP2 *\n"
showExp (Div a b) = (showExp a)++(showExp b)++"\nOP2 /\n"
showExp (Neg a) = (showExp a)++"\nOP1 -\n"
showExp (Num i) = "cPUSH "++(show i)++"\n"

shower ::  Int  ->  Stmt String -> (Int, String)
shower n (If e s1 s2) = 
	   ( m, (show e) ++"cJUMP label"++(show n)++"\n"
						 ++ code1
						 ++"JUMP label"++(show (n+1))++"\n"
						 ++"label"++(show n)++":\n"
						 ++code2
						 ++"label"++(show (n+1))++":\n"
		 )   where
	  (n',code1) = shower (n+2) s1
	  (m, code2) = shower n' s2
shower n (While e s) = (n', "label"++(show n)++"\n"
						 ++ (showExp e)
						 ++ "cJUMP label"++(show (n+1))++"\n"
						 ++ code
						 ++ "JUMP label" ++ (show n)++ "\n" )  
      where
	    (n', code) = shower (n+2) s
{-

check:
exp
cJUMP out
statement
JUMP check
out:

-}

shower n (Block []) = (n, "")
shower n (Block (s:rem)) = (n2, str1 ++ str2)
	   where 
	   (n1, str1) = shower n s
	   (n2, str2) = shower n1 (Block rem)

shower n (Assign s e) = (n, (showExp e)++"LOAD " ++ s ++ "\n" )
shower n (Input (Id s)) = 
	   ( n, "LOAD " ++ s ++ "\n" )   
shower n (Print e) = 
	   ( n, (showExp e)++"\n"++"PRINT\n" )   
	   
