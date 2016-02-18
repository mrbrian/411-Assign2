all: A2Lexer1 A2Lexer2 A2Recogniser

A2Lexer1: A2Lexer.x
	alex A2Lexer.x

A2Lexer2: A2Lexer1
	ghc --make A2Lexer.hs

A2Recogniser: A2Lexer2
	ghc --make A2Recogniser.hs	
	
clean: 
	rm -f A2Recogniser A2Lexer.hs *.hi *.o

