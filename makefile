all: a1

lexerhs: lexer.x
	alex lexer.x

lexer: lexerhs
	ghc --make lexer.hs

rdp: rdp_recogniser_V1.hs
	ghc --make rdp_recogniser_V1.hs
	
main: lexer rdp
	ghc -o assign2 lexer.o rdp_recogniser_V1.o
	
clean: 
	rm -f assign1.hs assign1.hi assign1.o
