# Makefile for windows using MinGW/make and Cygwin/mv
Goat: GoatParser.hs GoatAST.hs GoatPrinter.hs
	stack exec ghc GoatParser.hs
	mv GoatParser.exe Goat.exe
clean:
	rm -f Goat*.o Goat*.hi

