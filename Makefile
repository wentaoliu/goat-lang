GoatParser: GoatParser.hs GoatAST.hs GoatPrinter.hs
	ghc GoatParser.hs

clean:
	rm -f *.o *.hi
	rm -f GoatParser 

