Goat: Goat.hs GoatAST.hs GoatParser.hs PrettyPrinter.hs GoatSymTable.hs GoatCodegen.hs
	ghc Goat.hs

clean: 
	rm -f *.o *.hi Goat
