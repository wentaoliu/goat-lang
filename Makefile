Goat: GoatParser.hs GoatAST.hs GoatPrinter.hs
	ghc -o Goat GoatParser.hs

clean:
	rm -f Goat*.o Goat*.hi Goat

