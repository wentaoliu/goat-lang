Goat: Goat.hs GoatAST.hs GoatParser.hs PrettyPrinter.hs GoatSymTable.hs GoatCodegen.hs
	ghc Goat.hs

.PHONY: oz
oz:
	$(MAKE) -C ./oz

clean: 
	rm -f *.o *.hi Goat
	$(MAKE) -C ./oz clobber
