# Makefile for windows using MinGW/make and Cygwin/mv
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#search-path
Goat:
	stack exec -- ghc --make -o Goat Main.hs -i.:../src/Goat \
	-odir ../bin -hidir ../bin
	# -odir <dir>, -hidir <dir>, -stubdir <dir> and -dumpdir <dir>.

clean:
	rm -f ../bin/Goat*.o ../bin/Goat*.hi

