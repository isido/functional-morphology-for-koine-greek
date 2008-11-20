GHCOPT    = -ffi -O -fvia-C -funbox-strict-fields -fwarn-incomplete-patterns

OFILES=rts.o trie_lib.o

.PHONY: rts.o trie_lib.o haddock all sw osw ita greek rus spa tools

all:
	make greek
rts.o: 
	gcc -c -O3 ./lib/rts.c
trie_lib.o:
	gcc -c -O3 ./lib/trie_lib.c
greek: $(OFILES)
	ghc $(GHCOPT) $(FMLIB) -i./lib -i./greek --make ./greek/Main.hs  $(OFILES) -o morpho_greek
	strip morpho_greek
.PHONY : clean
clean:
	rm -f ./lib/*.o ./lib/*.hi *~ ./lib/*~ ./greek/*.o ./greek/*.hi ./greek/*~ *~ *.o morpho_greek 
