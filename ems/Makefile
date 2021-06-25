LOC = ../cpp
#OBJS = main.o toks.o engine.o prog.o
OBJS = toks.o engine.o prog.o

#hhprolog.html: $(OBJS)
#	emcc -o $@ $^

mini.html: mini.o $(OBJS)
	emcc -o $@ $^ --emrun --preload-file tests/mini.pl.nl

#main.o: $(LOC)/main.cpp
#	emcc -c -I$(LOC) $^

mini.o: mini.cpp
	emcc -c -I$(LOC) $^

toks.o: $(LOC)/toks.cpp
	emcc -c -I$(LOC) $^

engine.o: $(LOC)/engine.cpp
	emcc -c -I$(LOC) $^

prog.o: $(LOC)/prog.cpp
	emcc -c -I$(LOC) $^

clean:
	rm *.o *.wasm *.js *.html *.data
