LOC = ../cpp
OBJS = toks.o engine.o prog.o

# hardcoded scripts

mini.html: mini.o $(OBJS)
	emcc -O2 -o $@ $^ --emrun --preload-file tests/mini.pl.nl

mini.o: mini.cpp
	emcc -O2 -c -I$(LOC) $^

queens10.html: queens10.o $(OBJS)
	emcc -O2 -o $@ $^ --emrun --preload-file tests/queens10.pl.nl

queens10.o: queens10.cpp
	emcc -O2 -c -I$(LOC) $^

# CLI script selection

hhprolog.html: main.o $(OBJS)
	emcc -O2 -o $@ $^ --emrun --preload-file tests

main.o: $(LOC)/main.cpp
	emcc -O2 -c -I$(LOC) $^

toks.o: $(LOC)/toks.cpp
	emcc -O2 -c -I$(LOC) $^

engine.o: $(LOC)/engine.cpp
	emcc -O2 -c -I$(LOC) $^

prog.o: $(LOC)/prog.cpp
	emcc -O2 -c -I$(LOC) $^

clean:
	rm *.o *.wasm *.js *.html *.data
