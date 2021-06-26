LOC = ../cpp
#OBJS = main.o toks.o engine.o prog.o
OBJS = toks.o engine.o prog.o

#hhprolog.html: $(OBJS)
#	emcc -o $@ $^

mini.html: mini.o $(OBJS)
	emcc -O2 -o $@ $^ --emrun --preload-file tests/mini.pl.nl

queens10.html: queens10.o $(OBJS)
	emcc -O2 -o $@ $^ --emrun --preload-file tests/queens10.pl.nl

#main.o: $(LOC)/main.cpp
#	emcc -c -I$(LOC) $^

mini.o: mini.cpp
	emcc -O2 -c -I$(LOC) $^

queens10.o: queens10.cpp
	emcc -O2 -c -I$(LOC) $^



toks.o: $(LOC)/toks.cpp
	emcc -O2 -c -I$(LOC) $^

engine.o: $(LOC)/engine.cpp
	emcc -O2 -c -I$(LOC) $^

prog.o: $(LOC)/prog.cpp
	emcc -O2 -c -I$(LOC) $^

clean:
	rm *.o *.wasm *.js *.html *.data
