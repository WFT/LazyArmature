CENGINE = transform.o objects.o display.o render.o matrix.o lines.o
SDLFLAGS = `sdl2-config --cflags --static-libs`
STDFLAGS = -g -O3

build: $(CENGINE)
	ghc --make Main.hs -lSDL2 $(CENGINE)
transform.o: transform.c transform.h
	gcc transform.c -o transform.o -c $(STDFLAGS)

objects.o: objects.c objects.h
	gcc objects.c -o objects.o -c $(STDFLAGS)

display.o: display.c display.h
	gcc display.c -o display.o -c $(STDFLAGS) $(SDLFLAGS)

render.o: render.c render.h options.h
	gcc render.c -o render.o -c $(STDFLAGS)

matrix.o: matrix.c matrix.h
	gcc matrix.c -o matrix.o -c $(STDFLAGS)

lines.o: lines.c lines.h options.h
	gcc lines.c -o lines.o -c $(STDFLAGS)

clean:
	rm -r *.o *.hi
