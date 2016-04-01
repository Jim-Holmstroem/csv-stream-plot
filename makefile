main: main.hs
	ghc --make -optc-O3 -optc-ffast-math -threaded main.hs

clean:
	rm *.o *.hi

run: main
	./main < /dev/ttyUSB0


setupusb:
	stty -F /dev/ttyUSB0 115200 cs8 cread clocal
