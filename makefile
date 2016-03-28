main: main.hs
	ghc --make -threaded main.hs

run: main
	./main < /dev/ttyUSB0


setupusb:
	stty -F /dev/ttyUSB0 115200 cs8 cread clocal
