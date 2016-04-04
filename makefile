build:
	stack build

run:
	stack exec csv-stream-plot -- /dev/ttyUSB0

setup:
	stty -F /dev/ttyUSB0 115200 cs8 cread clocal
