MAKEDIR=mkdir -p
MOVE=mv
REMOVE=rm

all:
	$(MAKEDIR) ./bin
	$(MAKEDIR) ./build
	$(MAKE) -C ./src
	$(MOVE) ./src/tigerc ./bin
	$(MOVE) ./src/runtime.o ./bin
	$(MOVE) ./src/*/*.uo ./build
	$(MOVE) ./src/*/*.ui ./build
	$(MOVE) ./src/*.uo ./build
	$(MOVE) ./src/*.ui ./build


clean:
	$(MAKE) -C ./src clean
	$(REMOVE) -rf ./build
	$(REMOVE) -rf ./bin
