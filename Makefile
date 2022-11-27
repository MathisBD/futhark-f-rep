
all: src/python/main.py engine
	python3 src/python/main.py

test: src/futhark/lib
	futhark test --backend=pyopencl src/futhark/tests

engine: src/futhark/*.fut src/futhark/lib
	futhark pyopencl --library src/futhark/dda.fut -o src/python/__engine

src/futhark/lib: src/futhark/futhark.pkg
	cd src/futhark && futhark pkg sync

clean: 
	rm -rf src/python/__pycache__ src/futhark/lib src/python/__engine.py

.PHONY: all clean test