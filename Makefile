
all: src/main.py src/dda.py
	python3 src/main.py

bench: src/*.fut src/lib
	futhark bench --backend=pyopencl src/dda.fut

src/dda.py: src/*.fut src/lib
	futhark pyopencl --library src/dda.fut

src/lib: src/futhark.pkg
	cd src && futhark pkg sync

clean: 
	rm -rf src/__pycache__ src/lib src/dda.py src/dda

.PHONY: all bench clean