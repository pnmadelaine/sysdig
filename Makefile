all:
	cabal build
	ln -s dist/build/simulator/simulator
	ln -s dist/build/generator/generator
	ln -s dist/build/optimizer/optimizer
	ln -s dist/build/compiler/compiler
	ln -s dist/build/assembly/assembly

clean:
	cabal clean
	rm simulator generator optimizer compiler assembly
