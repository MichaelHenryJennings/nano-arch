clean-hs:
	cabal clean
	rm *.hi *.o tests/*/*.nano

clean: clean-hs