clean-hs:
	cabal clean
	rm *.hi *.o tests/*/*.nano

clean: clean-hs

commit:
	git add . && git commit -m "$(m)" && git push