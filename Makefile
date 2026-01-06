clean-hs:
	cabal clean
	rm *.hi *.o tests/*/*.nano

clean: clean-hs

commit:
	./test.bash && git add . && git commit -m "$(m)" && git push