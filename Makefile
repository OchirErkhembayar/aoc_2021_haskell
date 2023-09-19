r:
	ghc src/day${d}.hs -o build/day${d}
	rm src/day${d}.hi src/day${d}.o
	./build/day${d}

b:
	ghc src/day${d}.hs -o build/day${d}
	rm src/day${d}.hi src/day${d}.o

f:
	ghc -O2 src/day${d}.hs -o build/day${d}
	rm src/day${d}.hi src/day${d}.o

e:
	./build/day${d}

n:
	touch src/day${d}.hs data/day${d}.txt
