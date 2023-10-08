r:
	cabal build
	cabal run

make run:
	cabal clean
	cabal build
	cabal run

b:
	cabal build

n: 
	touch app/Day${d}.hs data/day${d}.txt
