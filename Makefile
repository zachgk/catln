build:
	stack build --pedantic

format:
	find app -name "*.hs" | xargs stylish-haskell -i
	find src -name "*.hs" | xargs stylish-haskell -i
	find test -name "*.hs" | xargs stylish-haskell -i
