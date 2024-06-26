build:
	stack build --pedantic

test:
	stack test --pedantic

format:
	find app -name "*.hs" | xargs stylish-haskell -i
	find src -name "*.hs" | xargs stylish-haskell -i
	find test -name "*.hs" | xargs stylish-haskell -i

ctformat:
	stack install
	find stack -name "*.ct" -exec catln fmt {} \;
	find test/Integration -name "*.ct" -exec catln fmt {} \;

ctxformat:
	stack install
	catln fmt test/code
	catln fmt stack
