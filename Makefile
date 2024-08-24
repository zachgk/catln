HS_SOURCES = $(wildcard *.hs)
CT_SOURCES = $(wildcard *.ct)
CTX_SOURCES = $(wildcard *.ctx)

.PHONY: build
build: $(HS_SOURCES) $(CT_SOURCES)
	stack build --pedantic

.PHONY: test
test: $(HS_SOURCES) $(CT_SOURCES)
	stack test --pedantic

.PHONY: install
install: $(HS_SOURCES) $(CT_SOURCES)
	stack install

.PHONY: docker
docker:
	docker build .

.PHONY: format
format: $(HS_SOURCES)
	find app -name "*.hs" | xargs stylish-haskell -i
	find src -name "*.hs" | xargs stylish-haskell -i
	find test -name "*.hs" | xargs stylish-haskell -i

.PHONY: ctformat
ctformat: $(HS_SOURCES) $(CT_SOURCES) $(CTX_SOURCES) install
	find . -name "*.ct" -exec catln fmt {} \;
	find . -name "*.ctx" -exec catln fmt {} \;

.PHONY: errRepl
errRepl:
	stack ghci --profile catln --ghci-options "-fexternal-interpreter -prof" --test
