HS_SOURCES = $(wildcard *.hs)
CT_SOURCES = $(wildcard *.ct)
CTX_SOURCES = $(wildcard *.ctx)

.PHONY: build
build: $(HS_SOURCES) $(CT_SOURCES)
	stack build --pedantic

.PHONY: test
test: hs-test webdocs-test webdocs-test-integ

.PHONY: hs-test
hs-test: $(HS_SOURCES) $(CT_SOURCES)
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
	find . \( -path ./.claude -o -path ./test/Integration/golden/convert \) -prune -o -name "*.ct" -exec catln fmt {} \;
	find . \( -path ./.claude -o -path ./test/Integration/golden/convert \) -prune -o -name "*.ctx" -exec catln fmt {} \;

.PHONY: webdocs-test
webdocs-test:
	cd webdocs && npm run test:run

.PHONY: webdocs-test-integ
webdocs-test-integ:
	cd webdocs && npm run test:integration:full

# test/Integration/code is not served. It was left out (8162d29d) because a
# component was built against every page the service held, so the standalone
# fixtures collided on names they never import from each other (abs, Nothing,
# main). Since adb73260 a component is built against only what it reaches, and
# the integration suite builds every fixture in one service, but serving them
# from the doc server has not been tried again since.
.PHONY: webdocs-serve
webdocs-serve:
	cd webdocs && npm run build
	stack run catln -- doc stack docs

.PHONY: errRepl
errRepl:
	stack ghci --profile catln --ghci-options "-fexternal-interpreter -prof" --test
