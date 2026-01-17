# Directory where the build output should be written to
DIST ?= dist
# Change this to "development" for source maps and non-minifyed code
NODE_ENV ?= production
# If true, we don't attempt to fetch dependencies via npm
OFFLINE ?= false

ifeq ($(NODE_ENV),production)
ESBUILDFLAGS  = --minify
else
ESBUILDFLAGS  = --sourcemap
endif

ESBUILDFLAGS += --platform=browser --bundle
ESBUILDFLAGS += --target=es6,edge79,firefox54,chrome51,safari11

ESBUILD ?= esbuild
ESBUILD += $(ESBUILDFLAGS)

INSTALL ?= install
INSTALL += -m644

all: dist/main.js dist/index.html dist/custom.css dist/vis-network.min.css

$(DIST):
	mkdir -p $@

node_modules:
	$(OFFLINE) || npm install --production --omit=dev

$(DIST)/vis-network.min.css: node_modules $(DIST)
	$(ESBUILD) node_modules/vis-network/dist/dist/vis-network.min.css --outfile=$@

$(DIST)/%.js: source/%.js $(DIST) node_modules
	$(ESBUILD) $< --outfile=$@

$(DIST)/%.css: source/%.css $(DIST)
	$(ESBUILD) $< --outfile=$@

$(DIST)/%: source/% $(DIST)
	$(INSTALL) $< $@

clean:
	rm -rf $(DIST)
	rm -rf node_modules

.PHONY: clean node_modules
