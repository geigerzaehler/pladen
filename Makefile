# This file contains rules to build the server binary, the client
# javascript code, and the stylesheets. In addition, there are some
# development helpers and a 'deploy' task.


# Paths to executable tools
CSS_PREFIXER = node_modules/.bin/autoprefixer
AMD_BUILD = bin/amd-build

# Directory containing the assets (JS, CSS) that are served by the
# server
ASSETS = assets

.PHONY: web client first all

first: all

all: client server


# DEVELOPMENT AND DEPLOYMENT

.PHONY: deploy
deploy: web
	git push
	ssh webmaster@maboite.org bash < scripts/deploy.sh
	rsync --archive assets webmaster@maboite.org:www/pladen


# Install tools and utilities required for building the application
build-deps: server-deps client-deps

docs:
	cabal haddock --executables


.PHONY: clean
clean:
	cabal clean
	rm -rf $(ASSETS)/*
	rm client/**/*.js
	rm $(CABAL_DEPS)


# Generate tags for project and cabal packages
tags: $(SERVER_SOURCES) codex.tags
	hasktags --ctags $(SERVER_SOURCES)

codex.tags: $(CABAL_DEPS)
	codex update



# BROWSER APPLICATION
#
# Compiles the styles with Sass and the client with TypeScript,
# CoffeeScript and RequireJS. Creates the targets in the assets
# directory.


# Install the tools and vendor libraries that are required to build the
# client
.PHONY: client-deps
client-deps:
	npm install
	bower install
	which sass || gem install sass


client: styles webapp

# TypeScript and CoffeeScript files in the 'client' directory that make
# up the browser application and have to be compiled to JavaScript.
WEBAPP_SOURCES=$(filter-out %.d.ts,\
                 $(shell find client -name '*.coffee' -or -name '*.ts'))

# JavaScript files that are generated from the client sources
WEBAPP_TARGETS:=$(WEBAPP_SOURCES:%.coffee=%.js)
WEBAPP_TARGETS:=$(WEBAPP_TARGETS:%.ts=%.js)

# Compiled client side application for production
WEBAPP = $(ASSETS)/boot.js

# Vendor components that are included in the webapp builds.
BOWER_COMPONENTS = $(shell find bower_components -name .bower.json)

webapp: $(WEBAPP)

# Link the client javascript file into a bundle using RequireJS
$(WEBAPP): $(AMD_BUILD) $(WEBAPP_TARGETS) $(BOWER_COMPONENTS)
	$< client $@


# The development application only contains vendor. RequireJS loads the
# client code from the `client` directory.
WEBAPP_DEV = $(ASSETS)/boot.dev.js

webapp-dev: $(WEBAPP_DEV) $(WEBAPP_TARGETS)

$(WEBAPP_DEV): $(AMD_BUILD) $(BOWER_COMPONENTS)
	$< vendor $@

# Rules

%.js: %.coffee
	coffee --compile $<

%.js: %.ts
	@echo -n tsc $< ...
	@tsc --module amd --target ES5 $<
	@echo ok


# Built the main stylesheet with SASS
STYLE = $(ASSETS)/style.css
STYLE_MAIN_SOURCE = styles/main.scss
STYLE_INCLUDES = $(shell find styles -type f) $(ICOMOON_STYLE)

styles: $(STYLE) icomoon

$(STYLE): $(STYLE_MAIN_SOURCE) $(STYLE_INCLUDES)
	scss --load-path styles \
		   --require ./styles/functions.rb \
			 --style compressed \
			 $< $@
	$(CSS_PREFIXER) $@

# IcoMoon fonts and styles will be added to the assets
ICOMOON_EXT = svg woff ttf eot
ICOMOON_FONTS = $(ICOMOON_EXT:%=static/assets/fonts/icomoon.%)
ICOMOON_STYLE = styles/icons.scss

icomoon: $(ICOMOON_FONTS) $(ICOMOON_STYLE)
$(ICOMOON_FONTS): static/assets/%: icomoon/%
	cp $< $@
$(ICOMOON_STYLE): icomoon/style.css
	cp $< $@



# SERVER
#
# Uses cabal to build the server executable. Also makes sure that all
# cabal dependencies are installed in the sandbox.

# The server binary
SERVER_BIN = bin/server

# The server binary, built by 'cabal'
CABAL_SERVER_BIN = dist/build/server/server

# Source files the server is built from
SERVER_SOURCES = $(shell find Pladen -name '*.hs')

# Install all dependencies in the cabal package configuration to the
# local sandbox.
CABAL_DEPS = .cabal-sandbox/install-timestamp
CABAL_SANDBOX = cabal.sandbox.config


.PHONY: server
server: $(SERVER_BIN)

$(SERVER_BIN): $(CABAL_SERVER_BIN)
	cp -f $< $@

$(CABAL_SERVER_BIN): $(CABAL_DEPS) $(SERVER_SOURCES)
	cabal configure
	cabal build
	touch $@

.PHONY: server-deps
server-deps: $(CABAL_DEPS)

$(CABAL_DEPS): pladen.cabal $(CABAL_SANDBOX)
	cabal install --dependencies-only
	touch $@

# Create the cabal sandbox
$(CABAL_SANDBOX):
	cabal sandbox init
