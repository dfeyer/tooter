SHELL := /bin/bash

.PHONY: elm watch minify

default: all

NPM_PATH := ./node_modules/.bin
SRC_DIR := ./src
DIST_DIR := ./dist

export PATH := $(NPM_PATH):$(PATH)

all: elm scss

assets:
		@echo "Copy assets..."
		@cp -r ${SRC_DIR}/assets/* ${DIST_DIR}
		@cp -r ${SRC_DIR}/index.html ${DIST_DIR}

build: clean assets elmoptimized minify

test:
		@elm-test

repl:
		@elm repl

format:
		@elm-format --yes src

clean:
		@rm -Rf ${DIST_DIR}/*

deps:
		@npm install
		@elm-package install --yes

distclean: clean
		@rm -Rf elm-stuff
		@rm -Rf node_modules

elm:
		@elm make --debug ${SRC_DIR}/Main.elm --output ${DIST_DIR}/main.js

elmoptimized:
		@elm make --optimize ${SRC_DIR}/Main.elm --output ${DIST_DIR}/main.js

help:
		@echo "Run: make <target> where <target> is one of the following:"
		@echo "  all                    Compile all Elm files"
		@echo "  clean                  Remove 'dist' folder"
		@echo "  deps                   Install build dependencies"
		@echo "  distclean              Remove build dependencies"
		@echo "  help                   Magic"
		@echo "  watch                  Run 'make all' on Elm file change"

livereload:
		@livereload ${DIST_DIR} -e 'js, css'

minify:
		@npx uglify-js ${DIST_DIR}/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglify-js --mangle --output=${DIST_DIR}/main.js\

serve:
		serve --single --listen 5001 ./dist

watch:
		make livereload & \
		find ${SRC_DIR} -name '*.elm' | entr make elm

release:
		@git checkout gh-pages
		@git merge --no-ff -m "RELEASE: Upmerge master" master
		@make build
		@git add dist/*
		@git commit -m "RELEASE: Build"
		@git subtree push --prefix dist origin gh-pages
		@git checkout master