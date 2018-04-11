.PHONY: install build compile-contracts deploy test

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

build:
	pulp build

compile-contracts:
	pulp build --src-path compile -m Compile --to compile.js && node compile.js; rm compile.js

deploy: compile-contracts build
	pulp build --to deploy.js && node deploy.js; rm deploy.js
test:
	pulp test
