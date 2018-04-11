.PHONY: install build compile-contracts deploy test

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

build:
	pulp build

compile-contracts:
	pulp run --src-path compile -m Compile

deploy: compile-contracts build
	pulp run

test:
	pulp test
