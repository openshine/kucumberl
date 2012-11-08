#!/usr/bin/env make

all: build

build:
	./rebar compile escriptize

clean:
	./rebar clean

test:
	./kucumberl -s fail.feature
