all: generated/paper.html tests

include wg21/Makefile
tests: spec.cpp
	g++ "$<" -o "$@" -std=c++2b -Wall

