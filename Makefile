all: compile

compile:
	cd src; \
	ghc Main.hs -Wall -o ../flp22-fun -hidir ../build -odir ../build

zip:
	zip -r flp-fun-xkosin09.zip Makefile src/* doc/* test/*

clean:
	rm -r build
	rm flp22-fun