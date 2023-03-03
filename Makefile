all: compile

compile:
	cd src; \
	ghc Main.hs -o ../flp22-fun -hidir ../build -odir ../build 

clean:
	rm -r build
	rm flp22-fun