libs = libdrm libkms
CFLAGS = $(shell pkg-config --cflags $(libs))
LDFLAGS = $(shell pkg-config --libs $(libs))
LANGFLAGS = -XUnicodeSyntax -XImplicitParams
GHCFLAGS = -outputdir out/obj -i -isrc -iout/hs -Wall $(LANGFLAGS)

from_hsc = $(patsubst src/%.hsc,out/hs/%.hs,$(shell find src -name '*.hsc'))

default:build

.PHONY:default all launch clean build ghci

all:build launch

launch:main
	sudo openvt -ws -- ./$<

clean:
	rm -rf out

build:$(from_hsc)
	ghc --make src/main.hs -o main -dynamic $(GHCFLAGS) $(LDFLAGS)

ghci:
	ghci $(GHCFLAGS) $(LDFLAGS)

out/hs/%.hs:src/%.hsc
	mkdir -p $(@D)
	hsc2hs $< -o $@ $(CFLAGS)

