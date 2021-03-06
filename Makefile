packagefile = dist/package.conf.inplace
GHCFLAGS = -package-conf $(packagefile) -Wall
package-config = dist/setup-config
maindir = dist/main-out
main = $(maindir)/main

default:$(main)

all:$(main) launch

launch:$(main)
	sudo openvt -ws -- sh -c './$< ; read -n1 -r'

$(main):main.hs $(packagefile)
	mkdir -p $(maindir)
	ghc $< -outputdir $(maindir) -o $@ $(GHCFLAGS)

ghci:main.hs $(packagefile)
	ghci $< $(GHCFLAGS)

$(packagefile):$(package-config)
	cabal build

$(package-config):
	cabal configure

clean:
	rm -rf $(maindir)

cleanall:clean
	cabal clean

edit:
	@find src -type f -exec emacsclient -n {} +
	@emacsclient -n main.hs

.PHONY:default all launch ghci clean cleanall edit
