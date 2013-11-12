.PHONY: all all_linux clean linux

all:        clean | Cr
all_linux:  clean | linux

linux: sed '/Win32/d' Cr.cabal | ctodo

Cr:
	cabal install --only-dependencies
	cabal configure
	cabal build

clean:
	@echo " --- Clean binaries --- "
	rm -f Cr
	@echo " --- Clean temp files --- "
	find . -name '*~' -delete;
	find . -name '#*#' -delete;