.PHONY: all all_linux clean linux

all:        clean | Cr
all_linux:  clean | linux

linux: dosed | Cr

dosed:
	sed -i '/Win32/d' Cr.cabal

Cr:
	cabal-1.18 install --only-dependencies
	cabal-1.18 configure
	cabal-1.18 build

clean:
	@echo " --- Clean binaries --- "
	rm -f Cr
	@echo " --- Clean temp files --- "
	find . -name '*~' -delete;
	find . -name '#*#' -delete;