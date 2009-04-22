
RUNHASKELL = runhaskell Setup.lhs

all:	configure build

configure:
	$(RUNHASKELL) configure

build:
	$(RUNHASKELL) build

clean:
	$(RUNHASKELL) clean

run:
	dist/build/monao/monao.exe

doc:
	haddock -h -o man -l C:\\ghc\\haddock-2.0.0.0 -B c:\\ghc\\ghc-6.8.2 *.hs

imgs:
	runghc -itool tool/listup-imgs.hs data/img > Images.hs

count:
	@echo $(SRCS) | xargs -n1 echo | wc | gawk '{print $$1 " files";}'
	@cat $(SRCS) | wc | gawk '{print $$1 " lines";}'
