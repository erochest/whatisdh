#!/bin/sh

ghc --make -i. -idata -XFlexibleContexts -XQuasiQuotes -XTemplateHaskell -XOverloadedStrings -XKindSignatures -XTypeFamilies -XGADTs -Wall "$@"

