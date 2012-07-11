#!/bin/sh

ghc --make -i. -XFlexibleContexts -XQuasiQuotes -XTemplateHaskell -XOverloadedStrings -XKindSignatures -XTypeFamilies -XGADTs ./data/loadData.hs

