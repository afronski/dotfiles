#!/bin/bash

pacaur -S ghc cabal-install xmonad xmonad-contrib shellcheck

sudo cp -f xmonad.desktop /usr/share/xsessions/xmonad.desktop

cabal update

cabal install xmobar --flags="with_xft with_mpd with_utf8 with_iwlib" --ghc-option="-dynamic"
cabal install yeganesh --ghc-option="-dynamic"