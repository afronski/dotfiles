#!/bin/bash
cabal update

cabal install xmonad
cabal install xmonad-contrib --flags="-use_xft"

cabal install xmobar --flags="with_xft with_mpd with_utf8 with_iwlib"

cabal install yeganesh
