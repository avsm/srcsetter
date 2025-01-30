#!/bin/sh
# local opam build for CI for Homebrew taps and so on

export OPAMROOT=`pwd`/_opamroot
export OPAMYES=1
export OPAMCONFIRMLEVEL=unsafe-yes
opam init -ny
opam switch create . 
opam exec -- dune build --profile=release
