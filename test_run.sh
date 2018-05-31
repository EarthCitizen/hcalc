stack test --ghc-options "-threaded" --test-arguments "+RTS -N -RTS --num-threads 10 ${1:+--pattern "$1"}"
