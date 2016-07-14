REM Compiling and linking with lf95
lf95 ISC2PUF.FOR -o0 -c
lf95 PCCODE.FOR -o0 -c
lf95 SETUP.FOR -o0 -c
lf95 COSET.FOR -o0 -c
lf95 SOSET.FOR -o0 -c
lf95 RESET.FOR -o0 -c
lf95 MESET.FOR -o0 -c
lf95 TGSET.FOR -o0 -c
lf95 OUSET.FOR -o0 -c
lf95 METEXT.FOR -o0 -c
lf95 conv2cal.for -o0 -c
lf95 @isc2puf.LRF
