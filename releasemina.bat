#!/bin/sh --debug
# Not tried out after moving the zip building to the end
# To be added TOBLOCK.FRT TOBLOCK.EXE
m=mina
s=ci86.$m
r=release
echo $m $s
RELEASE="         \
COPYING           \
README.$m         \
$m.asm            \
$m.com            \
$m.texinfo        \
$m.html           \
$m.pdf            \
$m.ps             \
toblock.frt      \
$m$r.txt    \
forth.lab"       \
# That's all folks!

version=`echo $1 | sed -e 's/d/./g'`

echo $RELEASE

make VERSION=$version clean

make forth.lab.wina
ln -sf forth.lab.wina forth.lab
make $m.test
make VERSION=$version $s.asm
mv $s.asm $m.asm
make VERSION=$version $s.bin
mv $s.bin $m.com
# Before texinfo (not sure whether that is really required.)
make VERSION=$version $s.html
mv $s.html $m.html
make VERSION=$version $s.texinfo
mv $s.texinfo $m.texinfo
# Make pdf before ps. Seems like minimal risk, but indices
# by pdftex are incorrect.
pdftex $m.texinfo $m.pdf
pdftex $m.texinfo $m.pdf # second time for content
make VERSION=$version $m.ps
echo mna$1 $RELEASE | xargs zip -k
