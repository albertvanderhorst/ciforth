#!/bin/sh --debug
m=mina
mm=mna
s=ci86.$m
r=release
echo $m $s
RELEASEBIN="     \
$m.com           \
$m.pdf           \
forth.lab        \
" # That's all folks!

RELEASETXT="      \
COPYING           \
READmina.txt      \
$m.asm            \
$m.texinfo        \
$m.html           \
$m.ps             \
toblk.frt      \
fromblk.frt      \
" # That's all folks!

version=`echo $1 | sed -e 's/d/./g'`
echo $RELEASE

make VERSION=$version clean

make forth.lab.wina
ln -sf forth.lab.wina forth.lab
make VERSION=$version $s.asm
make test$m
mv $s.asm $m.asm
make VERSION=$version $s.bin
mv $s.bin $m.com
# Before texinfo (not sure whether that is really required.)
make VERSION=$version $s.html
mv $s.html $m.html
make VERSION=$version $s.texinfo
mv $s.texinfo $m.texinfo
make $m.ps
make $m.pdf

rm $mm$1.zip
echo $mm$1 $RELEASETXT | xargs zip -k -l
echo $mm$1 $RELEASEBIN | xargs zip -k
