#!/bin/sh --debug
# Not tried out after moving the zip building to the end
# To be added TOBLOCK.FRT TOBLOCK.EXE
m=wina
mm=wna
s=ci86.$m
r=release
echo $m $s
RELEASEBIN="     \
$m.exe           \
$m.pdf           \
forth.lab        \
" # That's all folks!

RELEASETXT="      \
COPYING           \
READwina.txt     \
$m.fas            \
$m.texinfo        \
$m.html           \
$m.ps             \
toblk.frt         \
fromblk.frt     \
" # That's all folks!

version=`echo $1 | sed -e 's/d/./g'`
echo $RELEASE

make VERSION=$version clean

make forth.lab.wina
ln -sf forth.lab.wina forth.lab
make VERSION=$version $s.fas
make test$m
mv $s.fas $m.fas
# make VERSION=$version $s.bin   # Has to wait for fasm
# mv $s.bin $m.exe
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
