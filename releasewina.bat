#!/bin/bash --debug

m=$1
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
READMEwina.txt     \
$m.fas            \
$m.texinfo        \
$m.html           \
$m.ps             \
hellow.frt        \
" # That's all folks!

#toblk.frt         \
#fromblk.frt     \
# MAYBE later

echo $RELEASE

make clean
make VERSION VERSION=$2


make forth.lab.wina
ln -sf forth.lab.wina forth.lab
make VERSION=$version $s.fas
make test$m
mv $s.fas $m.fas
fasm $m.fas -m300000
mv $m $m.exe
# make VERSION=$version $s.bin   # Has to wait for fasm
# mv $s.bin $m.exe
# Before texinfo (not sure whether that is really required.)
make VERSION=$version $s.html
mv $s.html $m.html
make VERSION=$version $s.texinfo
mv $s.texinfo $m.texinfo
make $m.ps
make $m.pdf

rm $m-$2.zip
echo $m-$2.zip $RELEASETXT | xargs zip -l
echo $m-$2.zip $RELEASEBIN | xargs zip
