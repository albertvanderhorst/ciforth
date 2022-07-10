#!/bin/bash
# This script has been successfully used to regenerate a lina-5.3.0
# after the fact in behalf of Debian.
export VERSION=$1
s=ci86.lina
l=lina-$VERSION

export EXAMPLESRC="\
mywc32      \
mywc64      \
hellow.frt \
wc.script"

export RELEASELINA="\
COPYING   \
READMElina.txt \
$s.info \
$s.html \
$s.pdf \
$s.ps \
$s.texinfo \
$s.fas    \
lina.1    \
forth.lab     \
$EXAMPLESRC"

make clean
rm -f forth.lab.lina
rm -rf $l*

make VERSION VERSION=$VERSION
make forth.lab.lina
make $s.fas

fasm $s.fas -m 256000
mv -f $s lina

make $RELEASELINA
mkdir $l
cp  $RELEASELINA lina $l
cp lina.mak $l/Makefile
tar -czvf $l.tar.gz $l
rm -rf $l
