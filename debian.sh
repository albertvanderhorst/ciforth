#!/bin/sh
# Copyright (2017): Albert van der Horst {by GNU Public License}
# From an official distribution in LINADIR
# derive an official debian distribution
VERSION=$1
FORTH=$2

rm -rf ci86.$FORTH-$VERSION forth.lab.lina
mkdir -p ci86.$FORTH-$VERSION
make forth.lab.lina
cp `cat MANIFEST` ci86.$FORTH-$VERSION
rm ci86.$FORTH-$VERSION/$FORTH
cp $FORTH.mak  ci86.$FORTH-$VERSION/Makefile
cd ci86.$FORTH-$VERSION
rename -e 's/ci86.//' *
cd ..
tar -czvf ci86.$FORTH-$VERSION.tar.gz ci86.$FORTH-$VERSION
#rm -rf ci86.$FORTH-$VERSION MANIFEST
