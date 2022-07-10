#!/bin/sh
VERSION=$1
FORTH=lina

# Combine source files and documentation in the current directory
# into a valid upstream source package for Debian.
# No reference is made to branches or versions, just the version
# that is checkedout into the current directory.

# Only files that are genuinely source in the revision are copied

# For a buildable source add debian.mak and extract.mak.

ln -sf lina32.cfg $FORTH.cfg

rm -rf namescooked.m4
make clean VERSION=$VERSION namescooked.m4 VERSION
echo $FORTH-$VERSION/VERSION                    >MANIFEST


echo $FORTH-$VERSION/COPYING                    >>MANIFEST
echo $FORTH-$VERSION/READMElina.txt             >>MANIFEST
echo $FORTH-$VERSION/debian.mak                 >>MANIFEST

for i in texinfo html pdf ps
do
    make ci86.$FORTH.$i
    echo $FORTH-$VERSION/ci86.$FORTH.$i           >>MANIFEST
done

echo $FORTH-$VERSION/wc.script   >>MANIFEST
echo $FORTH-$VERSION/mywc64      >>MANIFEST
echo $FORTH-$VERSION/mywc32      >>MANIFEST
echo $FORTH-$VERSION/lina.1      >>MANIFEST
echo $FORTH-$VERSION/hellow.frt  >>MANIFEST
make forth.lab.lina
echo $FORTH-$VERSION/forth.lab   >>MANIFEST

rm -rf extract
mkdir extract
echo $FORTH-$VERSION/extract >>MANIFEST

cp            VERSION            extract
cp            ci86.gnr           extract
cp            constant.m4     extract      || true
cp            constant_32.m4     extract   || true
cp            constant_64.m4     extract
cp            header.m4          extract
cp            ciforth.m4         extract
cp            width16.m4         extract
cp            width32.m4         extract
cp            width64.m4         extract

cp            fasm.m4            extract
cp            gas.m4             extract
cp            nasm.m4            extract
cp            masm.m4            extract

cp            lina32.cfg         extract
cp            lina64.cfg         extract

cp            namescooked.m4     extract
cp            postlude.m4        extract
cp            prelude.m4         extract
cp            protect.m4         extract

ln -sf . $FORTH-$VERSION
tar -czvf $FORTH-$VERSION.tar.gz `cat MANIFEST`
rm -r $FORTH-$VERSION extract
