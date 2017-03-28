#!/bin/sh -s
# Copyright (2017): Albert van der Horst {by GNU Public License}
# From an official distribution in LINADIR
# derive an official debian distribution
#LINADIR=$1      \ lina32-5.3.0
LINADIR=lina32-${1-5.3.0}

mkdir debiantmp | true

mkdir debiantmp/usr | true

mkdir debiantmp/usr/bin | true
cp $LINADIR/lina32 debiantmp/lina

mkdir debiantmp/usr/lib | true
cp $LINADIR/forth.lab debiantmp/forth.lab

chroot debiantmp ./lina -i /usr/bin/lina /usr/lib/forth.lab /bin/bash
rm debiantmp/lina debiantmp/forth.lab

mkdir debiantmp/usr/share | true

mkdir debiantmp/usr/share/info | true
cp $LINADIR/ci86.lina32.info debiantmp/usr/share/info/lina.info

mkdir debiantmp/usr/share/doc | true
mkdir debiantmp/usr/share/doc-base | true
cat <<% >debiantmp/usr/share/doc-base/lina-manual
Document: lina-manual
Title: lina Manual
Author: Albert van der Horst
Abstract: Documentation for lina, the linux 32 bit version of ciforth
 ciforth is a simple Forth interpreter and compiler targeting the
 Intel x86 series of processors.
Section: Programming

Format: HTML
Index: /usr/share/doc/lina/html/lina.html
Files: /usr/share/doc/lina/html/lina*.html

Format: info
Index: /usr/share/info/lina.info
Files: /usr/share/info/lina.info*

Format: pdf
Files: /usr/share/doc/lina/lina.pdf
%
mkdir debiantmp/usr/share/doc/lina | true
cp $LINADIR/READMElina.txt debiantmp/usr/share/doc/lina/README
cp $LINADIR/ci86.lina32.pdf     debiantmp/usr/share/doc/lina/lina.pdf
cp $LINADIR/ci86.lina32.ps      debiantmp/usr/share/doc/lina/lina.ps
#cp $LINADIR/copyright          debiantmp/usr/share/doc/lina
cp copyright                    debiantmp/usr/share/doc/lina

cat <<% >debiantmp/usr/share/doc/lina/AUTHORS
Albert van der Horst   albert@spenarnc.xs4all.nl
%

cat <<% >debiantmp/usr/share/doc/lina/changelog
Before 2017 jan:
See the changelog of ci86.gnr in the project
    https://github.com/albertvanderhorst/ciforth
Even more extensive logging is in the file
    https://github.com/albertvanderhorst/ciforth/logforth.txt
%

cat <<% >debiantmp/usr/share/doc/lina/changelog.Debian
This is in Debian format but not a Debian distribution.
%

mkdir debiantmp/usr/share/doc/lina/examples | true
for i in hellow.frt mywc32 mywc64 wc.script
do
   cp $LINADIR/$i debiantmp/usr/share/doc/lina/examples
done

mkdir debiantmp/usr/share/doc/lina/html | true
cp $LINADIR/ci86.lina32.html debiantmp/usr/share/doc/lina/html/lina.html

mkdir debiantmp/usr/share/src | true
mkdir debiantmp/usr/share/src/lina | true
cp $LINADIR/ci86.lina32.s debiantmp/usr/share/src/lina/lina.s
cp $LINADIR/forth.lab debiantmp/usr/share/src/lina/forth.lab
cat <<% | sed -e 's/^ /\t/' >debiantmp/usr/share/src/lina/Makefile
# The following code is for a 64 bit system.
lina : lina.s
 as --32 lina.s -o lina.o
 ld lina.o -melf_i386 -N -o lina

install : lina forth.lab
 ./lina -g 64 ./lina+    # Example: 64 --> 128 Megabyte
 ./lina+ -i /usr/bin/lina /usr/lib/forth.lab /bin/bash
%

mkdir debiantmp/usr/share/man | true
mkdir debiantmp/usr/share/man/man1 | true
cp $LINADIR/lina.1 debiantmp/usr/share/man/man1

#debiantmp/usr/share/doc/nasm:
#oAUTHORS
#oTODO.gz
#ochangelog.Debian.gz
#ochangelog.gz
#onasmdoc.txt.gz


mkdir debiantmp/DEBIAN | true
cat <<% > debiantmp/DEBIAN/control
Package: lina
Version: 5.3.0
Architecture: i386
Maintainer: tbs
Installed-Size: 342
Depends:
Section: devel
Priority: optional
Homepage: https://github.com/albertvanderhorst/ciforth
Description: ISO-compliant Forth interpreter and compiler
 Computer Intelligence Forth.
 .
 Forth is a tool for evolutionary programming.
 lina is an assembler based ("native") version of Forth (ciforth),
 an interpret environment and compiler for Intel Forth's. It is
 compliant with the ISO Forth standard; the CORE wordset is fully
 implemented. The small, classic, indirect threaded kernel contains
 the essential, i.a. file access and exceptions. Its power is
 multiplied by an extensive source library, that add such as a decompiler
 and integrated 386 assembler. Unlike most Forth's, it generates ELF
 binaries.
 .
 lina is released under the GNU Lesser General Public License (LGPL).
%
(cd debiantmp ; find usr -type f| xargs md5sum >DEBIAN/md5sums )
# FIXME! It is unclear whether this must be done.
echo 2.0 >debiantmp/DEBIAN/debian-binary

# We could do this.
#(cd debiantmp ; tar cf data.tar usr ; xz -z data.tar ; rm -rf usr )

# We could do this.
#(cd debiantmp ; tar cfz control.tar.gz control md5sums ; rm control md5sums )

# The crux. All files must be spelled out for ``ar''
dpkg-deb --build debiantmp $LINADIR

rm -rf debiantmp    # move to the front if debugging
