# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# Just to jot down small tests not wanted in the makefile
# and any other commands.

TESTTARGETS=testlina.[0-9] testmina.[0-9] testlinux.[0-9] orang hello.frt tsuite.out

testclean: ; rm -f $(TESTTARGETS)

# WARNING : the generation of postscript and pdf use the same files
# for indices, but with different content.

%.ps:%.dvi  ;
	for i in $(INDICES) ; do texindex  $(@:%.ps=%.$$i) ; done
	 dvips -ta4 $< -o$@
#       dvips -A -r -i       -S10 $< -oA$@
#       dvips -B -i -T 1.8cm,0.0cm -S10 $< -oB$@

%.pdf:%.texinfo  ;
	pdftex $<
	for i in $(INDICES) ; do texindex  $(@:%.pdf=%.$$i) ; done
	pdftex $<
	# Don't leave invalid indices for postscript!
	for i in $(INDICES) ; do rm $(@:%.pdf=%.$$i) ; done

do : ci86.mina.msm
		diff -w ci86.mina.msm orig/FORTH > masm.dif ||true
		more masm.dif

da : ci86.alone.asm
		diff -w ci86.alone.asm cmp > asm.dif ||true
		wc asm.dif

cm :
		cmp ci86.alone.bin cmp2/ci86.alone.bin

did: ci86.mina.msm
		diff -w ci86.mina.msm $(cd)/compare.asm

#ci86.mina.asm : header.m4 mina.m4 nasm.m4 ci86.gnr ; m4 $+ >$@

#copy: $(TARGETS:%=ci86.%.bin) $(TARGETS:%=ci86.%.msm)
copy:
		cp ci86.mina.bin  $(cd)/../test/mina.com
		cp ci86.alone.bin  $(cd)/../test/alone.com
		cp ci86.mina.asm  $(cd)/../test/mina.asm
		cp ci86.alone.asm  $(cd)/../test/alone.asm
		cp ci86.mina.msm  $(cd)/../test/mina.msm
		cp ci86.msdos1.msm  $(cd)/../test/msdos1.msm
		cp ci86.msdos2.msm  $(cd)/../test/msdos2.msm
		cp ci86.msdos3.msm  $(cd)/../test/msdos3.msm
		cp ci86.alone.msm  $(cd)/../test/alone.msm
		cp forth.lab       $(cd)/../test/forth.lab
		cp genboot.bat      $(cd)/../test/genboot.bat

cmp: ci86.mina.bin ci86.alone.bin ciforth lina
		strip lina
		strip ciforth
		cmp ci86.mina.bin  cmp/ci86.mina.bin
		cmp ci86.alone.bin  cmp/ci86.alone.bin
		cmp lina cmp/lina
		cmp ciforth cmp/ciforth

# Not all of these segments are present always.
strip : lina
	strip lina -s -R .bss -R .comment -R .data -R .text

copy1: $(TARGETS:%=ci86.%.bin)
		mount /mnt/dosa
		cp ci86.alone.bin  /mnt/dosa/alone.com
		cp ci86.mina.bin  /mnt/dosa/mina.com
		cp genboot.bat    /mnt/dosa
		cp /mnt/dosc/project/ci86/install/tlink.exe /mnt/dosa
		cp /mnt/dosc/project/ci86/install/tasm.exe  /mnt/dosa
		umount /mnt/dosa

test : ci86.alone.bin   ; cmp $+ cmp/$+

test1: ci86.alone.msm   ; diff -w $+ fortha.asm

ff2 : ci86.linux.o ciforth.c
		gcc -ggdb ciforth.c -c
				ld ci86.linux.o -Tlink.script -r -o ci86.linux2.o
		gcc ciforth.o ci86.linux2.o -o ciforth

x : ; echo $(RELEASECONTENT)
y : ; echo $(RELEASECONTENT) |\
sed -e's/\<ci86\.//g' |\
sed -e's/\<gnr\>/ci86.gnr/' |\
sed -e's/ \([^ .]\{1,8\}\)[^ .]*\./ \1./g'

fina : fina.c ci86.lina.o ; $(CC) $(CFLAGS) $+ -static -Wl,-Tlink.script -o $@

ci86.lina.lis : ci86.lina.mac ;
		as ci86.lina.mac -a=ci86.lina.lis  ;\
		objcopy a.out -O binary

# Done by the separate project now.
# Kept here until releasing the assemblers works out.
# It is unclear where I want the assembler doc's in the end.
RELEASEASSEMBLER=      assembler.txt    ps.frt

msdos32.zip : forth32.asm forth32.com msdos32.txt msdos9.cfg config.sys ; \
    make mslinks ; \
    echo ms$(VERSION) $+ |xargs zip

########################## DOCUMENTATION ########################################
#
# The documentation generates tex, info and html automatically for any version.
# They still go through a common texinfo file, but because of the restrictions
# of info regarding names, some tex-output is spoiled unecessarily.
# In particular forth words with what the c-people find strange characters,
# can not occur in chapter headers: ' : @ ( ) e.a.


# Sort the raw information and add the wordset chapter ends
# A .mig file has its @ duplicated!
%.mig : %.rawdoc ;
	ssort $+ -e '^worddoc[a-z]*($${@},{@}.*\n$$worddoc' -m 1s2s |\
	sed -e 's/@/@@/g' >$@

namescooked.m4 : names.m4 ci86.gnr ; \
	cat names.m4 >$@ ; \
	echo "define({ci86gnrversion}, ifelse(M4_VERSION,,\
{snapshot `grep TITLE ci86.gnr|sed -e 's/.*Revision: //'|\
   sed -e 's/ .*//' `},\
{M4_VERSION}\
))dnl" >>$@

# Make the worddoc macro's into glossary paragraphs to our liking
%.mim : gloss.m4 %.mig ; \
    ( cat $(@:ci86.%.mim=%.cfg) ; m4 $+ )| m4 |\
    sed -e '/Split here for documentation/,$$d' > $@

# Make the worddoc macro's into glossary html items to our liking
ci86.%.html : %.cfg glosshtml.m4 indexhtml.m4 ci86.%.mig namescooked.m4
	cat $(@:%.html=%.mig)|\
	sed -e 's/@@/@/g'               |\
	sed -e s'/worddocsafe/worddoc/g'  |\
	sed -e 's/</\&lt\;/g'   > temp.html
	( \
	    cat indexhtml.m4 ; \
	    ssort temp.html -e '^worddoc[a-z]*($${@},{@}.*\n$$worddoc' -m 2s1s \
	)| m4 > $@
	m4 $(@:ci86.%.html=%.cfg) glosshtml.m4 namescooked.m4 temp.html >> $@
	#rm temp.html

%.info : %.texinfo  ; makeinfo --no-split $< -o $@

# For tex we do not need to use the safe macro's
ci86.%.texinfo : %.cfg $(SRCMI) ci86.%.mim ci86.%.mig manual.m4 wordset.m4 menu.m4 namescooked.m4
	m4 menu.m4 $(@:%.texinfo=%.mig) > menu.texinfo
	m4 wordset.m4 $(@:%.texinfo=%.mim)  $(@:%.texinfo=%.mig) |m4 >wordset.mi
	echo 'define({thisfilename},{$@})' >>namescooked.m4
	echo 'define({thisforth},{$(@:ci86.%.texinfo=%)})'>>namescooked.m4
	( \
	    cat $(@:ci86.%.texinfo=%.cfg) manual.m4 namescooked.m4 ciforth.mi \
	)| tee spy | m4 |\
	sed -e '/Split here for documentation/,$$d' |\
	sed -e 's/thisforth/$(@:ci86.%.texinfo=%)/g' > $@
#        rm wordset.mi menu.texinfo

cifgen.texinfo : cifgen.mi manual.m4 namescooked.m4 lina.cfg
	m4 lina.cfg manual.m4 namescooked.m4 cifgen.mi |\
	sed -e 's/_lbracket_/@{/g'                 |\
	sed -e 's/_rbracket_/@}/g'                 |\
	sed -e 's/_comat_/@@/g'          > $@

TESTLINA= \
test.m4 \
ci86.lina.rawtest \
ci86.lina.labtest

TESTLINA64= \
test.m4 \
ci86.lina64.rawtest \
ci86.lina.labtest

TESTLINUX= \
test.m4 \
ci86.linux.rawtest

# No output expected, except for an official version (VERSION=A.B.C)
# The version number shows up in the diff.
testlina : $(TESTLINA) ci86.lina.rawtest lina forth.lab.lina tsuite.frt ;
	rm forth.lab
	cp forth.lab.lina forth.lab
	m4 $(TESTLINA)  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	lina <$@.1 2>&1| grep -v RCSfile >$@.3
	diff -b -B $@.2 $@.3 || true
	lina -a <tsuite.frt 2>&1 |cat >tsuite.out
	cvs diff -bBw tsuite.out || true
	ln -sf forth.lab.lina  forth.lab
	rm $(TEMPFILE)

# This just generates a test script and testfiles,
# but expects the test to run on a different system.
# The version number shows up in the diff.
testlina64 : $(TESTLINA64) ci86.lina64.rawtest ci86.lina64.s forth.lab.lina tsuite.frt ;
	echo "#!/bin/sh ">$@
	echo "rm -f forth.lab                                 ">>$@
	echo "cp forth.lab.lina forth.lab                  ">>$@
	m4 $(TESTLINA64)  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	echo "lina64 <$@.1 2>&1| grep -v RCSfile >$@.3      ">>$@
	echo "diff -b -B $@.2 $@.3 || true                ">>$@
	echo "lina64 -a <tsuite.frt 2>&1 |cat >tsuite.tmp   ">>$@
	echo "diff tsuite.tmp tsuite64.out || true            ">>$@
	echo "cp forth.lab.lina  forth.lab            ">>$@
	rm $(TEMPFILE)
	tar cf $@.tar lina64 forth.lab.lina tsuite.frt tsuite64.out $@ $@.1 $@.2 ci86.lina64.s

testlinux : $(TESTLINUX) ci86.linux.rawtest ciforthc forth.lab ;
	m4 $(TESTLINUX)  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	ciforthc <$@.1 | grep -v RCSfile >$@.3
	diff -b -B $@.2 $@.3 || true
	rm $(TEMPFILE)

test% : ci86.%.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)

# #################################################################
#       Test optimiser section
# Assumes there is a symbolic link from the official ciasdis archive
# to RCS-as

ANASRC=                 \
analyserconfig.frt      \
analyseras.frt          \
analysermain.frt        \
analyserdebug.frt       \
# That's all folks!

asgen.frt : RCS-as/asgen.frt,v ; co $<
asi386.frt : RCS-as/asi386.frt,v ; co $<

# A Forth with the analyser built-in.
lina-ana : lina asgen.frt asi386.frt #(ANASRC)
	echo '"analyser.frt" INCLUDED   REQUIRE SAVE-SYSTEM  "$@" SAVE-SYSTEM' |lina

# Test the optimiser. FIXME! Gives ERROR 22 ( --> not called from block).
#testoptimiser.out : testoptimiser.frt optimiser.frt lina-ana
#        echo 'INCLUDE optimiser.frt INCLUDE $<' |lina-ana > $@
#        cvs diff -bBw $@ || true

lina-opt : optimiser.frt lina-ana
	echo 'INCLUDE optimiser.frt "$@" SAVE-SYSTEM' |lina-ana
	echo include optimiser.frt from lina-opt manually!
