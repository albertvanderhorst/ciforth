# $Id: test.mak,v 5.33 2017/10/28 21:27:28 albert Exp $
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

cmp: ci86.mina.bin ci86.alone.bin ciforth lina32
		strip lina32
		strip ciforth
		cmp ci86.mina.bin  cmp/ci86.mina.bin
		cmp ci86.alone.bin  cmp/ci86.alone.bin
		cmp lina32 cmp/lina32
		cmp ciforth cmp/ciforth

# Not all of these segments are present always.
strip : lina32
	strip lina32 -s -R .comment -R .data

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

# FIXME maybe remove
fina : fina.c ci86.lina.o ; $(CC) $(CFLAGS) $+ -static -Wl,-Tlink.script -o $@

# FIXME maybe remove
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
# In the input of the .mig file all @ are (should be) duplicated!
%.mig : %.rawdoc lina forth.lab.lina ;
	sortworddoc.frt $< $@

namescooked.m4 : names.m4 ci86.gnr ; \
	cat names.m4 >$@ ; \
	echo "define({ci86gnrversion}, ifelse(M4_VERSION,test,\
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
	sortworddoc.frt -r temp.html temp2.html
	echo 'define({thisfilename},{$@})' >>namescooked.m4
	echo 'define({thisforth},{$(@:ci86.%.texinfo=%)})'>>namescooked.m4
	( \
	    cat indexhtml.m4 namescooked.m4 temp2.html \
	)| m4 > $@
	m4 $(@:ci86.%.html=%.cfg) glosshtml.m4 namescooked.m4 temp.html |\
	sed -e 's/~"/"/' -e 's/_left_parenthesis_/\(/' >> $@
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
	sed -e 's/thisforth/$(@:ci86.%.texinfo=%)/g' -e 's/_left_parenthesis_/\(/' > $@
#        rm wordset.mi menu.texinfo

cifgen.texinfo : cifgen.mi manual.m4 namescooked.m4 lina32.cfg
	m4 lina32.cfg manual.m4 namescooked.m4 cifgen.mi |\
	sed -e 's/_lbracket_/@{/g'                 |\
	sed -e 's/_rbracket_/@}/g'               > $@

TESTLINA32= \
test.m4 \
ci86.lina32.rawtest \
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
testlina32 : $(TESTLINA32) ci86.lina32.rawtest lina32 forth.lab.lina tsuite.frt ;
	rm -f forth.lab
	cp forth.lab.lina forth.lab
	m4 $(TESTLINA32) >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	./lina32 <$@.1 2>&1| grep -v beta >$@.3
	diff -b -B $@.2 $@.3 || true
	ln -sf forth.lab.lina  forth.lab
	./lina32 -a <tsuite.frt 2>&1 |cat >tsuite32.out
	diff -bBw tsuite32.out testcmp || true
	rm $(TEMPFILE)

# No output expected, except for an official version (VERSION=A.B.C)
# The version number shows up in the diff.
testlina64 : $(TESTLINA64) ci86.lina64.rawtest lina64 forth.lab.lina tsuite.frt ;
	rm -f forth.lab
	cp forth.lab.lina forth.lab
	m4 $(TESTLINA64) >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	./lina64 <$@.1 2>&1| grep -v beta >$@.3
	diff -b -B $@.2 $@.3 || true
	ln -sf forth.lab.lina  forth.lab
	./lina64 -a <tsuite.frt 2>&1 |cat >tsuite64.out
	diff -bBw tsuite64.out testcmp || true
	rm $(TEMPFILE)

# No output expected, except for an official version (VERSION=A.B.C)
# The version number shows up in the diff.
testwina : ci86.wina.rawtest test.m4 wina.exe forth.lab.wina tsuite.frt ;
	rm -f forth.lab
	cp forth.lab.wina forth.lab
	m4 test.m4 $<  >$(TEMPFILE)
	echo "'NOOP 'OK 3 CELLS MOVE"        >$@.1
	echo "'STDOUT >DFA @ 'STDERR >DFA !" >>$@.1
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >>$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)
	wine wina.exe <$@.1 2>&1|\
	grep -v beta |\
	sed -e 's/.*wina.exe/wina.exe/' >$@.3
	diff -b -B $@.2 $@.3 || true
	cp -f forth.lab.wina  forth.lab
	wine wina.exe -a <tsuite.frt 2>&1 |grep -v OK >tsuite32.out
	diff -bBw tsuite32.out testcmp || true


# This just generates a test script and testfiles,
# but expects the test to run on a different system.
# The version number shows up in the diff.
testlina64.tar : $(TESTLINA64) ci86.lina64.rawtest ci86.lina64.fas forth.lab.lina tsuite.frt ;
	echo "#!/bin/sh ">testlina64
	echo "rm -f forth.lab                                 ">>testlina64
	echo "cp forth.lab.lina forth.lab                  ">>testlina64
	m4 $(TESTLINA64)  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >testlina64.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >testlina64.2
	echo "lina64 <testlina64.1 2>&1| grep -v beta >testlina64.3      ">>testlina64
	echo "diff -b -B testlina64.2 testlina64.3 || true                ">>testlina64
	echo "lina64 -a <tsuite.frt 2>&1 |cat >tsuite.tmp   ">>testlina64
	echo "diff -bBw tsuite.tmp tsuite64.out || true            ">>testlina64
	echo "cp forth.lab.lina  forth.lab            ">>testlina64
	rm $(TEMPFILE)
	tar cf $@ lina64 forth.lab.lina tsuite.frt tsuite64.out mywc64 testlina64 testlina64.1 testlina64.2 ci86.lina64.fas

testlinux : $(TESTLINUX) ci86.linux.rawtest ciforthc forth.lab ;
	m4 $(TESTLINUX)  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	ciforthc <$@.1 | grep -v beta >$@.3
	diff -b -B $@.2 $@.3 || true
	rm $(TEMPFILE)

testdpmi : ci86.dpmi.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)

testwinafiles : ci86.wina.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	echo "'STDOUT >DFA @ 'STDERR >DFA !" >$@.1
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >>$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)

testwina64files : ci86.wina64.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	echo "'STDOUT >DFA @ 'STDERR >DFA !" >$@.1
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >>$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)

testxinafiles : ci86.xina.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	echo "'TYPE >DFA @ 'ETYPE >DFA !" >$@.1
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >>$@.1
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$@.2
	rm $(TEMPFILE)

testminafiles : ci86.mina.rawtest test.m4 ;
	m4 test.m4 $<  >$(TEMPFILE)
	echo "'STDOUT >DFA @ 'STDERR >DFA !" >$@.1
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >>$@.1
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

#asgen.frt : RCS-as/asgen.frt,v ; co $<
#asi386.frt : RCS-as/asi386.frt,v ; co $<

# A Forth with the analyser built-in.
lina-ana : lina32 asgen.frt asi386.frt $(ANASRC)
	echo '"analyser.frt" INCLUDED   "SAVE-SYSTEM" WANTED  "$@" SAVE-SYSTEM' |./lina32

# Test the optimiser. FIXME! Gives ERROR 22 ( --> not called from block).
testoptimiser.out : testoptimiser.frt optimiser.frt lina-ana
	echo 'INCLUDE optimiser.frt INCLUDE $<' |lina-ana > $@
	diff -bBw $@ testcmp || true

lina-opt : optimiser.frt lina-ana
	echo 'INCLUDE optimiser.frt "$@" SAVE-SYSTEM' |lina-ana
	echo include optimiser.frt from lina-opt manually!
