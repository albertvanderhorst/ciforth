# $Id: Makefile,v 5.52 $
# Copyright(2017): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file ci86.gnr
# into a diversity of Intel 86 assembly sources, and further into
# one of the $(TARGETS) Forth's.

#.SUFFIXES:
#.SUFFIXES:.bin.asm.m4.v.o.c.fas.s

# Applicable suffixes : * are generated files
# + are generated files if they are mentionned on the next line
#
#* .dvi .tex .ps : as usual (See TeX)
#+ .texinfo : texinfo
#   menu.texinfo gloss.texinfo
#* .asm : input file for `nasm' assembler
#* .BLK : contains blocks usable by Forth
# .frt : text file : contains blocks in an \n separated stream
#* .msm : input file for `MASM' and `tasm' assembler
#* .s : input file for `gas' assembler  Experimental
#* .bin : a binary image without header (useful i.a. for msdos .com)
#* .fas : input file for `fasm' assembler
#* .gas : input file for `gas' assembler
#* .rawdoc : unsorted glossary items from the generic source.
#* .rawtest : unsorted and unexpanded tests.
#+ .m4 : m4 macro's possibly including other macro's
#   except constant.m4
# .cfg : m4 macro's generating files ( ci86.%.x + %.cfg -> ci86.%.y)
# .mi : files that after processed by m4 give a .texinfo file
# .mig : Currently in use for the wordset, which is a .mi file (WRONG!)
# It could be, but it has been stolen.

# Normal use of m4
M4=m4 -G ciforth.m4
# Needed for gas and fasm where we need the patsubst macro.
M4_GNU=m4 ciforth.m4

FORTH=./lina            # Our utility Forth.

# ALL FILES STARTING IN ``ci86'' (OUTHER ``ci86.gnr'') ARE GENERATED

INGREDIENTS = \
ciforth.m4       \
header.m4        \
postlude.m4      \
prelude.m4       \
protect.m4       \
width16.m4       \
width32.m4       \
width64.m4       \
# That's all folks!

DOCTRANSFORMS = \
gloss.m4        \
glosshtml.m4    \
indexhtml.m4    \
manual.m4       \
menu.m4  \
names.m4        \
wordset.m4      \
# That's all folks!

# Normally tools are not supplied with the release.
# But this is a tool not otherwise available.
TOOLS=  \
ssort   \
# That's all folks!

EXAMPLESRC= \
mywc32      \
mywc64      \
hellow.frt \
wc.script \
# That's all folks!

# Index files used by info, some are empty for ciforth.
INDICES= cp fn ky pg tp vr

# Different assemblers should generate equivalent Forth's.
ASSEMBLERS= nasm  gas fasm masm
# Forth assembler sources that can be made using any assembler
# on any system, maybe needing a file constant.m4
TARGETS= lina32 lina64 wina mina xina \
alone linux alonehd msdos32 alonetr dpmi

# The kinds of Forth's binaries that can be made using NASM (not used)
BINTARGETS= mina alone
# If this makefile runs under Linux, the following forth's can be made and
# subsequently run
LINUXFORTHS= ciforthc lina32 glina32 lina64
# Auxiliary targets.
OTHERTARGETS=   \
filler.frt      \
forth.lab       \
forth.lab.lina  \
forth.lab.wina  \
toblk           \
constant.m4     \
namescooked.m4  \
stealconstant   \
MANIFEST        \
# That's all folks!

# Intermediate targets.
INTERTARGETS=   \
menu.texinfo    \
temp.html       \
wordset.mi      \
# That's all folks!

# C-sources with various aims. FIXME: start with .c names.
CSRCAUX= stealconstant
CSRCFORTH= ciforth stealconstant
CSRC= $(CSRCAUX) $(CSRCFORTH)

# Texinfo files still to be processed by m4.
SRCMI= \
assembler.mi    \
cifgen.mi \
ciforth.mi \
intro.mi    \
manual.mi   \
rational.mi  \
# That's all folks!

# Documentation files and archives
DOC = \
COPYING   \
README.ciforth      \
testreport.txt     \
$(SRCMI) \
# That's all folks!

# These files can be generated, on the linux Suse host.
# They assemble mostly on the Suse host, but always on the target.
EXAMPLES = \
ci86.alone.asm  \
ci86.mina.msm  \
ci86.wina.fas  \
ci86.lina.asm  \
ci86.lina.fas \
ci86.lina.s  \
ci86.lina64.s  \
ci86.lina64.fas \
ci86.alonehd.asm  \
forth.lab.wina \
forth.lab.lina \
constant.m4  \
constant_64.m4  \
constant_osx.m4  \
# That's all folks!

# Note that the generated file ``namescooked.m4'' is included,
# because it require RCS to generate.
RELEASECONTENT = \
ci86.gnr        \
$(CSRC:%=%.c)   \
$(TARGETS:%=%.cfg)      \
$(DOC)          \
namescooked.m4  \
Makefile        \
test.mak        \
test.m4         \
tsuite.frt      \
$(INGREDIENTS)  \
$(ASSEMBLERS:%=%.m4)    \
$(DOCTRANSFORMS)        \
$(TOOLS)        \
blocks.frt      \
options.frt     \
genboot.bat     \
mywc32          \
mywc64          \
$(EXAMPLES)     \
errors.linux.txt \
errors.dos.txt \
ci86.lina.labtest \
# That's all folks!

# 4.0 ### Version : an official release 4.0
# Left out : beta, revision number is taken from current date.
# For a release VERSION is passed via the command line as e.g. 5.3
VERSION=`date +%Y%b%d`
# M4_DEBUG=--debug=V   # deperado debugging.

RELEASELINA = \
COPYING   \
READMElina.txt \
ci86.lina.info \
ci86.lina.html \
ci86.lina.pdf \
ci86.lina.ps \
ci86.lina.texinfo \
ci86.lina.fas    \
lina      \
lina.1    \
forth.lab     \
$(EXAMPLESRC) \
#ci86.lina32.asm      \
# That's all folks!

RELEASELINA32 = \
COPYING   \
READMElina.txt \
ci86.lina32.info \
ci86.lina32.html \
ci86.lina32.pdf \
ci86.lina32.ps \
ci86.lina32.texinfo \
ci86.lina32.fas      \
lina32      \
lina.1    \
forth.lab     \
$(EXAMPLESRC) \
#ci86.lina32.asm      \
# That's all folks!

RELEASELINA64 = \
COPYING   \
READMElina.txt \
ci86.lina64.info \
ci86.lina64.html \
ci86.lina64.pdf \
ci86.lina64.ps \
ci86.lina64.texinfo \
ci86.lina64.fas  \
lina64          \
lina.1    \
forth.lab     \
$(EXAMPLESRC) \
#ci86.lina64.fas \
# That's all folks!

VERSION:;echo 'define({M4_VERSION},{'${VERSION}'})' >VERSION

TEMPFILE=/tmp/ciforthscratch

# How to generate a Forth executable.
%: %.frt ; $(FORTH) -c $<

# KEEP THIS IN UNTIL CREATION OF toblk IS OKAY
#%: %.frt ./lina ; cp boot.lab forth.lab; $(FORTH) -c $^

# Define fasm as *the* assembler generating bin files.
%:%.fas ; fasm $< -m256000

# Define NASM as an alternative for generating bin files.
%.bin:%.asm ; nasm -fbin $< -o $@ -l $*.lst

#%.exe: ci86.%.fas ; fasm $+ -m256000

# mina.cfg and alone.cfg are present (at least via RCS)
# allow to generate ci86.mina.bin etc.
ci86.%.rawdoc ci86.%.rawtest : ci86.%.asm ;


ci86.%.asm : %.cfg VERSION nasm.m4 ci86.gnr
	cat $+ | $(M4) $(M4_DEBUG) - > $(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.asm=%.rawdoc)
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$(@:%.asm=%.rawtest)
	rm $(TEMPFILE)

# This is for masm and tasm.
# They can't handle 0x.
ci86.%.msm : VERSION %.cfg masm.m4 ci86.gnr
	cat $+ | m4 >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' | \
	sed -e 's/0x\([A-F0-9]*\)/0\1H/g'             | \
	sed -e 's/^\([_A-Za-z0-9]*:\) *\(D[BWD]\)/\1\n\
	\2/g' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.msm=%.rawdoc)
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$(@:%.msm=%.rawtest)
	rm $(TEMPFILE)

# Using FASM, generating a windows executable on linux.
ci86.%.fas : VERSION %.cfg fasm.m4 ci86.gnr
	cat $+ | $(M4_GNU) - >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' \
	>$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.fas=%.rawdoc)
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$(@:%.fas=%.rawtest)
	rm $(TEMPFILE)

# Now that gas has the .Intel_syntax option, its source can be
# generated by m4 alone. In particular the order of operands.
ci86.%.s : VERSION %.cfg gas.m4 ci86.gnr ; \
	cat $+ | $(M4_GNU) - >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.s=%.rawdoc)
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$(@:%.s=%.rawtest)
	rm $(TEMPFILE)

.PRECIOUS: ci86.lina32.rawdoc ci86.lina32.mig ci86.wina.rawdoc ci86.wina.mig $(TEMPFILE)

.PHONY: default all clean RCSCLEAN boot filler moreboot allboot hdboot releaseproof zip mslinks release
# Default target for convenience
default : lina64
ci86.$(s).bin :

# tools
toblk: toblk.frt $(FORTH)
	cp boot.lab forth.lab
	$(FORTH) -c $<
	rm -f forth.lab forth.lab.lina


# Canonical targets
lina   : ci86.lina.fas   ; fasm $+ -m256000; mv ${<:.fas=} $@
lina32 : ci86.lina32.fas ; fasm $+ -m256000; mv ${<:.fas=} $@
lina64 : ci86.lina64.fas ; fasm $+ -m256000; mv ${<:.fas=} $@
#lina: lina64 ; $< -g 8000 $@
wina.exe: ci86.wina.fas ; fasm $+ -m256000 ; mv ${<:.fas=}.exe $@

# Some of these targets make no sense and will fail
all: $(TARGETS:%=ci86.%.asm) $(TARGETS:%=ci86.%.msm) $(BINTARGETS:%=ci86.%.bin) \
    $(LINUXFORTHS) $(OTHERTARGETS)

clean: \
; rm -f $(TARGETS:%=%.asm)  $(TARGETS:%=%.msm)  $(TARGETS:%=ci86.%.*)  \
; rm -f $(INTERTARGETS) \
; rm -f $(CSRCS:%=%.o) $(LINUXFORTHS) VERSION spy a.out\
; for i in $(INDICES) ; do rm -f *.$$i *.$$i's' ; done

cleanall: clean  testclean ; \
    rm -f $(OTHERTARGETS) $(INTERTARGETS) ; \
    rm -f *.aux *.log *.ps *.toc *.pdf    ; \
    rm -f *.zip *gz

release : strip zip msdos32.zip msdos.zip lina.zip

#Install it. To be run as root
install: ; @echo 'There is no "make install" ; use "lina -i <binpath> <libpath> <shellpath>"'

# You may need to run the following run as root.
# Make a boot floppy by filling the bootsector by a raw copy,
# then creating a dos file system in accordance with the boot sector,
# then copying the forth system to exact the first available cluster.
# The option BOOTFD must be installed into alone.m4.
boot: ci86.alone.bin
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440
	mformat -k a:
	mcopy $+ a:forth.com

# You may need to run the following run as root.
# Make a raw boot floppy with no respect for DOS.
# The Forth gets its information from the boot sector,
# that is filled in with care.
# The option BOOTSECTRK must be installed into alonetr.m4.
trboot: ci86.alonetr.bin forth.lab.wina
	rm fdimage || true
	echo \"ci86.alonetr.bin\" GET-FILE DROP HEX 10000 \
	     \"fdimage\" PUT-FILE BYE | $(FORTH)
	cat forth.lab.wina >>fdimage
	cp fdimage /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp fdimage /dev/fd0H1440

filler.frt: ; echo This file occupies one disk sector on IBM-PCs >$@

# ciforth calculates whether the screen boundaries are off by a sector.
# You can copy the filler by hand if this calculation fails, e.g. 5" floppies.
# The symptom is 8 LIST show the electives screen half and half of some other screen.
filler: ci86.alone.bin filler.frt
	rm -f wc # Use the official `wc' command
	# Have forth calculate whether we need the filler sector
	(filesize=`cat ci86.alone.bin |wc -c`; \
	x=`echo $$filesize 1 - 512 / 1 + 2 MOD . BYE | $(FORTH)`; \
	if [ 0 = $$x ] ; then mcopy filler.frt a:filler.frt ;fi)

moreboot: forth.lab.wina ci86.alone.bin  ci86.mina.bin
	mcopy forth.lab.wina a:forth.lab
	mcopy ci86.mina.bin      a:mina.com

allboot: boot filler moreboot

forth.lab.lina : toblk options.frt errors.linux.txt blocks.frt
	cat options.frt errors.linux.txt blocks.frt | toblk >$@
	ln -f $@ forth.lab

forth.lab.wina : toblk options.frt errors.dos.txt blocks.frt
	cat options.frt errors.dos.txt blocks.frt | toblk >$@
	ln -f $@ forth.lab

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: ci86.alonehd.bin
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440

figdoc.zip : ; echo "checkout an old version for figdoc.zip, use rcs!"

# Release a generic system.
zip : $(RELEASECONTENT) cifgen.info ;\
	mkdir ciforth-$(VERSION) ;\
	cp $+ ciforth-$(VERSION) ;\
	tar -cvzf ciforth-$(VERSION).tar.gz ciforth-$(VERSION) ;\
	rm -r ciforth-$(VERSION)

# For msdos truncate all file stems to 8 char's and loose prefix `ci86.'
# Compiling a simple c-program may be too much, so supply forth.lab
msdos.zip : $(RELEASECONTENT) mslinks ;\
	ln -f forth.lab.wina forth.lab ;\
    echo fg$(VERSION) $(RELEASECONTENT) forth.lab |\
    sed -e's/ ci86\./ /g' |\
    sed -e's/ gnr / ci86.gnr /g' |\
    xargs zip -k

# More messy things in behalf of msdos
mslinks :
	ln -sf ci86.lina32.asm lina32.asm
	ln -sf ci86.linux.asm linux.asm
	ln -sf ci86.mina.msm mina.msm
	ln -sf ci86.wina.asm wina.asm
	ln -sf ci86.alone.asm alone.asm
	ln -sf ci86.alonehd.asm alonehd.asm
	ln -f forth.lab.wina forth.lab


forth.lab : forth.lab.lina forth.lab.wina

LINAZIP : $(RELEASELINA) ;\
	rm -f lina-$(VERSION) forth.lab.lina
	make forth.lab.lina
	ls $+ | sed s:^:lina-$(VERSION)/: >MANIFEST
	ln -sf . lina-$(VERSION)
	tar -czvf lina-$(VERSION).tar.gz `cat MANIFEST`
	rm lina-$(VERSION)

LINA32ZIP : $(RELEASELINA32)
	rm -f lina32-$(VERSION) forth.lab.lina
	make forth.lab.lina
	ls $+ | sed s:^:lina32-$(VERSION)/: >MANIFEST
	ln -sf . lina32-$(VERSION)
	tar -czvf lina32-$(VERSION).tar.gz `cat MANIFEST`
	rm lina32-$(VERSION)

LINA64ZIP : $(RELEASELINA64) ;\
	rm -f lina64-$(VERSION) forth.lab.lina
	make forth.lab.lina
	ls $+ | sed s:^:lina64-$(VERSION)/: >MANIFEST
	ln -sf . lina64-$(VERSION)
	tar -czvf lina64-$(VERSION).tar.gz `cat MANIFEST`
	rm lina64-$(VERSION)

releaseproof : ; for i in $(RELEASECONTENT); do  rcsdiff -w $$i ; done

ci86.lina32.o : ci86.lina32.asm ; nasm $+ -felf -g -o $@ -l $(@:.o=.lst)
ci86.lina64.o : ci86.lina64.asm ; nasm $+ -felf64 -g -o $@ -l $(@:.o=.lst)

ci86.%.o : ci86.%.asm ; nasm $+ -o $@ -l $(@:.o=.lst)

# The tricky `link.script' has been dispensed with.
# However, now we need _fini and _init in ciforth.c and
# there are a few elf sections (.bss) that are mapped in the
# dictionary. But works.
ciforthc : ciforth.o ci86.linux.o
	 ld -static /usr/lib/gcrt1.o $+ -lc  -o ciforthc

# Linux native forth by nasm. FIXME the linking doesn't work.
nlina32 : ci86.lina32.o ; ld $+ -melf_i386 -N -o $@
nlina64 : ci86.lina64.o ; \
  export LDEMULATION=elf_x86_64 ;\
  ld $+ -N -o $@

# Linux native forth by gnu tools
glina32 : ci86.lina32.s ; as --32 $+; ld a.out -melf_i386 -N -o $@

# Linux native forth by gnu tools, only works on a 64 bit system
glina64 : ci86.lina64.s ; as --64 $+; ld -N a.out -o $@

# Linux native forth by fasm tools
flina32 : ci86.lina32.fas ; fasm $+ -m256000; mv ${<:.fas=} $@

# This dependancy is problematic.
# Do `make constant.m4' explicitly beforehand.
# Because otherwise `constant.m4' is counted into the ``$+'' set.b
# ci86.alone.asm : constant.m4

# Convenience under linux's and osx.
# Steal the definitions of constants from c include's.
stealconstant: stealconstant.c ;  \
    cc $+ -o stealconstant

# Build these only on the linux 32 ,linux 64, osx target,
# otherwise consider the constantxx files sources.
constant.m4 : stealconstant ; $+ >$@
#constant_64.m4 : stealconstant_64; $+ >$@
#constant_osx.m4 : stealconstant_osx ; $+ >$@

stealconstant_64:
	@echo By hand: build stealconstant for 64 bit using -m64, then rename it

stealconstant_osx:
	@echo By hand: build stealconstant for osx, then rename it

# Add temporary stuff for testing, if needed.
include test.mak
