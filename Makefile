# $Id: Makefile,v 5.67 2022/03/23 12:59:50 albert Exp $

# Copyright(2013): Albert van der Horst, HCC FIG Holland by GNU Public License
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
#   except constant_*.m4
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

TOOLSFORTH = \
boot.lab \
toblk.frt \
fromblk.frt \
sortworddoc.frt \
# That's all folks!

# Normally tools are not supplied with the release.
# But this is a tool not otherwise available.
TOOLS=  \
$(TOOLSFORTH:%.frt:%)
# That's all folks!

EXAMPLES= \
mywc        \
hellow.frt \
wc.script \
# That's all folks!


# Different assemblers should generate equivalent Forth's.
ASSEMBLERS= nasm  gas fasm masm
# Forth assembler sources that can be made using any assembler
# on any system, maybe needing a file constant_*.m4
TARGETS= lina32 lina64 wina64 wina32 xina32 xina64 \
alone linux alonehd msdos32 alonetr dpmi
DOCFORMAT= a.html  \
a.info             \
a.pdf              \
a.ps               \
# That's all folks!

# The kinds of Forth's binaries that can be made using NASM (not used)
BINTARGETS= mina alone
# If this makefile runs under Linux, the following forth's can be made and
# subsequently run
LINUXFORTHS= ciforthc lina32 lina64 flina32  flina64 nlina32  \
    nlina64 glina32  glina64

# Auxiliary targets.
OTHERTARGETS=   \
filler.frt      \
forth.lab       \
forth.lab.lina  \
forth.lab.wina  \
toblk           \
namescooked.m4  \
MANIFEST        \
# That's all folks!

# Intermediate targets.
INTERTARGETS=   \
menu.texinfo    \
temp.html       \
wordset.mi      \
# That's all folks!

# C-sources with various aims. FIXME: start with .c names.
CSRCFORTH= ciforth.c

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
ASSEMGEN = \
ci86.alone.asm  \
ci86.mina.msm  \
ci86.wina32.fas  \
ci86.wina64.fas  \
ci86.lina32.fas \
ci86.lina32.s  \
ci86.lina32.asm  \
ci86.lina64.fas \
ci86.lina64.s  \
ci86.alonehd.asm  \
forth.lab.wina \
forth.lab.lina \
# That's all folks!

# Note that the generated file ``namescooked.m4'' is included,
# because it forces to contain the release id.
RELEASECONTENT = \
ci86.gnr        \
constant_32.m4  \
constant_64.m4  \
constant_osx.m4  \
namescooked.m4  \
$(TARGETS:%=%.cfg)      \
$(DOC)          \
Makefile        \
test.mak        \
optim.mak        \
test.m4         \
$(TOOLSFORTH)   \
$(INGREDIENTS)  \
$(ASSEMBLERS:%=%.m4)    \
$(DOCTRANSFORMS)        \
$(TOOLS)        \
blocks.frt      \
options.frt     \
genboot.bat     \
$(EXAMPLES)   \
$(ASSEMGEN)  \
errors.linux.txt \
errors.dos.txt \
tsuite.frt      \
ci86.labtest \
# That's all folks!

TESTCMP = \
testcmp/tsuite32.out \
testcmp/tsuite64.out \
# That's all folks!

# 4.0 ### Version : an official release 4.0
# For a release VERSION is passed via the command line as e.g. 5.3
# If left out : beta, revision number is taken from current date.
VERSION=`date +%Y%b%d`
# M4_DEBUG=--debug=V   # deperado debugging.

EXTRACTORS= \
$(INGREDIENTS) \
$(ASSEMBLERS:%=%.m4)    \
constant_32.m4 \
constant_64.m4 \
namescooked.m4 \
# That's all folks!

CONFIGURATIONS= \
lina32.cfg            \
lina64.cfg

# The usable files.
RELEASELINA32USER = \
COPYING   \
READMElina.txt \
lina32      \
forth.lab     \
lina.1    \
$(DOCFORMAT:a%=ci86.lina32%) \
$(EXAMPLES) \
# That's all folks!

# Added source for modification.
RELEASELINA32 = \
$(RELEASELINA32USER) \
ci86.lina32.fas      \
ci86.lina32.s      \
ci86.lina32.texinfo \
# That's all folks!

RELEASELINA64USER = \
COPYING   \
READMElina.txt \
lina64      \
forth.lab     \
lina.1    \
$(DOCFORMAT:a%=ci86.lina64%) \
$(EXAMPLES) \
# That's all folks!

RELEASELINA64 = \
$(RELEASELINA64USER) \
ci86.lina64.fas      \
ci86.lina64.s      \
ci86.lina64.texinfo \
# That's all folks!

# full source release.
RELEASELINA32_M4 = \
$(RELEASELINA32) \
$(EXTRACTORS) \
$(TOOLS) \
$(CONFIGURATIONS) \
# That's all folks!

RELEASELINA64_M4 = \
$(RELEASELINA64) \
$(EXTRACTORS) \
$(TOOLS) \
$(CONFIGURATIONS) \
# That's all folks!

TEMPFILE=/tmp/ciforthscratch

# How to generate a Forth executable.
%: %.frt ; $(FORTH) -c $<

# Define fasm as *the* assembler generating binary files.
%:%.fas ; fasm $< -m256000

# Define NASM as an alternative for generating bin files.
%.bin:%.asm ; nasm -fbin $< -o $@ -l $*.lst

# mina.cfg and alone.cfg are present (at least via RCS)
# allow to generate ci86.mina.bin etc.
ci86.%.rawdoc ci86.%.rawtest : ci86.%.asm ;

VERSION : ; echo 'define({M4_VERSION},{'${VERSION}'})' >VERSION

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

.PRECIOUS: ci86.lina32.rawdoc ci86.lina32.mig ci86.wina32.rawdoc ci86.wina32.mig $(TEMPFILE) MANIFEST

.PHONY: default all clean boot filler moreboot allboot hdboot releaseproof zip mslinks release
# Default target for convenience
default : lina64
ci86.$(s).bin :

# tools
toblk: toblk.frt $(FORTH)
	cp boot.lab forth.lab
	$(FORTH) -c $<
	rm -f forth.lab forth.lab.lina

# Canonical targets
lina   : lina32 ; cp $< $@
#lina32   : flina32 ; cp $< $@
#lina64   : flina64 ; cp $< $@
lina32   : glina32 ; cp $< $@
lina64   : glina64 ; cp $< $@

#lina: lina64 ; $< -g 8000 $@
# For MS-windows programs fasm demands an include directory with assorted
# stuff. Use a symbolic link. Works on fasm 1.73.20 possibly not earlier.
wina32.exe: ci86.wina32.fas ; fasm $+ -m256000 ; mv ${<:.fas=}.exe $@
wina64.exe: ci86.wina64.fas ; fasm $+ -m256000 ; mv ${<:.fas=}.exe $@

# Some of these targets make no sense and will fail
all: $(TARGETS:%=ci86.%.asm) $(TARGETS:%=ci86.%.msm) $(BINTARGETS:%=ci86.%.bin) \
    $(LINUXFORTHS) $(OTHERTARGETS)

clean: \
; rm -f $(TARGETS:%=%.asm)  $(TARGETS:%=%.msm)  $(TARGETS:%=ci86.%.*)  \
; rm -f $(CSRCFORTHS:%.c=%.o) $(LINUXFORTHS) VERSION spy a.out\

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
	cat options.frt errors.linux.txt blocks.frt | ./toblk >$@
	ln -f $@ forth.lab

forth.lab.wina : toblk options.frt errors.dos.txt blocks.frt
	cat options.frt errors.dos.txt blocks.frt | ./toblk >$@
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
	mkdir ciforth-$(VERSION)/testcmp ;\
	cp $+ ciforth-$(VERSION) ;\
	cp $(TESTCMP) ciforth-$(VERSION)/testcmp ;\
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

LINA32ZIP : $(RELEASELINA32)
	rm -f ci86.lina32-$(VERSION) forth.lab.lina
	make forth.lab.lina
	ls $+ | sed s:^:ci86.lina32-$(VERSION)/: >MANIFEST
	ln -sf . ci86.lina32-$(VERSION)
	tar -czvf ci86.lina32-$(VERSION).tar.gz `cat MANIFEST`
	rm ci86.lina32-$(VERSION)

LINA64DEB  :  $(RELEASELINA64)
	echo $+ >MANIFEST
	debian.sh $(VERSION) lina64

LINA32DEB  :  $(RELEASELINA32)
	echo $+ >MANIFEST
	debian.sh $(VERSION) lina32

LINA64ZIP : $(RELEASELINA64)
	rm -f ci86.lina64-$(VERSION) forth.lab.lina
	make forth.lab.lina
	ls $+ | sed s:^:ci86.lina64-$(VERSION)/: >MANIFEST
	ln -sf . ci86.lina64-$(VERSION)
	tar -czvf ci86.lina64-$(VERSION).tar.gz `cat MANIFEST`
	rm ci86.lina64-$(VERSION)

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
# Linux native forth by gnu tools, only works on a 64 bit system
glina32 : ci86.lina32.s ; as --32 $+; ld a.out -melf_i386 -N -o $@
glina64 : ci86.lina64.s ; as --64 $+; ld a.out -melf_x86_64  -N -o $@


# Linux native forth by fasm tools
flina32 : ci86.lina32.fas ; fasm $+ -m256000; mv ${<:.fas=} $@
flina64 : ci86.lina64.fas ; fasm $+ -m256000; mv ${<:.fas=} $@

# Nowadays in the future the name constant.m4 has disappeared in
# favour of constant_*.m4
# All constant*.m4 files are gotten from source control.

# Linux native forth by gnu tools and linker
# The explicit invocation of a link script is intended to
# rid of -N. But -N seems the only one able to set p_type to CW .
# Otherwise the -c version leads to crashing progs.
gllina32 : ci86.lina32.s ; as --32 $+ -o gllina32.o; \
    echo ' SECTIONS { . = 0x08048054; }' >null.lds ;\
    ld -T null.lds gllina32.o -melf_i386 -N -o $@

# not functional
LINA32_M4ZIP : $(RELEASELINA32_M4) VERSION ci86.gnr
	rm -f lina32-$(VERSION) forth.lab.lina
	make forth.lab.lina
	echo  ci86.gnr VERSION $(EXTRACTORS) $(CONFIGURATIONS) |\
	sed s:^:lina32-$(VERSION)/: >MANIFEST
	ln -sf . lina32-$(VERSION)
	tar -czvf lina32-$(VERSION).tar.gz `cat MANIFEST`
	rm -r lina32-$(VERSION)

# not functional
LINA64_M4ZIP : $(RELEASELINA64_M4) VERSION ci86.gnr
	rm -f lina64-$(VERSION) forth.lab.lina
	make forth.lab.lina
	echo  ci86.gnr VERSION $(EXTRACTORS) $(CONFIGURATIONS) |\
	sed s:^:lina64-$(VERSION)/: >MANIFEST
	ln -sf . lina64-$(VERSION)
	tar -czvf lina64-$(VERSION).tar.gz `cat MANIFEST`
	rm -r lina64-$(VERSION)

# Add temporary stuff for testing, docs, optims.
include test.mak
include optim.mak

# not functional
# Obsolete, we don't yield for Debian pressure!
LINA32SRCZIPDEBIAN : $(RELEASELINA32_M4) VERSION ci86.gnr extract.mak
	rm -f lina32-$(VERSION) forth.lab.lina
	make forth.lab.lina
	mkdir extract
	cp ci86.gnr VERSION extract.mak $(EXTRACTORS) $(CONFIGURATIONS) extract
	find $(RELEASELINA32USER) extract | \
	sed s:^:lina32-$(VERSION)/: >MANIFEST
	ln -sf . lina32-$(VERSION)
	tar -czvf lina32-$(VERSION).tar.gz `cat MANIFEST`
	rm -r lina32-$(VERSION) extract
