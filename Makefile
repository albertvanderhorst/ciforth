# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file ci86.gnr
# into a diversity of Intel 86 assembly sources, and further into
# one of the $(TARGETS) Forth's.

#.SUFFIXES:
#.SUFFIXES:.bin.asm.m4.v.o.c

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
#* .pres : file to be pre-processed generating .s Experimental
#* .bin : a binary image without header (useful i.a. for msdos .com)
#* .gas : input file for `gas' assembler
#* .rawdoc : unsorted glossary items from the generic source.
#* .rawtest : unsorted and unexpanded tests.
#+ .m4 : m4 macro's possibly including other macro's
#   except constant.m4
# .cfg : m4 macro's generating files ( ci86.%.x + %.cfg -> ci86.%.y)
# .mi : files that after processed by m4 give a .texinfo file
# .mig : Currently in use for the wordset, which is a .mi file (WRONG!)
# It could be, but it has been stolen.

# ALL FILES STARTING IN ``ci86'' (OUTHER ``ci86.gnr'') ARE GENERATED

INGREDIENTS = \
header.m4       \
postlude.m4      \
prelude.m4       \
protect.m4       \
width16.m4       \
width32.m4       \
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

# Different assemblers should generate equivalent Forth's.
ASSEMBLERS= masm nasm gas
# The kinds of Forth assembler sources that can be made using any assembler
TARGETS= msdos alone linux lina alonehd msdos32 dpmi
# The kinds of Forth's binaries that can be made using NASM (not used)
BINTARGETS= msdos alone
# If this makefile runs under Linux, the following forth's can be made and
# subsequently run
LINUXFORTHS= ciforthc lina
# Auxiliary targets. Because of GNU make bug, keep constant.m4.
OTHERTARGETS= forth.lab toblock fromblock constant.m4
# C-sources with various aims.
CSRCAUX= toblock fromblock stealconstant
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
release.txt      \
fig86gnr.txt       \
testreport.txt     \
cfg.zip  \
$(SRCMI) \
# That's all folks!

# The following must be updated on the website, whenever
# any typo's are fixed. Unfortunately, it has become a separate
# maintenance chore, and is in effect a separate project.
DOCOLD = \
figdoc.zip    \
# That's all folks!

# These files can easily be generated, if you have linux.
EXAMPLES = \
ci86.alone.asm  \
ci86.msdos.msm  \
ci86.linux.asm  \
ci86.lina.asm  \
ci86.alonehd.asm  \
# That's all folks!

RELEASECONTENT = \
ci86.gnr        \
$(CSRC:%=%.c)    \
$(TARGETS:%=%.cfg) \
$(DOC)     \
Makefile         \
test.mak        \
$(INGREDIENTS)   \
$(ASSEMBLERS:%=%.m4) \
$(DOCTRANSFORMS) \
$(TOOLS)        \
blocks.frt       \
genboot.bat      \
$(EXAMPLES)     \
wc            \
# That's all folks!

# 3v1 ### Version : an official release 3.1
# 3d154  Release of a revision, so beta, the revision number is 3.154
VERSION=test  # Because normally VERSION is passed via the command line.

RELEASELINA = \
COPYING   \
ci86.lina.html \
ci86.lina.texinfo \
linarelease.txt  \
ci86.lina.asm      \
lina      \
forth.lab       \
$(CSRCAUX:%=%.c)    \
wc            \
# That's all folks!

TEMPFILE=/tmp/ciforthscratch

# Try to work with old files.
%:RCS/%,v ; co -d$(DATE) $<
ci86.gnr:RCS/ci86.gnr,v ; co -d$(DATE) $<

# Define NASM as *the* assembler generating bin files.
%.bin:%.asm
	nasm -fbin $< -o $@ -l $*.lst


# msdos.cfg and alone.cfg are present (at least via RCS)
# allow to generate ci86.msdos.bin etc.
ci86.%.rawdoc ci86.%.rawtest : ci86.%.asm ;

ci86.%.asm : %.cfg nasm.m4 ci86.gnr
	make constant.m4
	m4 $+ >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.asm=%.rawdoc)
	sed $(TEMPFILE) -e '1,/Split here for test/d' >$(@:%.asm=%.rawtest)
	rm $(TEMPFILE)

ci86.%.msm : %.cfg masm.m4 ci86.gnr ; \
	m4 $+ >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' | \
	sed -e '/Split here for test/,$$d' >$(@:%.msm=%.rawdoc)
	sed $(TEMPFILE) -e '/Split here for test/,$$d' >$(@:%.msm=%.rawtest)
	rm $(TEMPFILE)

ci86.%pres  : %.cfg gas.m4  ci86.gnr ; m4 $+ >$@
ci86.%     : %.cfg       ci86.gnr ; m4 $+ >$@

# gas needs extra transformations that m4 cannot handle.
# In particular the order of operands.
%.s : %pres ; sed -f transforms <$+ >$@

.PHONY: default all clean boot filler moreboot allboot hdboot releaseproof zip mslinks release

# Default target for convenience
default : lina
ci86.$(s).bin :

# Put include type of dependancies here
$(TARGETS:%=%.cfg) : $(INGREDIENTS) ; if [ -f $@ ] ; then touch $@ ; else co $@ ; fi

# Some of these targets make no sense and will fail
all: $(TARGETS:%=ci86.%.asm) $(TARGETS:%=ci86.%.msm) $(BINTARGETS:%=ci86.%.bin) \
    $(LINUXFORTHS) $(OTHERTARGETS)

clean : ; rm -f $(TARGETS:%=ci86.%.*)  $(CSRCS:%=%.o) $(LINUXFORTHS) $(OTHERTARGETS)

#msdos32.zip soesn't work yet.
release : strip figdoc.zip zip msdos.zip lina.zip # as.zip

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
trboot: ci86.alonetr.bin lina forth.lab
	rm fdimage
	echo \"ci86.alonetr.bin\" GET-FILE DROP HEX 10000 \
	     \"fdimage\" PUT-FILE BYE | lina
	cat forth.lab >>fdimage
	cp fdimage /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp fdimage /dev/fd0H1440

filler.frt: ; echo This file occupies one disk sector on IBM-PCs >$@

# Figforth calculates whether the screen boundaries are off by a sector.
# You can copy the filler by hand if this calculation fails, e.g. 5" floppies.
# The symptom is 8 LIST show the electives screen half and half of some other screen.
filler: ci86.alone.bin lina filler.frt
	rm -f wc # Use the official `wc' command
	# Have forth calculate whether we need the filler sector
	# Use the exit command to return 1 or 0
	(filesize=`cat ci86.alone.bin |wc -c`; \
	echo $$filesize 1 - 512 / 1 + 2 MOD 0 0 1 LINOS | lina>/dev/null; \
	if [ 0 = $$? ] ; then mcopy filler.frt a:filler.frt ;fi)

moreboot: forth.lab ci86.alone.bin  ci86.msdos.bin
	mcopy forth.lab a:
	mcopy ci86.msdos.bin      a:msdos.com

allboot: boot filler moreboot

forth.lab : toblock blocks.frt ; toblock <blocks.frt >$@

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: ci86.alonehd.bin
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440

figdoc.txt glossary.txt frontpage.tif memmap.tif : ; co -r1 $@
figdoc.zip : figdoc.txt glossary.txt frontpage.tif memmap.tif ; zip figdoc $+

zip : $(RELEASECONTENT) ; echo ci86g$(VERSION) $+ | xargs zip

# For msdos truncate all file stems to 8 char's and loose prefix `ci86.'
# Compiling a simple c-program may be too much, so supply forth.lab
msdos.zip : $(RELEASECONTENT) mslinks ;\
    echo fg$(VERSION) $(RELEASECONTENT) |\
    sed -e's/ ci86\./ /g' |\
    sed -e's/ gnr / ci86.gnr /g' |\
    xargs zip -k

# More messy things in behalf of msdos
mslinks :
	ln -sf ci86.lina.asm lina.asm
	ln -sf ci86.linux.asm linux.asm
	ln -sf ci86.msdos.msm msdos.msm
	ln -sf ci86.msdos.asm forth32.asm
	ln -sf ci86.msdos.bin forth32.com
	ln -sf ci86.alone.asm alone.asm
	ln -sf ci86.alonehd.asm alonehd.asm

lina.zip : $(RELEASELINA) ; zip lina$(VERSION) $+

releaseproof : ; for i in $(RELEASECONTENT); do  rcsdiff -w $$i ; done

ci86.lina.o : ci86.lina.asm ; nasm $+ -felf -o $@ -l $(@:.o=.lst)

ci86.%.o : ci86.%.asm ; nasm $+ -felf -o $@ -l $(@:.o=.lst)

# The tricky `link.script' has been dispensed with.
# However, now we need _fini and _init in ciforth.c and
# there are a few elf sections (.bss) that are mapped in the
# dictionary. But works.
ciforthc : ciforth.o ci86.linux.o
	 ld -static /usr/lib/gcrt1.o $+ -lc  -o ciforthc

# Linux native forth
lina : ci86.lina.o ; ld $+ -o $@

# This dependancy is problematic.
# Do `make constant.m4' explicitly beforehand.
# Because otherwise `constant.m4' is counted into the ``$+'' set.b
# ci86.alone.asm : constant.m4

# Convenience under linux. Steal the definitions of constants from c include's.
stealconstant: stealconstant.c ;  \
    cc -I/usr/include/asm $+ -o stealconstant

# Convenience under linux. Steal the definitions of constants from c include's.
constant.m4 : stealconstant ; $+ >$@

# Add termporary stuff for testing, if needed.
include test.mak
