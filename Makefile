# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file fig86.gnr
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
#+ .m4 : m4 macro's possibly including other macro's
#   except constant.m4
# .cfg : m4 macro's generating files ( fig86.%.x + %.cfg -> fig86.%.y)
# .mi : files that after processed by m4 give a .texinfo file
# .mig : Currently in use for the wordset, which is a .mi file (WRONG!)
# It could be, but it has been stolen.

# ALL FILES STARTING IN ``fig86'' BUT BUT ``fig86.gnr'' ARE GENERATED

INGREDIENTS = \
header.m4        \
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
menu.m4         \
names.m4        \
wordset.m4      \
# That's all folks!

# Different assemblers should generate equivalent Forth's.
ASSEMBLERS= masm nasm gas
# The kinds of Forth assembler sources that can be made using any assembler
TARGETS= msdos alone linux lina alonehd
# The kinds of Forth's binaries that can be made using NASM (not used)
BINTARGETS= msdos alone
# If this makefile runs under Linux, the following forth's can be made and
# subsequently run
LINUXFORTHS= figforth lina
# Auxiliary targets. Because of GNU make bug, keep constant.m4.
OTHERTARGETS= BLOCKS.BLK toblock fromblock # constant.m4
# C-sources with various aims.
CSRCAUX= toblock fromblock stealconstant
CSRCFORTH= figforth stealconstant
CSRC= $(CSRCAUX) $(CSRCFORTH)

# Texinfo files still to be processed by m4.
SRCMI= \
figforth.mi \
intro.mi    \
manual.mi   \
rational.mi  \
# That's all folks!

# Documentation files and archives
DOC = \
COPYING          \
assembler.txt    \
editor.txt     \
release.txt      \
figdocadd.txt \
fig86gnr.txt       \
testreport.txt     \
cfg.zip         \
$(SRCMI) \
# That's all folks!

# The following must be updated on the website, whenever
# any typo's are fixed. Unfortunately, is has become a separate
# maintenance chore, and is in effect a separate project.
DOCOLD = \
figdoc.zip    \
# That's all folks!

# These files can easily be generated, if you have linux.
EXAMPLES = \
fig86.alone.asm  \
fig86.msdos.msm  \
fig86.linux.asm  \
fig86.lina.asm  \
fig86.alonehd.asm  \
# That's all folks!

RELEASECONTENT = \
fig86.gnr        \
$(CSRC:%=%.c)    \
$(TARGETS:%=%.cfg) \
$(DOC)           \
Makefile         \
test.mak        \
$(INGREDIENTS)   \
$(ASSEMBLERS:%=%.m4) \
$(DOCTRANSFORMS) \
blocks.frt       \
genboot.bat      \
link.script    \
$(EXAMPLES)     \
wc              \
# That's all folks!

# r## revision 2.## a beta release
# #d# 2.#.# stable release, e.g. 0d1
VERSION=test  # Because normally VERSION is passed via the command line.

RELEASELINA = \
COPYING          \
linarelease.txt  \
figdoc.zip    \
figdocadd.txt \
lina.asm      \
lina          \
BLOCKS.BLK       \
$(CSRCAUX:%=%.c)    \
wc              \
# That's all folks!

TEMPFILE=/tmp/figforthscratch

# Define NASM as *the* assembler generating bin files.
%.bin:%.asm
	nasm -fbin $< -o $@ -l $*.lst


# msdos.cfg and alone.cfg are present (at least via RCS)
# allow to generate fig86.msdos.bin etc.
fig86.%.asm fig86.%.rawdoc : %.cfg nasm.m4 fig86.gnr
	m4 $+ >$(TEMPFILE)
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@
	sed $(TEMPFILE) -e '1,/Split here for doc/d' >$(@:%.asm=%.rawdoc)
	rm $(TEMPFILE)

fig86.%.msm fig86.%.rawdoc : %.cfg masm.m4 fig86.gnr ; \
	m4 $+ >$(TEMPFILE) ; \
	sed $(TEMPFILE) -e '/Split here for doc/,$$d' >$@ ; \
	sed $(TEMPFILE) -e '1,/Split here for doc/d' >$(@:%.asm=%.rawdoc)
	rm $(TEMPFILE)

fig86.%pres  : %.cfg gas.m4  fig86.gnr ; m4 $+ >$@
fig86.%     : %.cfg         fig86.gnr ; m4 $+ >$@

# gas needs extra transformations that m4 cannot handle.
# In particular the order of operands.
%.s : %pres ; sed -f transforms <$+ >$@

.PHONY: default all clean boot filler moreboot allboot hdboot releaseproof zip mslinks
# Default target for convenience
default : figforth
fig86.$(s).bin :

# Put include type of dependancies here
$(TARGETS:%=%.cfg) : $(INGREDIENTS) ; if [ -f $@ ] ; then touch $@ ; else co $@ ; fi

# Some of these targets make no sense and will fail
all: $(TARGETS:%=fig86.%.asm) $(TARGETS:%=fig86.%.msm) $(BINTARGETS:%=fig86.%.bin) \
    $(LINUXFORTHS) $(OTHERTARGETS)

clean : ; rm -f $(TARGETS:%=fig86.%.*)  $(CSRCS:%=%.o) $(LINUXFORTHS) $(OTHERTARGETS)

# The following must be run as root.
# Make a boot floppy by filling the bootsector by a raw copy,
# then creating a dos file system in accordance with the boot sector,
# then copying the forth system to exact the first available cluster.
# The option BOOTFD must be installed into alone.m4.
boot: fig86.alone.bin
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440
	mformat -k a:
	mcopy $+ a:forth.com

filler.frt: ; echo This file occupies one disk sector on IBM-PCs >$@

# Figforth calculates whether the screen boundaries are off by a sector.
# You can copy the filler by hand if this calculation fails, e.g. 5" floppies.
# The symptom is 8 LIST show the electives screen half and half of some other screen.
filler: fig86.alone.bin lina filler.frt
	rm -f wc # Use the official `wc' command
	# Have forth calculate whether we need the filler sector
	# Use the exit command to return 1 or 0
	(filesize=`cat fig86.alone.bin |wc -c`; \
	echo $$filesize 1 - 512 / 1 + 2 MOD 0 0 1 LINOS | lina>/dev/null; \
	if [ 0 = $$? ] ; then mcopy filler.frt a:filler.frt ;fi)

moreboot: BLOCKS.BLK fig86.alone.bin  fig86.msdos.bin
	mcopy BLOCKS.BLK a:
	mcopy fig86.msdos.bin      a:msdos.com

allboot: boot filler moreboot

BLOCKS.BLK : toblock blocks.frt ; toblock <blocks.frt >$@

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: fig86.alonehd.bin
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440

figdoc.zip : figdoc.txt glossary.txt frontpage.tif memmap.tif ; zip figdoc $+

zip : $(RELEASECONTENT) ; echo fig86g$(VERSION) $+ | xargs zip

# For msdos truncate all file stems to 8 char's and loose prefix `fig86.'
# Compiling a simple c-program may be too much, so supply BLOCKS.BLK
msdoszip : $(RELEASECONTENT) mslinks ;\
    echo fg$(VERSION) $(RELEASECONTENT) |\
    sed -e's/\<fig86\.//g' |\
    sed -e's/\<gnr\>/fig86.gnr/' |\
    sed -e's/\<blocks.frt\>/BLOCKS.BLK/' |\
    sed -e's/ \([^ .]\{1,8\}\)[^ .]*\./ \1./g' |\
    xargs zip

# More messy things in behalf of msdos
mslinks :
	ln -sf fromblock.c frombloc.c
	ln -sf fig86.lina.asm lina.asm
	ln -sf fig86.linux.asm linux.asm
	ln -sf fig86.msdos.msm msdos.msm
	ln -sf fig86.msdos.asm forth32.asm
	ln -sf fig86.msdos.bin forth32.com
	ln -sf fig86.alone.asm alone.asm
	ln -sf fig86.alonehd.asm alonehd.asm
	ln -sf figdocadd.txt figdocad.txt
	ln -sf stealconstant.c stealcon.c

lina.zip : $(RELEASELINA) ; zip lina$(VERSION) $+

releaseproof : ; for i in $(RELEASECONTENT); do  rcsdiff -w $$i ; done

fig86.%.o : fig86.%.asm ; nasm $+ -felf -o $@ -l $(@:.o=.lst)

# This linking must be static, because `link.script' is tricky enough.
# but a .5M executable is better than a 64 M executable.
figforth : figforth.c fig86.linux.o link.script
	$(CC) $(CFLAGS) figforth.c fig86.linux.o -static -Wl,-Tlink.script -Wl,-M -o $@

# Linux native forth
lina : fig86.lina.o ; ld $+ -o $@

# Error in GNU make. This dependancy is not seen.
# Do `make constant.m4' explicitly beforehand.
fig86.alone.asm : constant.m4

# Convenience under linux. Steal the definitions of constants from c include's.
constant.m4 : stealconstant.c ; cc -E -I/usr/include/asm $+ | m4 prelude.m4 - >$@

# Add termporary stuff for testing, if needed.
include test.mak
