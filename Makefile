# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file fig86.gnr
# into a diversity of Intel 86 assembly sources, and further into
# one of the $(TARGETS) Forth's.

#.SUFFIXES:
#.SUFFIXES:.bin.asm.m4.v.o.c

# Of the following constant.m4 is not a real source file.
# It could be, but it has been stolen.
INGREDIENTS = \
constant.m4      \
header.m4        \
postlude.m4      \
prelude.m4       \
protect.m4       \
width16.m4       \
width32.m4       \
# That's all folks!

# Different assemblers should generate equivalent Forth's.
ASSEMBLERS= masm nasm gas          
# The kinds of Forth assembler sources that can be made using any assembler
TARGETS= msdos alone linux lina
# The kinds of Forth's binaries that can be made using NASM (not used)
BINTARGETS= msdos alone 
# If this makefile runs under Linux, the following forth's can be made and 
# subsequently run
LINUXFORTHS= figforth lina
# Auxiliary targets
OTHERTARGETS= BLOCKS.BLK toblock fromblock
# C-sources with various aims.
CSRCAUX= toblock fromblock stealconstant
CSRCFORTH= figforth toblock fromblock stealconstant
CSRC= $(CSRCAUX) $(CSRCFORTH)

RELEASECONTENT = \
COPYING          \
fig86.gnr        \
BLOCKS.BLK       \
$(INGREDIENTS)   \
$(ASSEMBLERS:%=%.m4) \
$(TARGETS:%=%.cfg) \
$(CSRC:%=%.c)    \
Makefile         \
release.txt      \
fig86gnr.txt       \
fig86.alone.asm  \
fig86.msdos.msm  \
fig86.linux.asm  \
fig86.lina.asm  \
genboot.bat      \
link.script    \
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


# Define NASM as *the* assembler generating bin files.
%.bin:%.asm
	nasm -fbin $< -o $@ -l $*.lst 

# msdos.cfg and alone.cfg are present (at least via RCS)
# allow to generate fig86.msdos.bin etc.
fig86.%.asm : %.cfg nasm.m4 fig86.gnr ; m4 $+ >$@
fig86.%.msm : %.cfg masm.m4 fig86.gnr ; m4 $+ >$@
fig86.%.ps  : %.cfg gas.m4  fig86.gnr ; m4 $+ >$@
fig86.%     : %.cfg         fig86.gnr ; m4 $+ >$@

# gas needs extra transformations that m4 cannot handle.
# In particular the order of operands.
%.s : %.ps ; sed -f transforms <$+ >$@

.PHONY: default all clean boot filler moreboot allboot hdboot releaseproof zip
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

# Figforth calculates whether the screen boundaries are off by a sector.
# You can copy the filler by hand if this calculation fails, e.g. 5" floppies.
# The symptom is 8 LIST show the electives screen half and half of some other screen.
filler: fig86.alone.bin lina
	rm -f wc # Use the official `wc' command
	# Have forth calculate whether we need the filler sector
        # Use the exit command to return 1 or 0
	(filesize=`cat fig86.alone.bin |wc -c`; \
	echo $$filesize 1 - 512 / 1 + 2 MOD 0 0 1 LINOS | lina>/dev/null; \
	if [ 0 = $$? ] ; then mcopy filler.frt a:filler.frt ;fi)

moreboot: BLOCKS.BLK fig86.alone.bin  fig86.msdos.bin	   
	mcopy BLOCKS.BLK a: 
	mcopy fig86.msdos.bin	   a:msdos.com
                          
allboot: boot filler moreboot

BLOCKS.BLK : toblock blocks.frt ; toblock <blocks.frt >$@

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: fig86.alone.bin  
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440 

# ZIP targets
msdos.msm : fig86.msdos.msm ; cp $+ $@
alone.asm : fig86.alone.asm ; cp $+ $@

figdoc.zip : figdoc.txt glossary.txt frontpage.tif memmap.tif ; zip figdoc $+

zip : $(RELEASECONTENT) ; zip fig86g$(VERSION) $+

lina.zip : $(RELEASELINA) ; zip lina$(VERSION) $+

releaseproof : ; for i in $(RELEASECONTENT); do  rcsdiff -w $$i ; done

fig86.%.o : fig86.%.asm ; nasm $+ -felf -o $@ -l $(@:.o=.lst)

# This linking must be static, because `link.script' is tricky enough.
# but a .5M executable is better than a 64 M executable.
figforth : figforth.c fig86.linux.o ; $(CC) $(CFLAGS) $+ -static -Wl,-Tlink.script -o $@ 

# Linux native forth
lina : fig86.lina.o ; ld $+ -o $@
lina.asm : fig86.alone.asm ; cp $+ $@

# Convenience under linux. Steal the definitions of constants from c include's.
constant.m4 : stealconstant.c ; cc -E $+ | m4 prelude.m4 - >$@

# Add termporary stuff for testing, if needed.
#include test.mak
