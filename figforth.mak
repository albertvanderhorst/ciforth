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

# The kinds of Forth's that can be made
# Different assemblers should generate equivalent Forth's.
TARGETS= msdos alone linux lina
CSRC= figforth toblock fromblock stealconstant

RELEASECONTENT = \
fig86.gnr        \
BLOCKS.BLK       \
$(INGREDIENTS)   \
$(TARGETS:%=%.cfg) \
$(CSRC:%=%.c)    \
Makefile         \
release.txt      \
readme.txt       \
fig86.alone.asm  \
fig86.msdos.msm  \
fig86.linux.asm  \
masm.m4          \
nasm.m4          \
genboot.bat      \
link.script    \
# That's all folks!

# Letter versions are beta!
VERSION=0A


# Define NASM as *the* assembler generating bin files.
%.bin:%.asm
	nasm -fbin $< -o $@ -l $*.lst 

# msdos.cfg and alone.m4 are present (at least via RCS)
# allow to generate fig86.msdos.bin etc.
fig86.%.asm : %.cfg nasm.m4 fig86.gnr ; m4 $+ >$@
fig86.%.msm : %.cfg masm.m4 fig86.gnr ; m4 $+ >$@
fig86.%     : %.cfg         fig86.gnr ; m4 $+ >$@

# Default target for convenience
default : figforth
fig86.$(s).bin :

# Put include type of dependancies here
$(TARGETS:%=%.cfg) : $(INGREDIENTS) ; if [ -f $@ ] ; then touch $@ ; else co $@ ; fi

all: $(TARGETS:%=fig86.%.asm) $(TARGETS:%=fig86.%.msm) $(TARGETS:%=fig86.%.bin) 

clean : ; rm -f $(TARGETS:%=fig86.%.*)  $(CSRCS:%=%.o) figforth

# The following must be run as root.
# Make a boot floppy by filling the bootsector by a raw copy,
# then creating a dos file system in accordance with the boot sector,
# then copying the forth system to exact the first available cluster.
# The option BOOTFD must be installed into alone.m4.
boot: fig86.alone.bin  
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440 
	mformat -k a: 
	mcopy $+ a:forth.com

# Use this if screen boundaries are off by a sector.
filler: 
	mcopy filler.frt a:filler.frt

moreboot: BLOCKS.BLK fig86.alone.bin  
	mcopy BLOCKS.BLK a:
	mcopy fig86.msdos.bin	   a:msdos.com
                           
BLOCKS.BLK : toblock blocks.frt ; toblock <blocks.frt >$@

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: fig86.alone.bin  
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440 

# ZIP targets
msdos.msm : fig86.msdos.msm ; cp $+ $@
alone.asm : fig86.alone.asm ; cp $+ $@

zip : $(RELEASECONTENT) ; zip fig86g$(VERSION) $+

releaseproof : ; for i in $(RELEASECONTENT); do  rcsdiff -w $$i ; done

fig86.%.o : fig86.%.asm ; nasm $+ -felf -o $@ -l $(@:.o=.lst)

# This linking must be static, because `link.script' is tricky enough.
# but a .5M executable is better than a 64 M executable.
figforth : figforth.c fig86.linux.o ; $(CC) $(CFLAGS) $+ -static -Wl,-Tlink.script -o $@ 

# Convenience under linux. Steal the definitions of constants from c include's.
constant.m4 : stealconstant.c ; cc -E $+ | m4 prelude.m4 - >$@

# Add termporary stuff for testing, if needed.
include test.mak
