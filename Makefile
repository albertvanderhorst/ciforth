# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file fig86.gnr
# into a diversity of Intel 86 assembly sources, and further into
# one of the $(TARGETS) Forth's.

.SUFFIXES:
.SUFFIXES:.com.asm.m4.v

RELEASECONTENT = \
Makefile         \
release.txt      \
readme.txt       \
fig86.gnr        \
alone.asm        \
msdos.msm        \
default.m4       \
alone.m4         \
msdos.m4         \
masm.m4          \
nasm.m4          \
header.m4        \
protect.m4       \
width16.m4       \
width32.m4       \
genboot.bat      \
# That's all folks!

# Letter versions are beta!
VERSION=0A

# The kinds of Forth's that can be made.
# Different assemblers should generate equivalent Forth's.
TARGETS= msdos alone

# Define NASM as *the* assembler generating com files.
%.com:%.asm
	nasm -fbin $< -o $@ -l $*.lst 

# msdos.m4 and alone.m4 are present (at least via RCS)
# allow to generate fig86.msdos.com etc.
fig86.%.asm : %.m4 nasm.m4 fig86.gnr ; m4 $+ >$@
fig86.%.msm : %.m4 masm.m4 fig86.gnr ; m4 $+ >$@
fig86.%     : %.m4         fig86.gnr ; m4 $+ >$@

# Default target for convenience
fig86.$(s).com :

# Put include type of dependancies here
msdos.m4 : default.m4 header.m4 ; co $@
alone.m4 : default.m4 header.m4 protect.m4 ; co $@ 

all: $(TARGETS:%=fig86.%.com) $(TARGETS:%=fig86.%.msm) $(TARGETS:%=fig86.%.asm)

clean : ; rm $(TARGETS:%=fig86.%.*)  

# The following must be run as root.
# Make a boot floppy by filling the bootsector by a raw copy,
# then creating a dos file system in accordance with the boot sector,
# then copying the forth system to exact the first available cluster.
# The option BOOTFD must be installed into alone.m4.
boot: fig86.alone.com  
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440 
	mformat -k a: 
	mcopy $+ a:forth.com

# Like above. However there is no attempt to have MSDOS reading from
# the hard disk succeed.
# The option BOOTHD must be installed into alone.m4.
hdboot: fig86.alone.com  
	cp $+ /dev/fd0H1440 || fdformat /dev/fd0H1440 ; cp $+ /dev/fd0H1440 

# ZIP targets
msdos.msm : fig86.msdos.msm ; cp $+ $@
alone.asm : fig86.alone.asm ; cp $+ $@

zip : $(RELEASECONTENT) ; zip fig86g$(VERSION) $+

# Add termporary stuff for testing, if needed.
include test.mak
