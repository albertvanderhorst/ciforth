# $Id$
# Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
#
# This defines the transformation from the generic file fig86.gnr
# into a diversity of Intel 86 assembly sources, and further into
# one of the $(TARGETS) Forth's.

.SUFFIXES:
.SUFFIXES:.com.asm.m4.v

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

fig86.msdos.com :

all: $(TARGETS:%=fig86.%.com) $(TARGETS:%=fig86.%.msm)

clean : ; rm $(TARGETS:%=fig86.%.*)  

include test.mak
