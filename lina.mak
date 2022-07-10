# $Id: lina.mak,v 1.1 2018/01/18 19:18:55 albert Exp $
# Copyright (2017): Albert van der Horst {by GNU Public License 2}
# This generates all buildables from a source distribution of lina.

# This makefile contains the targets that can be build based on
# the assembler file.

# Sources these files may be used to generate other files
SRC = \
lina.fas    \
forth.lab \
# That's all folks!

# Documentation
DOC = \
COPYING   \
READMElina.txt \
lina.1    \
ci86.lina.texinfo \
ci86.lina.html \
ci86.lina.pdf \
ci86.lina.info \
ci86.lina.html \
ci86.lina.ps \
# That's all folks!

# Generated files
GEN = \
lina      \
lina.html \
lina.info \
lina.pdf \
lina.ps \
# That's all folks!

# All files to be in a binary release
RELEASELINA = \
$(SRC) \
$(DOC) \
$(GEN) \
$(EXAMPLESRC) \
# That's all folks!

# gnu type destination
DESTDIR=/

# Define fasm as *the* assembler generating bin files.
%:%.fas ; fasm $< -m256000

build: $(GEN)

lina : ci86.lina.fas
        fasm $< -m256000
        #./ci86.lina -g 1000 lina
        mv ci86.lina lina

clean :
        rm -f $(GEN)

install : lina forth.lab
        mkdir -p $(DESTDIR)/usr/bin
        cp lina $(DESTDIR)/usr/bin
        mkdir -p $(DESTDIR)/usr/lib
        cp forth.lab $(DESTDIR)/usr/lib

lina.html : ci86.lina.html
        cp -l $< $@

lina.info : ci86.lina.info
        cp -l $< $@

lina.pdf : ci86.lina.pdf
        cp -l $< $@

lina.ps : ci86.lina.ps
        cp -l $< $@
