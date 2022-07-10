# Adapted for initial release of lina
# Copyright (2022): Albert van der Horst {by GNU Public License 2}
# This generates the executable from a source distribution of lina.

# This makefile contains the targets that can be build based on
# the assembler file.

FORTH=lina32

# Prefabricated files.
PREFAB = \
$(FORTH).s \
$(FORTH).fas \
$(FORTH).texinfo \
$(FORTH).info \
$(FORTH).html \
$(FORTH).pdf \
$(FORTH).ps \
# That's all folks!

DOC= \
READMElina.txt \
lina.1 \
# That's all folks!

# Generated files
GEN = \
$(FORTH)      \
# That's all folks!

# gnu type destination
DESTDIR=/
PREFIX=/usr

build: $(GEN)

$(FORTH) : $(FORTH).s
	sed -e 's?forth.lab         ?/usr/lib/forth.lab?' $< > temp.s
	as -32 temp.s
	ld -melf_i386   -s -N a.out -o $@
	rm -f temp.s a.out

# $(FORTH) : $(FORTH).fas
#         sed -e 's?forth.lab?/usr/lib/forth.lab?' $< > temp.fas
#         fasm temp.fas -m256000
#         # ./$(FORTH) -g 1000 $(FORTH)
#         mv temp $@

clean :
	rm -f $(GEN)

install : $(FORTH) forth.lab
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $(FORTH) $(DESTDIR)$(PREFIX)/bin/lina
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	cp forth.lab $(DESTDIR)$(PREFIX)/lib
