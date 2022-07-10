# $Id: optim.mak,v 5.3 2022/03/01 22:14:07 albert Exp $
# Copyright(2022): Albert van der Horst, HCC FIG Holland by GNU Public License

# Highly experimental, ignore for basic distribution.



seesim : seesim.frt optimiser
	optimiser < seesim.frt

sieve64.bin : seesim.frt optimiser
	optimiser < seesim.frt

sieve : sieve.asm
	lina -c sieve.asm

sieve64 : sieve64.asm optimiser
	optimiser $<

sieve64.dis : sieve64.bin sieve64.cul
	ciasdis64 -d sieve64.bin sieve64.cul  > sieve64.proto
	echo "     NEXT" >>sieve64.proto
	echo END-CODE  >>sieve64.proto
	sievefilter sieve64.proto $@

citesta64 : citesta64.frt sieve64.asm lina-ana64
	lina-ana64 -c $<
