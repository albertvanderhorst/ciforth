( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ WARNING: HEX THROUGHOUT THIS WHOLE FILE !
                                HEX
\ Debugging aids for analyser.frt. Print the information added to flag fields.

WANT CRACK

\ Type the interpretation of a stack effect NIBBLE.
: .SE/2 DUP 0 = IF "unknown" TYPE ELSE DUP 0F = IF "variable" TYPE
ELSE BASE @ >R DECIMAL 1 - . R> BASE ! _ THEN THEN DROP ;

\ Type the stack effect BYTE.
: .SE   SE:1>2 "( " TYPE SWAP .SE/2 "-- " TYPE .SE/2 &) EMIT ;

\ For DEA type its optimisation and other properties.
: OPT? >FFA @
    CR "Optim. bits etc.: " TYPE
    DUP FMASK     AND 0= IF "No optimisations " TYPE THEN
    DUP FMASK-N@  AND IF "No fetches " TYPE THEN
    DUP FMASK-N!  AND IF "No stores " TYPE THEN
    DUP FMASK-ST  AND IF "No depth " TYPE THEN
    DUP FMASK-IL  AND IF "In line data " TYPE THEN
    DUP FMASK-HO  AND IF "Been optimised " TYPE THEN
    DUP FMASK-HOB AND IF "Cannot be optimised " TYPE THEN
    DUP FMASK-FDI AND IF "Loop index is available " TYPE THEN
    DUP FMASK-DUP AND IF "A duplicator" TYPE THEN
    DROP
;

\ For DEA type its stack effect.
: SE?   SE@ .SE ;

\ For DEA type everything.
: .DE DUP SE? DUP CRACKED OPT? ;

DECIMAL
