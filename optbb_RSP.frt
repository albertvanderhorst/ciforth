( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_RSP.frt,v 5.1 2020/07/14 22:18:45 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.

\ This file contains optimisation related to the return stack,
\ in particular caching return stack items in registers.
\ If sufficient registers are present the return stack may be eliminated
\ from the code.

\ ----------------------------------------------------------------
\ \ FIXME this patch cannot handle immediate data
\ \ the nops land at the wrong place.
\
VARIABLE my-offset
: patch-RSP-bufc   ( target -- )
        bufc 3 + DUP L@ 2/ 2/ 2/ >R  0 SWAP L!
        bufc BP-AXg TOGGLE \ Change to MOV, R|  NOPS,
        R> permanent-WHERE? @ unpack 2DROP NIP
\         DUP bufc put2-reg   permanent-registers SET+
        bufc put2-reg
        bufc$ $@   >R SWAP R>  MOVE
;
REGRESS HERE Q: MOV, X| F| BX'| XO| [BP] 0 L,  HERE OVER - bufc$ $! S:

\ Patch a  gap  containing a [BP] addressing, possibly replacing with
\ AX and NOP's. Leave the  end  of the gap.
: patch-RSP  2DUP OVER - bufc$ $!
    'LEA, PDISS BAG-WHERE IF
        OVER bufc$ @ NOP@ FILL
     ELSE
\      &* EMIT bufc 3 + L@ 2/ 2/ 2/ permanent-WHERE? @ unpack . . . . CR
    bufc 3 + L@ 2/ 2/ 2/ permanent-WHERE? @ unpack NIP NIP NIP 0= IF
        OVER bufc$ @ NOP@ FILL
    ELSE
        OVER patch-RSP-bufc
    THEN THEN
    NIP ;

REGRESS init-RSP  13 add-read S:
REGRESS HERE Q: MOV, X| F| BX'| XO| [BP] 13 CELLS L,  HERE patch-RSP S: HERE

: report-RSP "Report about return stack usage" TYPE CR
    "new report " TYPE CR
    permanent-properties DO-BAG I @ unpack >R >R >R . R> . R> . R> . CR
    LOOP-BAG
;

\ The  adr  points after an instruction that addresses via the
\ return stack pointer [BP]. Analyse that instruction and leave  adr  .
    : get-BP-offset 4 - L@ 2/ 2/ 2/ ;
: analyse-return-ins
   'LEA, PDISS BAG-WHERE IF 1 RPO-LEA's +! ELSE
   'T| PDISS BAG-WHERE IF DUP get-BP-offset add-read ELSE
   'F| PDISS BAG-WHERE IF
        DUP get-BP-offset add-write
        'MOV, PDISS BAG-WHERE NOT? IF ( transput)
             DUP get-BP-offset add-read
        THEN
    THEN  THEN  THEN
;

\ For  range  return tally into `permanent-registers how
\ many times offsets are used as source and as destination.
: analyse-return-one
    SWAP BEGIN 2DUP > WHILE
        ~DISASSEMBLE
        '[BP] PDISS BAG-WHERE IF analyse-return-ins THEN
    REPEAT 2DROP
;

\ Optimise the  range  , with the information in `permanent-registers etc.
\ All code is changed in place
: optimise-RSP-one    \ RPO-LEA's @ 2 <> IF 2DROP EXIT THEN
    SWAP BEGIN 2DUP > WHILE
          DUP >R ~DISASSEMBLE
          '[BP] PDISS BAG-WHERE IF
             R> SWAP patch-RSP
          ELSE R> DROP THEN
   REPEAT
   2DROP
;


\ : optimise-RSP-one  ." BEFORE RSP" 2DUP 2DUP D-R optimise-RSP-one
\    ." AFTER RSP " D-R ;
