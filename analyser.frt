( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ INCLUDE asgen.frt
\ INCLUDE asi586.frt

REQUIRE BOUNDS

HEX
\ Data design : >FFA leavers the flag field that is considered
\ an area of 4 bytes. >FFA 3 + gives the stack effect nibbles:
\ high nibble : input:  1-- 0E depth popped +1
\ low nibble : output: 0 = unknown , 1-- 0E depth pushed +1
\ 0 = unknown , 0FH = variable.
\ A ``STACK EFFECT'' is a pair (input nibble, output nibble) with the above encoding.
\ A ``pure stack effect'' is a pair (depth popped, depth pushed).

\ A set : #elements , #elemets cells .

\ Fill in the STACK effect into the flag field of DEA.
: !SE >FFA 3 + C! ;

\ For DEA return its stack effect BYTE.
: SE@ >FFA 3 + C@ ;

\ Type the interpretation of a stack effect NIBBLE.
: .SE/2 DUP 0 = IF "unknown" TYPE ELSE DUP 0F = IF "variable" TYPE
ELSE BASE @ >R DECIMAL 1 - . R> BASE ! _ THEN THEN DROP ;

\ Split a BYTE into a STACK EFFECT .
: SE:1>2 DUP 4 RSHIFT SWAP 0F AND ;

\ Combine the STACK EFFECT into one BYTE.
: SE:2>1 0F AND SWAP 4 LSHIFT OR ;

\ Type the stack effect BYTE.
: .SE   SE:1>2 "( " TYPE SWAP .SE/2 "-- " TYPE .SE/2 &) EMIT ;

\ For DEA type its stack effect.
: SE?   SE@ .SE ;

\ For VALUE and SET : value IS present in set.
: IN-SET? $@ CELLS BOUNDS
 DO DUP I @ = IF DROP -1 UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;

ASSEMBLER
CREATE POPS  HERE 0 ,
' POP,       ,          ' POPF,      ,          ' POP|ALL,   ,
' POP|DS,    ,          ' POP|ES,    ,          ' POP|FS,    ,
' POP|GS,    ,          ' POP|SS,    ,          ' POP|X,     ,
HERE OVER - 0 CELL+ / 1- SWAP !

CREATE PUSHES  HERE 0 ,
' PUSH,      ,          ' PUSHF,     ,          ' PUSHI|B,   ,
' PUSHI|X,   ,          ' PUSH|ALL,  ,          ' PUSH|CS,   ,
' PUSH|DS,   ,          ' PUSH|ES,   ,          ' PUSH|FS,   ,
' PUSH|GS,   ,          ' PUSH|SS,   ,
' PUSH|X,    ,
HERE OVER - 0 CELL+ / 1- SWAP !


\ Bookkeeping for pops and pushes.
VARIABLE #POPS          VARIABLE #PUSHES
: !PP    0 #POPS !    0 #PUSHES ! ;


\ Add the bookkeeping of pops and pushes for the latest instruction
\ dissassembled.
: COUNT-PP DISS CELL+ @
    DUP POPS IN-SET? NEGATE #POPS +!
    PUSHES IN-SET? NEGATE #PUSHES +!   ;

\ Count pushes and pops among the instructions from address ONE to
\ address TWO.
: COUNT-PPS !PP
    SWAP   BEGIN (DISASSEMBLE) ^M EMIT COUNT-PP 2DUP > 0= UNTIL   2DROP
;

PREVIOUS

\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  ASSEMBLER  LODS, X'|   JMPO, D0| [AX] ; PREVIOUS

\ The common content of all high level definitions.
\ Catch before ``NEXT'' is optimised!
'NEXT @ CONSTANT DOCOL

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

\ The sequence of bytes that forms next, as a string.
\ (This means you can $@ it.)
CREATE NEXT-IDENTIFICATION 0 , NEXT
HERE NEXT-IDENTIFICATION CELL+ -   NEXT-IDENTIFICATION !
\ \D ." Expect NEXT-IDENTIFICATION to contain NEXT :" CR
\ \D : DUMP-STRING
\ \D     OVER H. SPACE DUP H. DUMP ;
\ \D  NEXT-IDENTIFICATION $@ DUMP-STRING

\ For some code at ADDRESS, find the start ADDRESS of its ``NEXT''.
: >NA   BEGIN DUP NEXT-IDENTIFICATION $@ CORA WHILE 1+ REPEAT ;


\ Get the pops and pushes from DEA which must be a code definition.
: ANALYSE-CODE >CFA @ DUP >NA COUNT-PPS ;

\ For DEA return the stack effect BYTE.
\ It must be a code definition.
: FIND-SE-CODE ANALYSE-CODE
  #POPS @ 1+   #PUSHES @ 1+   SE:2>1   ;

\ Irritating exceptions
0FF '?DUP !SE
0FF 'DIGIT !SE

\ FILL IN EVERYTHING
\ Add to an existing pure STACK EFFECT the pure STACK EFFECT. Return the combined
\ pure STACK EFFECT.
: COMBINE-PSE >R - DUP 0< IF - 0 THEN R> + ;

\ Combine two STACK EFFECT's. Return the resulting STACK EFFECT.
: COMBINE-SE
    1- >R 1- >R 1- >R 1 - >R
    R> R> R> R> COMBINE-PSE
    1+ >R 1+ >R
    R> R>
;
\ Combine stack effect bytes ONE and TWO. Result a NEW byte.
: COMBINE-BYTES
    2DUP 0= SWAP 0= OR IF 2DROP 0 ELSE
    2DUP 0FF = SWAP 0FF = OR IF 2DROP 0FF ELSE
    >R SE:1>2 R> SE:1>2 COMBINE-SE SE:2>1 THEN THEN ;

\ Add to a BYTE the stack effect of DEA. Result a new BYTE.
: ADD-SE    SE@   COMBINE-BYTES ;

\ ---------------------------------------------------------------------------

\ MAYBE THIS PART BELONGS IN optimiser.frt

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

80 CONSTANT IMASK   \ Data is following in line.

IMASK    ' SKIP >FFA OR!        IMASK    ' BRANCH >FFA OR!
IMASK    ' 0BRANCH >FFA OR!     IMASK    ' (LOOP) >FFA OR!
IMASK    ' (+LOOP) >FFA OR!     IMASK    ' (DO) >FFA   OR!
IMASK    ' (?DO) >FFA  OR!      IMASK    ' LIT >FFA    OR!

\ ---------------------------------------------------------------------------

\ Maybe this part belogns in the .lab file.

'TASK >CFA @ CONSTANT DOCOL
'FORTH >CFA @ CONSTANT DODOES

\ ---------------------------------------------------------------------------

\ Inspect POINTER and XT. If the xt is of a type followed by inline
\ code advance pointer appropriately. Inspect new POINTER and XT.
: ?INLINE? >R
   R@ >FFA @ IMASK AND IF
    R@ 'LIT = IF CELL+ ELSE $@ + ALIGNED THEN
   THEN R> ;

\ To a stack effect BYTE apply a CHAIN of high level code.
\ Return the resulting stack effect BYTE.
: ANALYSE-CHAIN
        BEGIN @+ DUP '(;) <> WHILE ?INLINE? SWAP >R ADD-SE R> REPEAT 2DROP ;

\ For DEA return the stack effect BYTE.
\ It must be a high level definition
: FIND-SE-DOCOL 11 SWAP >DFA @ ANALYSE-CHAIN ;

\ For DEA return the stack effect BYTE.
\ It must be a ``CREATE .. DOES>'' definition
: FIND-SE-DODOES 12 SWAP >DFA @ @ ANALYSE-CHAIN ;

\ For DEA return the stack effect BYTE.
\ It can be any definition.
: FIND-SE-ANY DUP >CFA @ DOCOL = IF FIND-SE-DOCOL ELSE
              DUP >CFA @ DODOES = IF FIND-SE-DODOES ELSE
              FIND-SE-CODE THEN THEN ;


\ For DEA find the stack effect and fill it in.
\ It can be any definition.
: FILL-SE DUP FIND-SE-ANY SWAP !SE ;

13 '(NUMBER) !SE
DECIMAL
