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
\

\ A set : #elements , #elemets cells .

\ Fill in the STACK effect into the flag field of DEA.
: !SE >FFA 3 + C! ;

\ Type the interpretation of a stack effect NIBBLE.
: .SE/2 DUP 0 = IF "unknown" TYPE ELSE DUP 0F = IF "variable" TYPE
ELSE BASE @ >R DECIMAL 1 - . R> BASE ! _ THEN THEN DROP ;


\ Type the stack effect of DEA.
: .SE >FFA 3 + C@ DUP 4 RSHIFT .SE/2 " -- " TYPE 0F AND .SE/2 ;

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

\ Count pushes and pops among the instrructions from address ONE to
\ address TWO.
: COUNT-PPS !PP
    SWAP POINTER !
    BEGIN (DISASSEMBLE) ^M EMIT COUNT-PP POINTER @ OVER < 0= UNTIL
    DROP
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


\ Get the pops and pushes from DEA m which must be a code definition.
: ANALYSE-CODE >CFA @ DUP >NA COUNT-PPS ;

\ For DEA find the stack effect and fill it in.
: FILL-SE DUP ANALYSE-CODE
  #POPS @ 1+ 4 LSHIFT #PUSHES @ 1+ OR SWAP !SE ;

\ Irritating exceptions
2F '?DUP !SE
3F 'DIGIT !SE

\ FILL IN EVERYTHING

13 '(NUMBER) !SE
DECIMAL
