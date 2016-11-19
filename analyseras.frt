( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ ------------------ assembler stuff here -----------------------------------

\ WARNING: HEX THROUGHOUT THIS WHOLE FILE !

WANT IN-BAG?

                                HEX

ASSEMBLER
CREATE POPS  HERE 0 ,
' POP,       ,          ' POPF,      ,
' POP|DS,    ,          ' POP|ES,    ,          ' POP|FS,    ,
' POP|GS,    ,          ' POP|SS,    ,          ' POP|X,     ,
HERE SWAP !

CREATE PUSHES  HERE 0 ,
' PUSH,      ,          ' PUSHF,     ,          ' PUSHI|B,   ,
' PUSHI|X,   ,                                  ' PUSH|CS,   ,
' PUSH|DS,   ,          ' PUSH|ES,   ,          ' PUSH|FS,   ,
' PUSH|GS,   ,          ' PUSH|SS,   ,
' PUSH|X,    ,
HERE SWAP !

\ Instruction that have an input side effect.
\ Two operand instructions are handled directly, via ``T|'' ``F|''
CREATE FETCHES HERE 0 ,
' MOV|TA,    ,          ' LODS,      ,
' SCAS,      ,          ' CMPS,      ,          ' MOVS,      ,
' OUTS,      ,          ' INS,       ,          ' OUT|D,     ,
' IN|D,      ,          ' OUT|P,     ,          ' IN|P,      ,
' SCAS,      ,          ' INT,       ,
HERE SWAP !

\ Instruction that have an output side effect.
\ Two operand instructions are handled directly, via ``T|'' ``F|''
CREATE STORES HERE 0 ,
' MOV|FA,    ,                                  ' STOS,      ,
                                                ' MOVS,      ,
' OUTS,      ,          ' INS,       ,          ' OUT|D,     ,
' IN|D,      ,          ' OUT|P,     ,          ' IN|P,      ,
                        ' INT,       ,
HERE SWAP !


\ Instruction that use EDI. Normally this is not done.
\ But some (string) operations require it.
CREATE EDIERS HERE 0 ,
                                                ' STOS,      ,
' SCAS,      ,          ' CMPS,      ,          ' MOVS,      ,
HERE SWAP !

\ Bookkeeping for pops and pushes.
VARIABLE #POPS          VARIABLE #PUSHES
: !PP    0 #POPS !    0 #PUSHES ! ;

\ After a call to ``(DISASSEMBLE)'' return the OPCODE.
: OPCODE    DISS CELL+ @ ;

\ Add the bookkeeping of pops and pushes for the latest instruction
\ dissassembled.
: COUNT-PP
    OPCODE POPS IN-BAG? #PUSHES @ IF #PUSHES +! ELSE NEGATE #POPS +! THEN
    OPCODE PUSHES IN-BAG? NEGATE #PUSHES +!   ;

\ Bookkeeping for input and output side effects.
VARIABLE PROTO-FMASK

\ Initialise to "no side effects". Innocent until proven guilty.
: !FMASK FMASK-NS PROTO-FMASK ! ;

\ A forth flag has all bit set. Hence the idiom ``FLAG MASK AND''
\ instead of FLAG IF MASK ELSE 0 THEN''. Also note that in Stallman
\ convention a capitalised verb means a (forth) flag.

\ Add to FLAGS if instruction IS storing, the no output side effects flag.
\ Return the new FLAGS. (So those are the flags to become invalid.)
: IS-STORING       FMASK-N! AND    OR ;

\ Add to FLAGS if instruction IS looping, i.e. it uses register IDE.
\ Return the new FLAGS. (So those are the flags to become invalid.)
: IS-LOOPING       FMASK-FDI AND    OR ;

\ Add to FLAGS if instruction IS fetching, the no input side effects flag.
\ Return the new FLAGS. (So those are the flags to become invalid.)
: IS-FETCHING   FMASK-N@ AND    OR ;

\ Look whether we have the current disassembled instruction forces us to
\ revise the flag mask.
: REVISE-FMASK
    0
    OPCODE STORES IN-BAG? IS-STORING
    OPCODE FETCHES IN-BAG? IS-FETCHING
    OPCODE EDIERS IN-BAG?  IS-LOOPING
    'R| DISS IN-BAG? 0= IF
        'T| DISS IN-BAG?   IS-FETCHING
        'F| DISS IN-BAG?   IS-STORING
        \ Memory (non-move) operations always fetch!
        'F| DISS IN-BAG?    'MOV, OPCODE <>    AND  IS-FETCHING
    THEN
    \ Overrule : operations on the return stack don't count.
    '[BP] DISS IN-BAG? IF DROP 0 THEN
    PROTO-FMASK AND!U
;

\ Accumulate information for an assembler definition from address ONE to
\ address TWO.
\ Count pushes and pops among the instructions.
\ Find out about `no input side effect' and `no output side effect'.
: ACCUMULATE-AS-INFO
    SWAP
    !PP !FMASK
    BEGIN 2DUP > WHILE
        (DISASSEMBLE) ^M EMIT   COUNT-PP   REVISE-FMASK
    REPEAT
    2DROP
;


\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  LODS, X'|   JMPO, ZO| [AX] ;

PREVIOUS

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
\ Get also its side effect mask.
: ANALYSE-CODE >CFA @ DUP >NA ACCUMULATE-AS-INFO ;

\ For DEA fill in the stack effect byte and the side effect mask.
\ It must be a code definition.
: FILL-FLAG-CODE   >R   R@ ANALYSE-CODE
    PROTO-FMASK @ #POPS @ #PUSHES @ R> !FLAGS ;

\ ------------------------------------------------

DECIMAL
