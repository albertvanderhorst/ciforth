( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ INCLUDE asgen.frt
\ INCLUDE asi586.frt

REQUIRE BOUNDS

\ ------------------------------------------------
\ Set BITS of mask in ADDRESS.
: OR!U >R R@ @ OR R> ! ;

\ Reset BITS of mask in ADDRESS.
: AND!U >R INVERT R@ @ AND R> ! ;
\ ------------------------------------------------


\ Maybe this part belogns in the .lab file.

'TASK >CFA @ CONSTANT DOCOL
'FORTH >CFA @ CONSTANT DODOES
'BASE  >CFA @ CONSTANT DOUSER
'RESULT >CFA @ CONSTANT DOVAR

\ ---------------------------------------------------------------------------
HEX
\ Data design : >FFA leavers the flag field that is considered
\ an area of 4 bytes. >FFA 3 + gives the stack effect nibbles:
\ high nibble : input:  1-- 0E depth popped +1
\ low nibble : output: 0 = unknown , 1-- 0E depth pushed +1
\ 0 = unknown , 0FH = variable.
\ A ``STACK EFFECT'' is a pair (input nibble, output nibble) with the above encoding.
\ A ``pure stack effect'' is a pair (depth popped, depth pushed).

10 CONSTANT FMASK-N@    \ No input side effects. "No fetches."
20 CONSTANT FMASK-N!    \ No output side effects. "No stores."
80 CONSTANT FMASK-IL    \ Data is following in line.

\ A set : #how far filled, data. See asgen.frt

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
: IN-SET? $@ SWAP
 DO DUP I @ = IF DROP -1 UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;

ASSEMBLER
CREATE POPS  HERE 0 ,
' POP,       ,          ' POPF,      ,          ' POP|ALL,   ,
' POP|DS,    ,          ' POP|ES,    ,          ' POP|FS,    ,
' POP|GS,    ,          ' POP|SS,    ,          ' POP|X,     ,
HERE SWAP !

CREATE PUSHES  HERE 0 ,
' PUSH,      ,          ' PUSHF,     ,          ' PUSHI|B,   ,
' PUSHI|X,   ,          ' PUSH|ALL,  ,          ' PUSH|CS,   ,
' PUSH|DS,   ,          ' PUSH|ES,   ,          ' PUSH|FS,   ,
' PUSH|GS,   ,          ' PUSH|SS,   ,
' PUSH|X,    ,
HERE SWAP !

\ Bookkeeping for pops and pushes.
VARIABLE #POPS          VARIABLE #PUSHES
: !PP    0 #POPS !    0 #PUSHES ! ;

\ Add the bookkeeping of pops and pushes for the latest instruction
\ dissassembled.
: COUNT-PP DISS CELL+ @
    DUP POPS IN-SET? #PUSHES @ IF #PUSHES +! ELSE NEGATE #POPS +! THEN
    PUSHES IN-SET? NEGATE #PUSHES +!   ;

\ Bookkeeping for input and output side effects.
VARIABLE PROTO-FMASK

\ Initialise to "no side effects". Innocent until proven guilty.
: !FMASK FMASK-N@ FMASK-N! OR PROTO-FMASK ! ;

\ Look whether we have the current disassembled instruction forces us to
\ revise the flag mask.
\ A forth flag has all bit set. Hence the idiom ``FLAG MASK AND''
\ instead of FLAG IF MASK ELSE 0 THEN''.
: REVISE-FMASK
    0
    'MOV|TA, DISS IN-SET? FMASK-N@ AND    OR
    'MOV|FA, DISS IN-SET? FMASK-N! AND    OR
    'R| DISS IN-SET? 0= IF
        'T| DISS IN-SET? FMASK-N@ AND   OR
        'F| DISS IN-SET? FMASK-N! AND   OR
        \ Memory (non-move) operations always fetch!
        'F| DISS IN-SET? 'MOV, DISS IN-SET? 0= AND  FMASK-N@ AND   OR
    THEN
    PROTO-FMASK AND!U
;

\ Accumulate information for an assembler definition from address ONE to
\ address TWO.
\ Count pushes and pops among the instructions.
\ Find out about `no input side effect' and `no output side effect'.
: ACCUMULATE-AS-INFO
    SWAP
    !PP !FMASK
    BEGIN
        (DISASSEMBLE) ^M EMIT   COUNT-PP   REVISE-FMASK
    2DUP > 0= UNTIL
    2DROP
;

PREVIOUS

\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  ASSEMBLER  LODS, X'|   JMPO, D0| [AX] ; PREVIOUS

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
\ Get also its side effect mask.
: ANALYSE-CODE >CFA @ DUP >NA ACCUMULATE-AS-INFO ;

\ For DEA return the stack effect BYTE and the side effect mask.
\ It must be a code definition.
: (FIND-SE-CODE) ANALYSE-CODE
  #POPS @ 1+   #PUSHES @ 1+   SE:2>1  PROTO-FMASK @ ;

\ --------------- DIRTY FIXME ------------------------------
\ For DEA return the stack effect BYTE.
\ FIll in the side effect mask.
\ It must be a code definition.
: FIND-SE-CODE DUP ANALYSE-CODE   PROTO-FMASK @ SWAP >FFA OR!U
  #POPS @ 1+   #PUSHES @ 1+   SE:2>1 ;

\ Irritating exceptions
0FF '?DUP !SE
0FF 'EXECUTE !SE
021 'FOR-VOCS !SE       \ Despite an execute this is known
031 'FOR-WORDS !SE

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
FMASK-IL    ' SKIP >FFA OR!U        FMASK-IL    ' BRANCH >FFA OR!U
FMASK-IL    ' 0BRANCH >FFA OR!U     FMASK-IL    ' (LOOP) >FFA OR!U
FMASK-IL    ' (+LOOP) >FFA OR!U     FMASK-IL    ' (DO) >FFA   OR!U
FMASK-IL    ' (?DO) >FFA  OR!U      FMASK-IL    ' LIT >FFA    OR!U


\ ---------------------------------------------------------------------------

\ Inspect POINTER and XT. If the xt is of a type followed by inline
\ code advance pointer appropriately. Inspect new POINTER and XT.
: ?INLINE? >R
    R@ >FFA @ FMASK-IL AND IF
        R@ 'LIT = OVER @ 0< OR IF   \ In line literal or back jump.
            CELL+
        ELSE
            $@ + ALIGNED
        THEN
    THEN R> ;

CREATE STOPPERS HERE 0 ,
' (;)        ,          ' (;CODE)    ,          ' DOES>      ,
HERE SWAP !

\ The DEA means : we ARE still in a chain.
: CHAIN? STOPPERS IN-SET? 0= ;

\ To a stack effect BYTE apply a CHAIN of high level code.
\ Return the resulting stack effect BYTE.
: ANALYSE-CHAIN
        BEGIN @+ ( DUP ID.) DUP CHAIN? WHILE ?INLINE? SWAP >R ADD-SE R> REPEAT 2DROP ;

\ For DEA return the stack effect BYTE.
\ It must be a high level definition
: FIND-SE-DOCOL 11 SWAP >DFA @ ANALYSE-CHAIN ;

\ For DEA return the stack effect BYTE.
\ It must be a ``CREATE .. DOES>'' definition
: FIND-SE-DODOES 12 SWAP >DFA @ @ ANALYSE-CHAIN ;

\ For DEA return the stack effect BYTE.
\ It can be any definition.
: FIND-SE-ANY
    DUP >CFA @ DOCOL = IF FIND-SE-DOCOL ELSE
    DUP >CFA @ DODOES = IF FIND-SE-DODOES ELSE
    FIND-SE-CODE
    THEN THEN ;

\ For DEA find the stack effect and fill it in.
\ It can be any definition.
\ Dummy headers are ignored.
: FILL-SE
    DUP >FFA @ 1 AND 0= IF \ Ignore dummy headers
        DUP FIND-SE-ANY SWAP !SE _
    THEN DROP ;

\ The number of entries with unknown stack effect.
VARIABLE #UNKNOWNS

: !UNKNOWNS 0 #UNKNOWNS ! ;

\ For DEA fill in the stack effect if it is not yet known.
: ?FILL-SE?   ( DUP ID. CR)
DUP SE@ 0=   IF   1 #UNKNOWNS +!   FILL-SE _   THEN   DROP ;

\ For a WID fill in all stack effects.
: FILL-SE-WID '?FILL-SE? SWAP FOR-WORDS ;

\ Sweep once through the base dictionary filling in stack effects.
\ There are three vocabularies. ``Forth'' is done partly, from ``TASK''.
\ ``ENVIRNONMENT'' is done in full. ``DENOTATION'' hangs off ``FORTH''.
\ Count the ``#UNKNOWNS''.
: FILL-ALL   !UNKNOWNS   'TASK FILL-SE-WID   'ENVIRONMENT >WID FILL-SE-WID ;

\ Go on
: FILL-ALL-SE 0 BEGIN FILL-ALL #UNKNOWNS @ SWAP OVER = UNTIL DROP ;

13 '(NUMBER) !SE
DECIMAL
