( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ Adds to the flag fields of Forth words with the stack effect and
\ optimisation data. See DATA DESIGN.

\ Assumptions made by the analyser:
\   1. In low level code pushes and pops are never by-passed with branches.
\   2. In low level code POP|ALL, and PUSH|ALL, are not used at all.
\   3. Variable stack effect is filled manually.
\   4. Once the stack effect has been filled in, it isn't touched.
\      This way a prefilled stack effect overrules the automated search.
\   5. In the same way optimisation bits can be overruled,
\        but only for code words.
\
\ Convention: ?xxx? works only have effect if needed.

\ ---------------- TO DO ---------------------------
\ The analyser must be in two parts, one that needs the assembler, one that doesn't.

\ WARNING: HEX THROUGHOUT THIS WHOLE FILE !
\ INCLUDE asgen.frt
\ INCLUDE asi586.frt

REQUIRE BOUNDS
REQUIRE BAG
REQUIRE IN-BAG?
REQUIRE ?CD

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
VARIABLE DUMMY 'DUMMY >CFA @ CONSTANT DOVAR
'BL >CFA @ CONSTANT DOCON

\ ---------------------------------------------------------------------------
HEX
\ ------------------------ DATA DESIGN ----------------------------------
\ >FFA leaves the flag field that is considered an area of 4 bytes.
\ >FFA 0 + gives the dummy, invisible, immediate and denotation bits.
\ bits 4..7 are available and used here.
\ >FFA 1 + : any bits set open up an optimisation opportunity.
\ >FFA 2 + reserved for the return stack effect nibbles.
\ >FFA 3 + gives the data stack effect nibbles:
\ high nibble : input:  1-- 0E depth popped +1
\ low nibble : output: 0 = unknown , 1-- 0E depth pushed +1
\ 0 = unknown , 0FH = variable.
\ A ``STACK EFFECT'' is a pair (input nibble, output nibble) with the above encoding.
\ A ``pure stack effect'' is a pair (depth popped, depth pushed).


0FF00 CONSTANT FMASK

\ ----------------------    ( From analyser.frt)
10 CONSTANT FMASK-IL    \ Data is following in line.
20 CONSTANT FMASK-HO    \ This definition has been high level optimised.
40 CONSTANT FMASK-HOB   \ This definition cannot be high level optimised.
80 CONSTANT FMASK-DUP   \ This is a duplicator, i.e. stack items consumed
                        \ are put back unchanged.

\ If 0 the side effects are absent.
100 CONSTANT FMASK-N@    \ No input side effects. "No fetches."
200 CONSTANT FMASK-N!    \ No output side effects. "No stores."
400 CONSTANT FMASK-ST    \ No stack side effects.
                        \ "No absolute stack reference or return stack."
800 CONSTANT FMASK-FDI   \ This definition has the `EDI'' register free. "No loop"

FMASK-N@ FMASK-N! OR FMASK-FDI OR CONSTANT FMASK-ES \ No side effects except stack.
FMASK-ES FMASK-ST OR CONSTANT FMASK-NS \ No side effects at all.

\ For DEA : it IS a real (non-dummy) header.
: REAL? >FFA @ 1 AND 0= ;

\ Fill optimisations BITS in into DEA.
: !OB   >FFA >R    R@ @ FMASK INVERT AND   OR  R> ! ;

\ Fill in the stack effect BYTE into the flag field of DEA.
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

\ Set of duplicators.
CREATE DUPS  HERE 0 ,
' DUP       ,           ' 2DUP ,
' OVER      ,           ' 2OVER ,
HERE SWAP !

\ Mark all duplicators as such.
: MARK-DUP  DUPS @+ SWAP ?DO   FMASK-DUP I @ >FFA OR!U   0 CELL+ +LOOP ;

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
\ A forth flag has all bit set. Hence the idiom ``FLAG MASK AND''
\ instead of FLAG IF MASK ELSE 0 THEN''.
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

PREVIOUS

\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  ASSEMBLER  LODS, X'|   JMPO, D0| [AX] ; PREVIOUS

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

\ --------------- DIRTY FIXME ------------------------------
\ For DEA fill in the stack effect byte and the side effect mask.
\ It must be a code definition.
: FILL-FLAG-CODE   >R   R@ ANALYSE-CODE   PROTO-FMASK @   R@ >FFA   OR!U
  #POPS @ 1+   #PUSHES @ 1+   SE:2>1   R> !SE ;

\ From FLAGS POPS PUSHES compose a flag field content and return IT.
: COMPOSE-FLAGS 1+ SWAP 1+ 4 LSHIFT OR 18 LSHIFT OR ;

\ Fill FLAGS POPS PUSHES in into DEA's flag field.
: !FLAGS >R COMPOSE-FLAGS R> >FFA ! ;

\ Irritating exceptions filled in by hand. Filling in the stack effect
\ (although it could be found automatically), prevents changes.
0FF '?DUP !SE
0FF 'EXECUTE !SE
0                     1  0 'FOR-VOCS  !FLAGS  \ Despite an execute this is known
0                     2  0 'FOR-WORDS !FLAGS
FMASK-NS FMASK-IL OR  0  1 'LIT       !FLAGS
FMASK-NS FMASK-IL OR  0  0 'SKIP      !FLAGS
FMASK-NS FMASK-IL OR  0  0 'BRANCH    !FLAGS
FMASK-NS FMASK-IL OR  1  0 '0BRANCH   !FLAGS
FMASK-ES FMASK-IL OR  0  0 '(LOOP)    !FLAGS
FMASK-ES FMASK-IL OR  1  0 '(+LOOP)   !FLAGS
FMASK-ES FMASK-IL OR  2  0 '(DO)      !FLAGS
FMASK-ES FMASK-IL OR  2  0 '(?DO)     !FLAGS
FMASK-ES              0  0 'LEAVE     !FLAGS
FMASK-ES              0  0 'EXIT      !FLAGS
FMASK-ES              0  1 'DSP@      !FLAGS
FMASK-ES              0  1 'DEPTH     !FLAGS
FMASK-ES              0  0 '.S    !FLAGS
FMASK-ES              1  0 '>R    !FLAGS
FMASK-ES              0  1 'R@    !FLAGS
FMASK-ES              0  1 'I     !FLAGS
FMASK-ES              0  1 'R>    !FLAGS
FMASK-ES              0  0 'RDROP !FLAGS

\ FILL IN EVERYTHING
\ Add to an existing pure STACK EFFECT the pure STACK EFFECT.
\ Return the combined pure STACK EFFECT.
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

\ Inspect POINTER and XT. If the xt is of a type followed by inline
\ code advance pointer as far as possible, remaining in the same chain.
\ Leave new POINTER and XT.
: ?INLINE? >R
    R@ >FFA @ FMASK-IL AND IF
        R@ 'LIT = OVER @ 0< OR IF   \ In line literal or back jump.
            CELL+
        ELSE
            $@ + ALIGNED
        THEN
    THEN R> ;

CREATE STOPPERS HERE 0 ,
' (;)        ,          ' (;CODE)    ,          ' DOES>      ,  ' EXIT    ,
HERE SWAP !

\ The DEA means : we ARE still in a chain.
: CHAIN? STOPPERS IN-BAG? 0= ;

\ To a stack effect BYTE apply a CHAIN of high level code.
\ Return the resulting stack effect BYTE.
: ANALYSE-CHAIN
        BEGIN @+ ( DUP ID.) DUP CHAIN? WHILE ?INLINE? SWAP >R ADD-SE R> REPEAT 2DROP ;

\ For DEA fill in the stack effect byte.
\ It must be a high level definition
: FILL-SE-DOCOL   >R   11   R@ >DFA @   ANALYSE-CHAIN   R> !SE ;

\ For DEA fill in the stack effect byte.
\ It must be a ``CREATE .. DOES>'' definition
: FILL-SE-DODOES   >R   12   R@ >DFA @ @   ANALYSE-CHAIN   R> !SE ;

\ A code definition ( not ``:'' and not ``DOES>'') may be a type if the
\ code pointer is not pointing past the header. Reanalysing is then a waste
\ of time, so that dea's can be looked up here.
\ Moreover the analyser now works independantly of the dissassembler, if the
\ user fills in the stack effect of his code words if any.
CREATE CODE-TYPES  HERE 0 ,
    FMASK-NS 0 1 COMPOSE-FLAGS   \ Content of flag field for all those types.
    DOCON , DUP ,   DOVAR , DUP ,   DOUSER , DUP ,
DROP    HERE SWAP !

\ For DEA fill in the stack effect byte and the side effect bits.
\ It can be any code definition.
: FILL-ANY-CODE >R
    R@ >CFA @ CODE-TYPES BAG-WHERE DUP IF
        CELL+ @ R> >FFA OR!U
    ELSE
        DROP R> FILL-FLAG-CODE  \ Normal code definition.
    THEN ;

\ For DEA find the stack effect and fill it in.
\ For code definitions also fill in the optimisation flags.
\ It can be any definition.
: FILL-SE-ANY
    DUP >CFA @ DOCOL = IF FILL-SE-DOCOL ELSE
    DUP >CFA @ DODOES = IF FILL-SE-DODOES ELSE
    FILL-ANY-CODE
    THEN THEN ;


\ Keep track of the number of entries with something unknown.
VARIABLE #UNKNOWNS      VARIABLE PROGRESS
: !UNKNOWNS 0 #UNKNOWNS !    0 PROGRESS ! ;

\ For DEA fill in the stack effect if it is not yet known.
: ?FILL-SE?   \ DUP ID. \ CR
DUP SE@ 0=   IF   1 #UNKNOWNS +!   FILL-SE-ANY _   THEN   DROP ;


\ Inspect POINTER and XT. If the xt is of a type followed by inline
\ code advance pointer to next high level code.
\ Leave new POINTER and XT.
: ?INLINE2? >R
    R@ >FFA @ FMASK-IL AND IF
        R@ 'SKIP = IF   \ In line string.
            $@ + ALIGNED
        ELSE
            CELL+
        THEN
    THEN R> ;


\ Add to a BYTE the fetch-store optimisation bits of DEA. Result a new BYTE.
: ADD-!@    >FFA @ AND ;

\ To a stack effect BYTE apply a CHAIN of high level code.
\ Return the resulting stack effect BYTE.
: ANALYSE-CHAIN2
        BEGIN @+ ( DUP ID.) DUP CHAIN? WHILE ?INLINE2? SWAP >R ADD-!@ R> REPEAT 2DROP ;

\ For DEA return the optimisation BITS.
\ It must be a high level definition
: FIND-OB-DOCOL   FMASK-NS SWAP >DFA @ ANALYSE-CHAIN2 ;

\ For DEA return the optimisation BITS.
\ It must be a ``CREATE .. DOES>'' definition
: FIND-OB-DODOES FMASK-NS SWAP >DFA @ @ ANALYSE-CHAIN2 ;

\ For DEA return the optimisation BITS.
\ It can be any definition, because non high level are ignored, they
\ have already been filled in.
: FIND-OB-ANY
    DUP >CFA @ DOCOL = IF FIND-OB-DOCOL ELSE
    DUP >CFA @ DODOES = IF FIND-OB-DODOES ELSE
       >FFA @
    THEN THEN ;

\ For DEA find the optimisation bits and fill it in.
\ It can be any definition.
\ Dummy headers are ignored.
: (FILL-OB) DUP FIND-OB-ANY SWAP !OB ;

\ For DEA fill in the opt bits and remember if there was a change.
: FILL-OB   \ DUP ID. \ CR
    DUP >FFA @ >R   DUP (FILL-OB)    >FFA @ R> <> PROGRESS OR!U ;

\ General part

\ For a DEA fill in the flag field.
: ?FILL?   DUP REAL? IF DUP FILL-OB  ?FILL-SE? _ THEN DROP ;

\ For all words belonging to WID fill in the whole flag field..
: FILL-WID DUP ID. >WID '?FILL? SWAP FOR-WORDS ;

\ Sweep once through all vocabularies filling in flag fields.
\ Keep track of progress.
: (FILL-ONCE)  !UNKNOWNS   'FILL-WID FOR-VOCS ;

\ Go on filling flag fields until the number of unknown stack effects no longer changes.
: (FILL-ALL) 0 BEGIN (FILL-ONCE) DUP .
#UNKNOWNS @ SWAP OVER = PROGRESS @ 0= AND UNTIL DROP ;

\ Fill in everything.
: FILL-ALL MARK-DUP (FILL-ALL) ;

DECIMAL
