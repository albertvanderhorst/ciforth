( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ WARNING: HEX THROUGHOUT THIS WHOLE FILE !
                                HEX
\ The main part for analysing high level code.
\ This code is common to the initial analysis and analysis of high level
\ additions.

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

\ ----------------------
\ For DEA : it IS a real (non-dummy) header.
: REAL? >FFA @ 1 AND 0= ;

\ Fill optimisations BITS in into DEA.
: !OB   >FFA >R    R@ @ FMASK INVERT AND   OR  R> ! ;

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
\ code pointer is not pointing past the header.
\ There are several motivations for this lookup table:
\  1. CONSTANT's and VARIABLE's have the FMASK-NS optimisation which
\      doesn't follow from an automatic analysis.
\  2. Reanalysing for each same code field is a waste of time.
\  3. The analyser works (partly) independantly of the dissassembler.
\  4. If multi-threading is used the automatic analysis is right for
\      USER's, however normally they can have the FMASK-NS optimisation.
\ The user only needs to fill in the stack effect of his code words, to forego
\ the need of loading the assembler.
CREATE CODE-TYPES  HERE 0 ,
    FMASK-NS 0 1 COMPOSE-FLAGS   \ Content of flag field for all those types.
    DOCON , DUP ,   DOVAR , DUP ,  DOUSER , DUP ,
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
: FILL-ALL 0 BEGIN (FILL-ONCE) DUP .
#UNKNOWNS @ SWAP OVER = PROGRESS @ 0= AND UNTIL DROP ;

DECIMAL
