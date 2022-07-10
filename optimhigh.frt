( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optimhigh.frt,v 5.16 2020/03/01 11:03:13 albert Exp $)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.
\ The basic idea is to copy a word at HERE meanwhile expanding
\ and correcting its constituents, recursively.
\ In the end we have a sequence of low level words with only inline after
\ the words identified in analyser.frt :
\ LIT SKIP BRANCH 0BRANCH

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

WANT BAG DO-BAG BOUNDS :F 2>R NULL
WANT 0<>  <=  >= ,,

\ ----------------------    keeping track of the stack ----------------------

\ The virtual stack depth, maintained while parsing.
VARIABLE VD

\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE OPT-START

: !OPT-START   HERE OPT-START !   0 VD ! ;

\ There has been made progress during the last optimisation.
VARIABLE IMPROVED

\ For STACKEFFECTNIBBLE : it IS no good, because it is unknown or variable.
: NO-GOOD DUP $F = SWAP 0= OR ;

\ For STACKEFFECTBYTE : it IS good, neither unknown or variable.
: SE-GOOD DUP $F AND NO-GOOD SWAP 4 RSHIFT NO-GOOD OR  NOT? ;

\ For STACKEFFECTBYTE : we KNOW we have enough pops, i.e. after applying
\ this stackeffect the virtual stack doesn't underflow.
: ENOUGH-POPS   DUP SE-GOOD   SWAP 4 RSHIFT 1- VD @ <=  AND ;

\ For DEA and FLAG : all optimisations required by flag ARE allowed there.
: ALLOWED?   >R   >FFA @   R@ AND   R> = ;

\ For DEA : it HAS no side effects regards stack.
: NSST?   FMASK-ST ALLOWED? ;

\ For DEA : it HAS no side effects regards store (or stack.)
: NS!? FMASK-N! FMASK-ST OR   ALLOWED? ;

\ For DEA : it HAS no side effects, input or output (or stack).
: NS?   FMASK-NS ALLOWED? ;

\ The return stack is not handled in any way.   FIXME!

\ The minimum step depth we have encountered.
VARIABLE MIN-DEPTH

\ Keep a record of how LOW the stack has been.
: REMEMBER-DEPTH VD @   MIN-DEPTH @   MIN MIN-DEPTH ! ;

: !MIN-DEPTH   VD @  MIN-DEPTH ! ;

\ Combine a STACKEFFECTBYTE into ``VD''.
\ Remember : both nibbles have offset 1!
: COMBINE-VD    SE:1>2 SWAP 1- NEGATE VD +! REMEMBER-DEPTH 1- VD +! ;

\ ----------------------    Annihilaton ----------------------

\ For DEA return "it CAN be part of annihilatable code",
\ as far as its stack & side effects are concerned.
: ANNILABLE? DUP NS!?   SWAP SE@ NO-GOOD NOT? AND ;

\ We are at a stable point, i.e. we consumed all the extra stuff,
\ that is placed in the annihilation chain. Maybe even more.
: ANNIL-STABLE?   VD @ MIN-DEPTH @ =  VD @ 1 <  AND ;

\ For ADDRESS and XT : it IS a branch going backwards.
: BACK-BRANCH?  IS-A-BRANCH IF CELL- @ 0 < ELSE DROP 0 THEN ;

\ Replace an ADDRESS after a branch code by: the branch TARGET.
: PASTBRANCH>TARGET   CELL- >TARGET ;

\ If FLAG , do a throw to indicate we can't optimise.
: KAPUT??   4002 ?ERROR ;

\ The highest address inspected during annihilation and the corresponding
\ stack depth.
VARIABLE MAX-ANNIL   VARIABLE MAX-ANNIL-DEPTH
: !MAX-ANNIL  0 MAX-ANNIL ! ;

\ Post current annil POINTER and virtual depth to ``MAX-ANNIL''.
: POST-ANNIL MAX-ANNIL ! VD @ MAX-ANNIL-DEPTH ! ;

\ Swap the current POINTER and virtual depth with the ``MAX-ANNIL'', leave the
\ old ``MAX-ANNIL'' address.
: SWAP-ANNIL  MAX-ANNIL @ MAX-ANNIL-DEPTH @ 2>R POST-ANNIL 2R> VD ! ;

\ Compare annihilation POINTER and current vd with the maximums above.
\ If they were not filled in, do that now. Return pointer WHERE and
\ "we MUST go on, because ``MAX-ANNIL'' is higher.''
: MUST-HIGHER?
    MAX-ANNIL @ 0= IF DUP POST-ANNIL 0 ELSE
    DUP MAX-ANNIL @ = IF VD @ MAX-ANNIL-DEPTH @ <> KAPUT?? 0 ELSE
    DUP MAX-ANNIL @
      > IF SWAP-ANNIL THEN -1
\   <
    THEN THEN ;


\ For POINTER in the current situation, return WHERE to go on,
\ and "we MUST go on".
: FINISHED? MUST-HIGHER? NOT? ANNIL-STABLE? AND ;

\ Investigate the start of SEQUENCE. Return the ADDRESS
\ to which it can be annihilated, else throw.
: (ANNIHILATE-SEQ)
    NEXT-PARSE 0= KAPUT??
    DUP ANNILABLE? NOT? KAPUT??
    2DUP BACK-BRANCH? KAPUT??
    DUP 'BRANCH = IF SWAP PASTBRANCH>TARGET SWAP THEN
    DUP SE@ COMBINE-VD
    '0BRANCH = IF
        VD @
        OVER PASTBRANCH>TARGET FINISHED? NOT? IF RECURSE THEN DROP
        VD !
    THEN
    FINISHED? NOT? IF RECURSE THEN
;

\ Investigate the start of SEQUENCE. Leave END of that
\ sequence plus a FLAG whether it can be annihilated
\ without considering jumps into the middle of this code.
: ANNIHILATE-SEQ? !OPT-START !MIN-DEPTH !MAX-ANNIL
    '(ANNIHILATE-SEQ) CATCH  4002 <> ;

\ For GAP and virtual depth calculate ``GAP-OFFSET'' such as used
\ in ``CORRECT-GAP'' .
: CALCULATE-ANNIL-OFFSET - VD @ CELLS - GAP-OFFSET ! ;

\ Fill the gap at START with ``DROP'' s.
: FILL-WITH-DROPS   DUP VD @ NEGATE CELLS +   'DROP   WFILL ;

\ Annihilate the GAP i.e. replace it by the equivalent number of ``DROP''.
\ Assure `bblocks reflects the new situation.
\ Return the new position of END (where we have to go on optimising.).
: ANNIHILATE-GAP
    2DUP CALCULATE-ANNIL-OFFSET
    CORRECT-GAP
\     2DUP " Between " TYPE SWAP H. " and " TYPE H.
\     " we can replace with " TYPE SPACE VD @ NEGATE   . " DROPS. " TYPE CR
      SWAP FILL-WITH-DROPS
;

\ Investigate the start of SEQUENCE. If it can be anihilated do it.
\ Always leave the new SEQUENCE be it just
\ after the annihilitee or bumped by one item.
: ANNIHILATE-ONE
\     DUP @ IS-A-DROP IF NEXT-ITEM ELSE
    DUP ANNIHILATE-SEQ? 0= IF DROP NEXT-ITEM ELSE
    2DUP FORBIDDEN-GAP? 1 > IF DROP NEXT-ITEM ELSE
\     ANNIHILATE-GAP THEN THEN THEN ;
     ANNIHILATE-GAP THEN THEN ;

\ Annihilate as much as possible from SEQUENCE.
\ Return recompiled SEQUENCE (which is in fact the same address.)
: ANNIHILATE DUP   ( DUP fill-bblocks)
    BEGIN DUP ?NOT-EXIT WHILE ANNIHILATE-ONE REPEAT DROP
;

\ ---------------------------------------------------------------------

\ For DEA we are still in the swappable code, i.e. we don't dig below
\ the constant stack entries, and we have no stack side effects that kill.
\ At this point ``VD'' contains the remaining stack depth, i.e. the
\ number of constants not touched by the swappable code. Those can be
\ placed after the swappable code.
: STILL-REORDERING? DUP NSST? SWAP SE@ ENOUGH-POPS AND ;

\ We are at an stable point. i.e. we consumed all the constants,
\ we may have replaced ourselves. Or we can't swap anyway.
: STABLE? VD @ MIN-DEPTH @ = ( VD @ 1 < AND) ;

\ For DEA : adding it would result in a not yet stable sequence.
\ Otherwise the optimisation is known to end here or there is no optimisation.
: NOT-YET-STABLE?
DUP STILL-REORDERING? IF SE@ COMBINE-VD STABLE? NOT? ELSE -1 MIN-DEPTH !
DROP 0 THEN ;

\ From ADDRESS collect all code to be executed before (some of the) constants
\ collected. Leave ADDRESS LENGTH (in address units.)
: FIND-REORDER !MIN-DEPTH
    DUP BEGIN NEXT-PARSE SWAP NOT-YET-STABLE? AND WHILE REPEAT
     SWAP OVER <>   STABLE? AND   VD @ 0<> AND
;

\ Assuming there has been folding, increment SEQUENCE to point past all
\ constants at its start. Return incrementer SEQUENCE.
: COUNT-LIT !OPT-START BEGIN DUP @ 'LIT = WHILE 2 CELLS + 1 VD +! REPEAT ;

\ For a reorderable GAP return the first part as string.
: FIRST-PART   DROP VD @ CELLS ;

\ Reorder a reorderable GAP .
     DATA _PAD 1000 ALLOT
: REORDER-PIECES
( B D) >R DUP VD @ 2 * CELLS + R>
( C) OVER >R
    OVER - _PAD $!
    DUP R>   OVER -   _PAD $+!
    _PAD $@ >R SWAP R> MOVE ;

: FORBIDDEN-REORDER?
    2DUP FORBIDDEN-GAP? >R
    FIRST-PART OVER + FORBIDDEN-GAP? R> OR
;

\ Try to reorder one gap at the start of a SEQUENCE, leave the incremented
\ SEQUENCE.
: REORDER-ONE
    DUP @ 'LIT <> IF NEXT-ITEM ELSE
    DUP COUNT-LIT DUP ?NOT-EXIT NOT? IF DROP NEXT-ITEM ELSE
    FIND-REORDER 0= IF DROP NEXT-ITEM ELSE
    2DUP FORBIDDEN-REORDER? IF DROP NEXT-ITEM ELSE
    DUP >R REORDER-PIECES R>  TRUE IMPROVED !
    THEN THEN THEN THEN
;
\ Reorder a SEQUENCE to delay constants as much as possible.
\ Return rearranged SEQUENCE (which is in fact the same address.)
\ It is required that the `bblocks set has been filled.
: REORDER   DUP ( DUP FILL-bblocks)
    BEGIN DUP ?NOT-EXIT WHILE REORDER-ONE REPEAT DROP ;

\ ---------------------------------------------------------------------

\ Initialise the stack tracking for folding.
: !FOLD-START   0 VD ! ;

\ Combining the effect of DEA into the current state, return
\ "the folding optimisation still HOLDS".
: CAN-FOLD?   >R
   R@ NS?    R@ SE@ ENOUGH-POPS AND    R@ IS-A-BRANCH NOT? AND
RDROP ;

\ For START return the END of the largest GAP starting there that can be folded.
: FIND-FOLD BEGIN DUP NEXT-PARSE >R  DUP CAN-FOLD? >R
    OVER IS-A-BRANCH-TARGET NOT?   R> AND   R> AND WHILE
SE@ COMBINE-VD  SWAP DROP REPEAT 2DROP ;

\ For GAP and virtual depth calculate ``GAP-OFFSET'' such as used
\ in ``CORRECT-GAP'' .
: CALCULATE-FOLD-OFFSET - VD @ CELLS 2* + GAP-OFFSET ! ;

\ Fill the GAP with constants calculated via a NONAME_XT (See ``COMPILE-GAP'').
\ The return stack is used because downward loops don't handle the zero case
\ gracefully.
: FILL-WITH-CONSTANTS
    DSSWAP   SWAP >R >R   !CSP EXECUTE-GAP
    BEGIN R> R> 2DUP <> WHILE >R 2 CELLS - >R
        'LIT R@ !  R@ CELL+ ! REPEAT
    DROP ?CSP DROP
;

\ For a foldable GAP, replace it with constants. Leave the new END of
\ the gap where optimisation continues.
: FOLD-GAP 2DUP COMPILE-GAP >R
           2DUP CALCULATE-FOLD-OFFSET
           CORRECT-GAP
           2DUP R> FILL-WITH-CONSTANTS
           SWAP DROP
;

\ Try to reorder one gap at the start of a SEQUENCE, leave the incremented
\ SEQUENCE.
: FOLD-ONE
    !FOLD-START
    DUP FIND-FOLD
    2DUP = IF DROP NEXT-ITEM ELSE
       FOLD-GAP
    THEN
;

\ Fold a SEQUENCE i.e. replace code with constants where possible.
\ Return rearranged SEQUENCE (which is in fact the same address.)
\ It is required that the `bblocks set has been filled.
: FOLD   DUP BEGIN DUP ?NOT-EXIT WHILE FOLD-ONE REPEAT DROP ;

\ ----------------------------------------------------------------

20 CONSTANT STRIDE       \ # of cells between entries in MATCH-TABLE

\ Add as many noops to the dictionary to have it aligned at an stride*cell
\ boundary. Add at least one.
\ This assumes an alligned high level Forth.
: ALIGN-NOOPS   BEGIN   POSTPONE NOOP   HERE STRIDE CELLS MOD WHILE   REPEAT ;
IMMEDIATE

\ Place holder
CREATE P

\ \ Create the code sequence for a literal with a place holder.
\ : 'P       ['] LIT COMPILE, ['] P COMPILE, ; IMMEDIATE
\ This looks more portable, but is it?


\ Create a table of optimisations :
\  ``STRIDE CELLS'' code pattern  | ``STRIDE CELLS'' optimised replacement |
\ The first pattern to start with NOOP is the end.
\ Not intended to be executed, but patterns are to be matched
\ from this definitions, and pieces to be copied out.
\ ``P'' is used as a placeholder for a data item to be copied to the
\ optimised side. Several P's must be replaced in order, even if escaped
\ by a ``LIT''
\ The replacement code must not be longer, such that it can always be copied
\ into a gap. It is padded with noop's.
\ If a real noop is needed use ``NOP1''
'NOOP ALIAS NOP1        \ The same, except for the xt.

    'ALIGN-NOOPS ALIAS |                         \ Convenience alias.
    : ]L ] POSTPONE LITERAL ; IMMEDIATE \ Convenience alias.

: (MATCH-TABLE)                         |
\ `` MATCH-TABLE'' points here :
'P  EXECUTE             | P                        | \ Execute optimisation
'P  + 'P  +             | 'P  'P  + +              | \ Associativity optimisation
'P 'P  D+ 'P 'P D+      | 'P 'P  'P  'P  D+ D+     | \ Associativity optimisation
'P  + 'P  -             | 'P  'P  - +              |
'P  - 'P  +             | 'P  'P  - -              |
'P  - 'P  -             | 'P  'P  + -              |
'P  @ 'P  !             | NOOP                     |
'P  ! 'P  @             | DUP 'P !                 |
'P  M* DROP 'P  M* DROP | 'P  'P  M* DROP M* DROP  | \ Invalid if last drop removed!
'P  OR 'P  OR           | 'P  'P  OR OR            |
'P  AND 'P  AND         | 'P  'P  AND AND          |
'P  XOR 'P  XOR         | 'P  'P  XOR XOR          |
[ 0 ]L +                | NOOP                     | \ Shortcut evalutions
[ 0 ]L -                | NOOP                     |
[ 0 ]L M* DROP          | DROP 0                   |
[ 0 ]L OR               | NOOP                     |
[ 0 ]L AND              | DROP 0                   |
[ 0 ]L XOR              | NOOP                     |
[ 1 ]L M* DROP          | NOOP                     |
[ 1 ]L /                | NOOP                     |
[ -1 ]L M* DROP         | NEGATE                   |
[ -1 ]L /               | NEGATE                   |
[ -1 ]L OR              | DROP -1                  |
[ -1 ]L AND             | NOOP                     |
[ -1 ]L XOR             | INVERT                   |
'P  LSHIFT 'P  LSHIFT   | 'P  'P  + LSHIFT         | \ Distributivity optimisation
'P  RSHIFT 'P  RSHIFT   | 'P  'P  + RSHIFT         |
[ 0 ]L 0BRANCH [ 'P , ] | NOP1 NOP1 BRANCH [ 'P , ] | \ Branch optimisation
'P 0BRANCH [ 'P , ]     | NOOP                     | \ Non-zero, zero is matched by previous
BRANCH [ 0 , ]          | NOOP                     |
0BRANCH [ 0 , ]         | DROP                     |
< 0=                    | 1+ >                     |
> 0=                    | 1- <                     |
>R R>                   | NOOP                     |
R> >R                   | NOOP                     |
;

\ Optimalisation of this table is thoroughly forbidden!
FMASK-HOB '(MATCH-TABLE) >FFA OR!
\ Get rid of those auxiliary words.
'| HIDDEN       ']L HIDDEN

\ Here is your table
' (MATCH-TABLE) >DFA @ CELL+   STRIDE CELLS /MOD   SWAP 0<> -   STRIDE CELLS *
CONSTANT MATCH-TABLE

\ \ Portability note.
\ \ You could have the table like this :
\ 'LIT , P , '+ ,    'LIT , P , '+ , ALIGN-NOOPS
\     'LIT , P ,    'LIT , P , '+ , '+ , ALIGN-NOOPS
\ \ and do POSTPONE's of items. This is still not portable however.


\ ----------------------------------------------------------------
STRIDE BAG PEES
: !PEES PEES !BAG ;
\ ----------------------------------------------------------------
\ For a SEQUENCE and an ENTRY in ``MATCH-TABLE'' :
\ Return the STRING to be copied into the gap, else two zeros.
\ It is matched in length to the gap, but may end in noop's.
\ Return the MATCH itself, or two zeros.
\ As a side effect, remember the place holders.
: ?MATCH    !PEES
    STRIDE 0 DO
        DUP [I] 'NOOP = IF SWAP DROP STRIDE CELLS + I CELLS LEAVE THEN       \ Success
        DUP [I] 'P = IF
            OVER [I] PEES BAG+!
        ELSE OVER [I] OVER [I] <> IF
            2DROP 0 0 LEAVE                                     \ Failure
        THEN THEN
    LOOP
;

\ Match any entry of the table to SEQUENCE.
\ Return the STRING to be copied into the gap, else two zeros.
: ?MM   MATCH-TABLE
    BEGIN    2DUP ?MATCH DUP IF 2SWAP 2DROP EXIT THEN 2DROP
        STRIDE 2 * CELLS + DUP ?NOT-EXIT WHILE REPEAT
    2DROP 0 0 ;

\ If ADDRESS contains a place holder, replace it by the next placeholder data.
: ?PEE? DUP @ 'P = IF PEES BAG+@ SWAP ! _ THEN DROP ;

\ Replace code at SEQUENCE with STRING , filling in the place holders.
\ Leave the END of the replaced string (where matching must continue.)
: COPY-MATCH   >R   OVER R@ MOVE
    !PEES DUP R@ BOUNDS DO I ?PEE? 0 CELL+ +LOOP
    R> + TRUE IMPROVED !
;

\ For SEQUENCE : copy its first item to ``HERE'' possibly
\ replacing it by a match optimisation.
\ Leave sequence BEGIN' of what is still to be handled.
:  MATCH-ONE
        DUP ?MM DUP 0= IF   2DROP NEXT-ITEM   ELSE
        >R OVER R@ OVER + FORBIDDEN-GAP? 1 > IF RDROP DROP NEXT-ITEM ELSE R>
            COPY-MATCH
        THEN THEN
;

\ Optimise a SEQUENCE using pattern matching.
\ Return optimised SEQUENCE (which is in fact the same address.)
\ It is required that the `bblocks set has been filled.
: MATCH   DUP BEGIN DUP ?NOT-EXIT WHILE MATCH-ONE REPEAT DROP ;

\ ----------------------    Branch shortcuts, dead code  -------------------------------

: ?DEAD-CODE? ( not yet) ;

\ The branch (loop whatever) at ADDRESS jumps to a ``BRANCH''.
\ Shortcut it.
: SHORTEN-BRANCH CELL+ >R   R@ >TARGET CELL+ >TARGET
    R@ CELL+ -   R> ! ;

\ If possible, shortcut the branch at the start of SEQUENCE .
: ?SHORTEN-BRANCH?
BEGIN DUP CELL+ >TARGET @ 'BRANCH = WHILE DUP SHORTEN-BRANCH REPEAT DROP ;

\ Try to apply branch optimisation to the start of a SEQUENCE .
\ Always leave the new SEQUENCE bumped by one item.
: BRANCH-SPECIAL-ONE
    DUP @ 'BRANCH = IF ( DUP ?DEAD-CODE?) THEN
    DUP @ IS-A-BRANCH IF DUP ?SHORTEN-BRANCH? THEN
    NEXT-ITEM
;

\ Expand some special xt's from SEQUENCE. In partical ``LEAVE''.
\ Return recompiled SEQUENCE (which is in fact the same address.)
\ It is required that the `bblocks set has been filled.
: DEAD-CODE DUP BEGIN DUP ?NOT-EXIT WHILE BRANCH-SPECIAL-ONE REPEAT DROP ;

\ ----------------------------------------------------------------
\ Optimise DEA by expansion plus applying optimations to the expanded code.
: OPT-EXPAND   >DFA DUP @  |^|
EXPAND-PLAIN |^|
BEGIN FALSE IMPROVED !
    FOLD |^| MATCH |^| REORDER |^| ANNIHILATE |^| DEAD-CODE |^|
IMPROVED @ WHILE REPEAT |^|
SWAP ! ;

\ For DEA remember that it has been optimised
: !OPTIMISED   FMASK-HO SWAP >FFA OR!  ;

\ For DEA : it IS eligable for high level optimisation.
: H-OPTIMISABLE?  DUP HIGH-LEVEL?  SWAP  >FFA @ FMASK-HO AND 0=  AND ;

\ Try and optimise the DEA with respect HL inlining.
\ Reach trough to underlying levels first.
:F inline&fold ;
:R inline&fold
    DUP H-OPTIMISABLE? IF
        DUP >DFA @ BEGIN NEXT-PARSE WHILE inline&fold REPEAT 2DROP
        DUP OPT-EXPAND
    THEN
     DUP  ?FILL-SE?    DUP  FILL-OB    !OPTIMISED ;
