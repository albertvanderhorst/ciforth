( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

REQUIRE $
REQUIRE SWAP-DP

( ------------ DEBUGGING ------------------------------------- )
      : \D POSTPONE \ ; IMMEDIATE    : ^^ ;
\ : \D ; IMMEDIATE : ^^ &: EMIT &< EMIT ^ DUP CRACK-CHAIN &> EMIT &; EMIT ;

\ REQUIRE SET (not yet)
( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------)
( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: SET+@   DUP >R @ @ 0 CELL+ R> +! ;   ( retract from SET. Leave ITEM )
: .SET   @+ SWAP ?DO I ? 0 CELL+ +LOOP ;   ( Print non-empty SET )
\ Remove entry at ADDRESS from SET.
: SET-REMOVE   >R   DUP CELL+ SWAP  R@ @ OVER -   MOVE   -1 CELLS R> +! ;

\ For VALUE and SET : value IS present in set.
: IN-SET? $@ SWAP
 ?DO DUP I @ = IF DROP -1 UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;

\ For a SET print it backwards. Primarily intended as how to loop backwards example.
: SET-PRINT-BACKWARDS
@+ SWAP BEGIN 2DUP > WHILE   >R 1 CELLS - >R
    R@ ?
R> R> REPEAT 2DROP ;

\ Fill from ADDRESS to END a number of cells with CONTENT.
: WFILL   ROT ROT SWAP ?DO DUP I !   0 CELL+ +LOOP DROP ;

\ ---------------------- Handling double numbers on return stack -----------------

: D>R POSTPONE >R POSTPONE >R ; IMMEDIATE
: DR> POSTPONE R> POSTPONE R> ; IMMEDIATE
: DR@ POSTPONE DR> POSTPONE 2DUP POSTPONE D>R ; IMMEDIATE

\ Swap a SINGLE and a DOUBLE. Leave the DOUBLE and the SINGLE.
: SDSWAP   ROT ;
\ Swap a DOUBLE and a SINGLE. Leave the SINGLE and the DOUBLE.
: DSSWAP   ROT ROT ;

\ ----------------------    ( From optimiser.frt)
\ Store a STRING with hl-code in the dictionary.
: HL-CODE, HERE OVER ALLOT SWAP CMOVE ;
\ Store a LOW HIGH range with hl-code in the dictionary.
: HL-RANGE, OVER - HL-CODE, ;

\ Compile a GAP in a temporary area. Return a POINTER to the
\ unnamed code.
: COMPILE-GAP   SWAP-DP HERE DSSWAP HL-RANGE, POSTPONE (;) SWAP-DP ;

\ Execute the CODE from a gap such as returned by ``COMPILE-GAP''.
\ This leaves a variable number of stack items.
: EXECUTE-GAP >R ;

\ Recompile the code from BEGIN END. Leave END as the new begin.
: >HERE SWAP 2DUP -  HL-CODE, ;

\ Make the code from BEGIN END empty, by leaving END END.
: EMPTY>  SWAP DROP DUP ;

\ For POINTER : it POINTS not yet to an ``EXIT''.
: ?NOT-EXIT   @ '(;) = 0= ;
\ For POINTER : it POINTS not yet to a ``NOOP''.
: ?TILL-NOOP   @ 'NOOP = 0= ;

\ For a parse ADDRESS return an INCREMENTED parse address, the
\ DEA (content of ``ADDRESS'') and a  FLAG : this IS_NOT the
\ end of definition.
: NEXT-PARSE
   @+ >R   R@ CFA> >FFA @ FMASK-IL AND IF
       R@ CFA> 'SKIP = IF @+ + ALIGNED ELSE CELL+ THEN
   THEN
   R@
\  R@ CFA> ID.          \ For desperado debugging.
   R> '(;) <> ;

\ ----------------------    MISCELLANEOUS

\ For an ITEM in a high level word, return the next ITEM.
\ So it skips also ``ITEM'' 's inline data.
: NEXT-ITEM NEXT-PARSE 2DROP ;

\ For a SEQUENCE return its end, i.e. just after where `` (;) '' sits.
: END-OF-SEQUENCE BEGIN NEXT-PARSE WHILE DROP REPEAT DROP ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ For a soft FLAG, returns a FORTH flag (-1/0)
: 0<>   0= 0= ;

\ For NUMBER1 and NUMBER2 : "``NUMBER!'' IS smaller or equal."
: <= > 0= ;

\ For NUMBER1 and NUMBER2 : "``NUMBER!'' IS greater or equal."
: >= < 0= ;

\ ----------------------    keeping track of the stack ----------------------


\ The virtual stack depth, maintained while parsing.
VARIABLE VD


\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE OPT-START

: !OPT-START   HERE OPT-START !   0 VD ! ;

\ There has been made progress during the last optimisation.
VARIABLE PROGRESS            : !PROGRESS 0 PROGRESS ! ;

\ For STACKEFFECTNIBBLE : it IS no good, because it is unknown or variable.
: NO-GOOD DUP $F = SWAP 0= OR ;

\ For STACKEFFECTBYTE : it IS good, neither unknown or variable.
: SE-GOOD DUP $F AND NO-GOOD SWAP 4 RSHIFT NO-GOOD OR  0= ;

\ For STACKEFFECTBYTE : we KNOW we have enough pops, i.e. after applying
\ this stackeffect the virtual stack doesn't underflow.
: ENOUGH-POPS   DUP SE-GOOD   SWAP 4 RSHIFT 1- VD @ > 0=  AND ;

\ For DEA FLAG : all optimisations required by flag ARE allowed there.
\ The meaning of the st flag is reversed, this is a design error to
\ be fixed in analsyer.frt
: ALLOWED?   >R   >FFA @ FMASK-ST XOR  R@ AND   R> = ;

\ For DEA : it HAS no side effects regards stack.
: NSST?   FMASK-ST ALLOWED? ;

\ For DEA : it HAS no side effects regards store (or stack.)
: NS!? FMASK-N! FMASK-ST OR   ALLOWED? ;

\ For DEA : it HAS no side effects, input or output (or stack).
: NS?   FMASK-NS ALLOWED? ;

\ Move to analyser.frt    FIXME!
FMASK-ST '.S >FFA OR!
FMASK-ST 'DEPTH >FFA OR!

\ The minimum step depth we have encountered.
VARIABLE MIN-DEPTH

\ Keep a record of how LOW the stack has been.
: REMEMBER-DEPTH VD @   MIN-DEPTH @   MIN MIN-DEPTH ! ;

: !MIN-DEPTH   VD @  MIN-DEPTH ! ;

\ Combine a STACKEFFECTBYTE into ``VD''.
\ Remember : both nibbles have offset 1!
: COMBINE-VD    SE:1>2 SWAP 1- NEGATE VD +! REMEMBER-DEPTH 1- VD +! ;

\ ----------------------    Keeping track of branching ----------------------

\ In the following a GAP is a pair START END with START inclusive and END
\ exclusive.

\ For DEA : it IS a branch, i.e. it is followed by a relative control target.
: IS-A-BRANCH   DUP 'LIT <>   SWAP >FFA @ FMASK-IL AND 0= 0= AND ;

CREATE DROPS  HERE 0 ,
' DROP       ,          ' 2DROP      ,
HERE SWAP !

\ For DEA : it IS a trivial annihilator.
: IS-A-DROP DROPS IN-SET? ;

\ The set of addresses where a branch offset is stored.
50 SET BRANCHES
: !BRANCHES   BRANCHES !SET ;

\ For a POSITION of a branch offset, find the target.
: >TARGET   @+ + ;

\ For START if there is some branch at ADDRESS add it to ``BRANCHES''
: FILL-ONE-BRANCH DUP @ IS-A-BRANCH IF
    CELL+   BRANCHES SET+!
    _ THEN DROP ;

\ For a SEQUENCE fill the ``BRANCHES'' set.
: FILL-BRANCHES !BRANCHES BEGIN DUP FILL-ONE-BRANCH NEXT-PARSE WHILE DROP REPEAT 2DROP ;

\ BRANCH and TARGET is free with respect to GAP, i.e. this jump is either
\ totally outside or totally inside the GAP.
: FREE-WRT?
    >R 2DUP = IF 2DROP DROP RDROP -1 EXIT THEN R> \ You may jump to the start of a gap always!
    D>R
    SWAP DR@ WITHIN              \ BRANCH inside
    SWAP DR> WITHIN  \ Target Inside
    =                        \ Same
;

\ For a GAP : it IS forbidden, i.e. there is some branch crossing the gap boundary.
: FORBIDDEN-GAP?
BRANCHES @+ SWAP ?DO
    I @ DUP >TARGET 2OVER FREE-WRT? 0= IF 2DROP 0. LEAVE THEN
0 CELL+ +LOOP OR 0= ;

\ For an ADDRESS : it is the TARGET of a branch.
: IS-A-BRANCH-TARGET
BRANCHES @+ SWAP ?DO
    DUP I @ >TARGET = IF DROP -1 LEAVE THEN
0 CELL+ +LOOP   -1 = ;

\ ----------------------    Closing a gap -------------------

\ The offset over which the gap is shifted shut, generally negative.
VARIABLE GAP-OFFSET

\ For GAP and POSITION of a branch offset, adjust if it jumps from left
\ over the gap. ``GAP-OFFSET'' must have been filled in.
: ADJUST-BRANCH-FROM-LEFT    >R   R@ >TARGET <=   SWAP R@ >   AND IF
    GAP-OFFSET @ R@ +!
THEN RDROP ;

\ For GAP and POSITION of a branch offset, adjust if it jumps from right
\ over the gap. ``GAP-OFFSET'' must have been filled in.
: ADJUST-BRANCH-FROM-RIGHT    >R   R@ <  SWAP R@ >TARGET >=   AND IF
    GAP-OFFSET @ NEGATE R@ +!
THEN RDROP ;

\ The set of branches that is marked for elimination from the set ``BRANCHES''.
50 SET MARKED-BRANCHES
: !MARKED-BRANCHES  MARKED-BRANCHES !SET ;

\ For GAP and ADDRESS of entry in branches, if the branch is taken from inside
\ the gap, mark it for elimination from the table.
\ We can't remove them from the set right away because things get entangled.
: ELIMINATE-BRANCH-IN-GAP   >R   R@ @ DSSWAP WITHIN IF
    R@ MARKED-BRANCHES SET+!
THEN RDROP ;

\ Delete from ``BRANCHES'' what is marked for elimination.
\ Must go back because otherwise we would disturb the later addresses.
: DELETE-MARKED-BRANCHES MARKED-BRANCHES @+
   BEGIN 2DUP < WHILE   1 CELLS -   DUP @ BRANCHES SET-REMOVE   REPEAT 2DROP ;

\ For GAP adjust all branches sitting in ``BRANCHES'' and the set itself.
: ADJUST-BRANCHES !MARKED-BRANCHES  BRANCHES @+ SWAP ?DO
    2DUP I @ ADJUST-BRANCH-FROM-LEFT
    2DUP I @ ADJUST-BRANCH-FROM-RIGHT
    2DUP I ELIMINATE-BRANCH-IN-GAP
0 CELL+ +LOOP 2DROP ;

\ For END of gap, shift the remainder to close the gap.
: SHIFT-GAP-SHUT
    DUP END-OF-SEQUENCE OVER - >R   DUP GAP-OFFSET @ +  R>   MOVE ;

\ Correct the branch-addresses higher than the START of a gap,
\ to reflect the position they have after closing the gap.
: MOVE-BRANCHES   BRANCHES @+ SWAP ?DO
    DUP I @ < IF GAP-OFFSET @   I +! THEN
0 CELL+ +LOOP DROP ;

\ Correct the GAP, i.e. correct its end with ``GAP-OFFSET''.
\ The gaps content becomes invalid.
\ Adjust all branches over the gap and information about branches.
\ It is assumed that the gap is part of a sequence that ends at ``HERE'',
\ such that the dictionary may be adjusted.
\ Return the new GAP.
: CORRECT-GAP
    2DUP ADJUST-BRANCHES   DELETE-MARKED-BRANCHES   OVER MOVE-BRANCHES
    DUP SHIFT-GAP-SHUT
    \ Correct the end. The start is the same.
    GAP-OFFSET @ DUP ALLOT +
;


\ ----------------------    Expansion  ----------------------
\ the new expansion is supposed to take care of branches.

\ The set of shifts ; Each pairs is and ADDRESS SHIFT.
\ All branches-parts (start or targets) higher than address are to be
\ offset by the corresponding shift. These offsets are cumulative.
50 SET SHIFTS     : !SHIFTS   SHIFTS !SET ;

\ A GAP to be copied contains some branching.
: REMEMBER-BRANCH   HERE SHIFTS SET+!   SWAP - SHIFTS SET+! ;

: COPY-ONE IS-A-BRANCH IF HERE CELL+ BRANCHES SET+!  THEN >HERE ;

\ For a forward branch at ADDRESS do all the corrections found in ``SHIFTS''.
\ Note that this accumulates changes, and the decision to apply a correction
\ depends on order and previous corrections applied.
: CORRECT-ONE-BRANCH-FORWARD
SHIFTS @+ SWAP ?DO
    I @    OVER    DUP >TARGET    WITHIN IF I CELL+ @ OVER +! THEN
2 CELLS +LOOP DROP ;

\ For an ENTRY in ``SHIFTS'' return the highest ADDRESS in the expanded area.
: >END-SHIFT DUP @   SWAP CELL+ @  + ;

\ For a backward branch at ADDRESS do all the corrections found in ``SHIFTS''.
\ Note that this accumulates changes, and the decision to apply a correction
\ depends on order and previous corrections applied.
\ For a backward branch, ``SHIFTS'' must be inspected nackwards.
: CORRECT-ONE-BRANCH-BACKWARD
SHIFTS
@+ SWAP BEGIN 2DUP > WHILE   >R 2 CELLS - >R
    R@ >END-SHIFT   OVER DUP >TARGET SWAP   WITHIN IF R@ CELL+ @ NEGATE OVER +! THEN
R> R> REPEAT 2DROP DROP ;

\ Correct all branches.
: CORRECT-BRANCHES
BRANCHES @+ SWAP ?DO
    I @ DUP @ 0< IF CORRECT-ONE-BRANCH-BACKWARD ELSE CORRECT-ONE-BRANCH-FORWARD THEN
0 CELL+ +LOOP ;

\ Copy the SEQUENCE of high level code to ``HERE'' .
\ Do not initialise, or terminate.
: (EXPAND-ONE) DUP >R HERE >R
    BEGIN DUP NEXT-PARSE WHILE  COPY-ONE REPEAT
    R> SHIFTS SET+!
    2DROP
    R> CELL+ -   SHIFTS SET+!
;

\ For GAP and DEA : expand GAP to ``HERE'' filling in ``SHIFTS''.
\ Leave the END of the gap.
: EXPAND-ONE
    DUP HIGH-LEVEL? IF >DFA @ (EXPAND-ONE) SWAP DROP
    ELSE COPY-ONE THEN ;

\ Expand the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
\ Do not initialise, or terminate.
: (EXPAND)   BEGIN DUP NEXT-PARSE WHILE EXPAND-ONE REPEAT 2DROP DROP ;

\ Expand each constituent of SEQUENCE to ``HERE'' .
\ Leave a POINTER to equivalent linearised code.
: EXPAND HERE SWAP    !SHIFTS   (EXPAND) CORRECT-BRANCHES POSTPONE (;)  ;

\ ----------------------    Annihilaton ----------------------

\ For DEA return "it CAN be part of annihilatable code",
\ as far as its stack & side effects are concerned.
: ANNILABLE? DUP NS!?   SWAP SE@ NO-GOOD 0= AND ;

\ We are at a stable point, i.e. we consumed all the extra stuff,
\ that is placed in the annihilation chain. Maybe even more.
: ANNIL-STABLE?   VD @ MIN-DEPTH @ =  VD @ 1 <  AND ;

\ For DEA : adding it would result in a not yet stable sequence.
\ Otherwise the optimisation is known to end here with or without
\ possibility for optimisation.
: ANNILLING? DUP ANNILABLE? IF SE@ COMBINE-VD ANNIL-STABLE? 0= ELSE DROP 0 THEN ;

\ For ADDRESS and XT : it IS a branch going backwards.
: BACK-BRANCH?  IS-A-BRANCH IF 1 CELLS - @ 0 < ELSE DROP 0 THEN ;

\ Investigate the start of SEQUENCE. Return the ADDRESS
\ to which it can be annihilated, else 0.
: (ANNIHILATE-SEQ)
    BEGIN
        NEXT-PARSE OVER ANNILABLE? AND 0= IF 2DROP 0 EXIT THEN
        2DUP BACK-BRANCH? IF 2DROP 0 EXIT THEN
        DUP 'BRANCH = IF SWAP 1 CELLS - @+ + SWAP THEN
        DUP '0BRANCH = IF
            SE@ COMBINE-VD
            DUP VD @ >R RECURSE VD @ R> VD ! >R
            SWAP 1 CELLS - @+ + RECURSE
            OVER <>
            R> VD @ <> OR IF DROP 0 THEN
        EXIT THEN
    ANNILLING?  WHILE REPEAT ;

\ Investigate the start of SEQUENCE. Leave END of that
\ sequence plus a FLAG whether it can be annihilated
\ without considering jumps into the middle of this code.
: ANNIHILATE-SEQ? !OPT-START !MIN-DEPTH (ANNIHILATE-SEQ)
    DUP 0<> ANNIL-STABLE? AND ;

\ For GAP and virtual depth calculate ``GAP-OFFSET'' such as used
\ in ``CORRECT-GAP'' .
: CALCULATE-ANNIL-OFFSET - VD @ CELLS - GAP-OFFSET ! ;

\ Fill the gap at START with ``DROP'' s.
: FILL-WITH-DROPS   DUP VD @ NEGATE CELLS +   'DROP   WFILL ;

\ Annihilate the GAP i.e. replace it by the equivalent number of ``DROP''.
\ Assure ``BRANCHES'' reflects the new situation.
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
    DUP @ IS-A-DROP IF NEXT-ITEM ELSE
    DUP ANNIHILATE-SEQ? 0= IF DROP NEXT-ITEM ELSE
    2DUP FORBIDDEN-GAP? IF DROP NEXT-ITEM ELSE
    ANNIHILATE-GAP THEN THEN THEN ;

\ Annihilate as much as possible from SEQUENCE.
\ Return recompiled SEQUENCE (which is in fact the same address.)
: ANNIHILATE DUP   DUP FILL-BRANCHES
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
DUP STILL-REORDERING? IF SE@ COMBINE-VD STABLE? 0= ELSE -1 MIN-DEPTH !
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
: REORDER-PIECES
( B D) >R DUP VD @ 2 * CELLS + R>
( C) OVER >R
    OVER - PAD $!
    DUP R>   OVER -   PAD $+!
    PAD $@ >R SWAP R> MOVE ;

: FORBIDDEN-REORDER?
    2DUP FORBIDDEN-GAP? >R
    FIRST-PART OVER + FORBIDDEN-GAP? R> OR
;

\ Try to reorder one gap at the start of a SEQUENCE, leave the incremented
\ SEQUENCE.
: REORDER-ONE
    DUP @ 'LIT <> IF NEXT-ITEM ELSE
    DUP COUNT-LIT DUP ?NOT-EXIT 0= IF DROP NEXT-ITEM ELSE
    FIND-REORDER 0= IF DROP NEXT-ITEM ELSE
    2DUP FORBIDDEN-REORDER? IF DROP NEXT-ITEM ELSE
    DUP >R REORDER-PIECES R>  -1 PROGRESS +!
    THEN THEN THEN THEN
;
\ Reorder a SEQUENCE to delay constants as much as possible.
\ Return rearranged SEQUENCE (which is in fact the same address.)
\ It is required that the ``BRANCHES'' set has been filled.
: REORDER   DUP ( DUP FILL-BRANCHES)
    BEGIN DUP ?NOT-EXIT WHILE REORDER-ONE REPEAT DROP ;

\ ---------------------------------------------------------------------

\ Initialise the stack tracking for folding.
: !FOLD-START   0 VD ! ;

\ Combining the effect of DEA into the current state, return
\ "the folding optimisation still HOLDS".
: CAN-FOLD?   >R
   R@ NS?    R@ SE@ ENOUGH-POPS AND    R@ IS-A-BRANCH 0= AND
RDROP ;

\ For START return the END of the largest GAP starting there that can be folded.
: FIND-FOLD BEGIN DUP NEXT-PARSE >R  DUP CAN-FOLD? >R   OVER IS-A-BRANCH-TARGET 0=
    R> AND   R> AND WHILE
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
\ It is required that the ``BRANCHES'' set has been filled.
: FOLD   DUP BEGIN DUP ?NOT-EXIT WHILE FOLD-ONE REPEAT DROP ;

\ ----------------------------------------------------------------

\ Belongs in analyser.frt
HEX
800 CONSTANT FMASK-SP    \ Special optimisation possible. Pattern.
DECIMAL
\ ----------------------------------------------------------------

10 CONSTANT STRIDE       \ # of cells between entries in MATCH-TABLE

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

    'ALIGN-NOOPS ALIAS |                         \ Convenience alias.
    : ]L ] POSTPONE LITERAL ; IMMEDIATE \ Convenience alias.

: (MATCH-TABLE)                         |
\ `` MATCH-TABLE'' points here :
'P  EXECUTE             | P                       |       \ Execute optimisation
'P  + 'P  +             | 'P  'P  + +             |       \ Associativity optimisation
'P  + 'P  -             | 'P  'P  - +             |
'P  - 'P  +             | 'P  'P  - -             |
'P  - 'P  -             | 'P  'P  + -             |
'P  M* DROP 'P  M* DROP | 'P  'P  M* DROP M* DROP |       \ Invalid if last drop removed!
'P  OR 'P  OR           | 'P  'P  OR OR           |
'P  AND 'P  AND         | 'P  'P  AND AND         |
'P  XOR 'P  XOR         | 'P  'P  XOR XOR         |
[ 0 ]L +                | NOOP                    |       \ Shortcut evalutions
[ 0 ]L -                | NOOP                    |
[ 0 ]L M* DROP          | DROP 0                  |
[ 0 ]L OR               | NOOP                    |
[ 0 ]L AND              | DROP 0                  |
[ 0 ]L XOR              | NOOP                    |
[ 1 ]L M* DROP          | NOOP                    |
[ 1 ]L /                | NOOP                    |
[ -1 ]L M* DROP         | NEGATE                  |
[ -1 ]L /               | NEGATE                  |
[ -1 ]L OR              | DROP -1                 |
[ -1 ]L AND             | NOOP                    |
[ -1 ]L XOR             | INVERT                  |
'P  LSHIFT 'P  LSHIFT   | 'P  'P  + LSHIFT        |       \ Distributivity optimisation
'P  RSHIFT 'P  RSHIFT   | 'P  'P  + RSHIFT        |
;

FMASK-SP 'EXECUTE >FFA OR!
FMASK-SP '+ >FFA OR!
FMASK-SP '- >FFA OR!
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
STRIDE SET PEES
: !PEES PEES !SET ;
\ ----------------------------------------------------------------

\ From ARRAY fetch element Index. Return IT.
: [] CELLS + @ ;

\ From ARRAY fetch element ``I''. Return IT.
\ To be used within a loop
: [I] POSTPONE I POSTPONE [] ; IMMEDIATE

\ For a SEQUENCE and an ENTRY in ``MATCH-TABLE'' :
\ Return the STRING to be copied into the gap, else two zeros.
\ It is matched in length to the gap, but may end in noop's.
\ Return the MATCH itself, or two zeros.
\ As a side effect, remember the place holders.
: ?MATCH    !PEES
    STRIDE 0 DO
        DUP [I] 'NOOP = IF SWAP DROP STRIDE CELLS + I CELLS LEAVE THEN       \ Success
        DUP [I] 'P = IF
            OVER [I] PEES SET+!
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
: ?PEE? DUP @ 'P = IF PEES SET+@ SWAP ! _ THEN DROP ;

\ Replace code at SEQUENCE with STRING , filling in the place holders.
\ Leave the END of the replaced string (where matching must continue.)
: COPY-MATCH   >R   OVER R@ MOVE
    !PEES DUP R@ BOUNDS DO I ?PEE? 0 CELL+ +LOOP
    R> +
;

\ For SEQUENCE : copy its first item to ``HERE'' possibly
\ replacing it by a match optimisation.
\ Leave sequence BEGIN' of what is still to be handled.
:  MATCH-ONE
        DUP ?MM DUP 0= IF   2DROP NEXT-ITEM   ELSE
        >R OVER R@ OVER + FORBIDDEN-GAP? IF RDROP DROP NEXT-ITEM ELSE R>
            COPY-MATCH -1 PROGRESS !
        THEN THEN
;

\ Find optimisation patterns in the SEQUENCE of high level code
\ and perform optimisation while copying to ``HERE'' ,
\ Do not initialise, or terminate.
: (MATCH) BEGIN DUP ?NOT-EXIT WHILE MATCH-ONE REPEAT DROP ;

\ Optimise a SEQUENCE using pattern matching.
: OPTIMISE   DUP DUP FILL-BRANCHES (MATCH)   ;

\ ----------------------------------------------------------------
\ Optimise DEA by expansion plus applying optimations to the expanded code.
: OPT-EXPAND   >DFA DUP @  ^^
    EXPAND
    BEGIN !PROGRESS FOLD ^^ OPTIMISE ^^ REORDER ^^ ANNIHILATE ^^
PROGRESS @ WHILE REPEAT
SWAP ! ;

\ For DEA remember that it has been optimised
: !OPTIMISED   FMASK-HO SWAP >FFA OR!  ;

\ For DEA : it IS eligable for high level optimisation.
: H-OPTIMISABLE?  DUP HIGH-LEVEL?  SWAP  >FFA @ FMASK-HO AND 0=  AND ;

\ Try and optimise the DEA with respect HL inlining.
\ Reach trough to underlying levels first.
: OPTIMISE-O
    DUP H-OPTIMISABLE? IF
        DUP >DFA @ BEGIN NEXT-PARSE WHILE RECURSE REPEAT 2DROP
        DUP OPT-EXPAND
    THEN
    DUP ?FILL-SE?   DUP FILL-OB   !OPTIMISED ;

\D : test 1 SWAP 3 2 SWAP ;
\D 'test OPTIMISE-O
\D "EXPECT `` 1 SWAP 2 3 '' :" CR TYPE CRACK test
\D : test1 1 2 + 3 4 * OR ;
\D 'test1 OPTIMISE-O
\D "EXPECT `` F '' :" CR TYPE CRACK test1
\D : test2 1 2 SWAP ;
\D 'test2 OPTIMISE-O
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test2
\D : test3 1 2 'SWAP EXECUTE ;
\D 'test3 OPTIMISE-O
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test3
\D : A0 1 ;
\D : A1 A0 A0 + ;   : A2 A1 A1 + ;    : A3 A2 A2 + ;
\D : A4 A3 A3 + ;   : A5 A4 A4 + ;    : A6 A5 A5 + ;
\D : A7 A6 A6 + ;   : A8 A7 A7 + ;    : A9 A8 A8 + ;
\D
\D : B0 A9 A9 + ;
\D 'B0 OPTIMISE-O
\D "EXPECT `` 400 '' :" CR TYPE CRACK B0
