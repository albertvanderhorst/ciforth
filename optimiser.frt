( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

WANT $-PREFIX
WANT SWAP-DP BAG DO-BAG BOUNDS :F

( Retract from BAG in same order. Leave ITEM. Use after !BAG )
\ This is tricky, it uses the filled pointer so while retracting
\ No other bag word can be used. Afterwards do !BAG.
: BAG+@   DUP >R @ @ 0 CELL+ R> +! ;

( ------------ PORTABILITY --------------------------------- )

\ It is assumed that all addresses are positive, so < etc. is used for
\ comparison of addresses with impunity. For 16 bit Forths unsigned
\ comparison may have to be carefully introduced.


( ------------ COMFIGURATION --------------------------------- )

500 CONSTANT MAX-BAG       \ Default bag size (sets too)

\ Throw codes used :
\ 4002 annihilation chain breaks down

( ------------ DEBUGGING ------------------------------------- )
: \D POSTPONE \ ; IMMEDIATE    : ^^ ;
\ : \D ; IMMEDIATE : ^^ &: EMIT &< EMIT .S DUP CRACK-COLON &> EMIT &; EMIT CR ;

( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------)
\ For a BAG print it backwards. Primarily intended as how to loop backwards example.
: BAG-PRINT-BACKWARDS
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
   @+ >R   R@ >FFA @ FMASK-IL AND IF
       R@ 'SKIP = IF @+ + ALIGNED ELSE CELL+ THEN
   THEN
   R@
\   R@ ID.          \ For desperado debugging.
   R> '(;) <> ;

\ ----------------------    MISCELLANEOUS

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

\ For an ITEM in a high level word, return the next ITEM.
\ So it skips also ``ITEM'' 's inline data.
: NEXT-ITEM NEXT-PARSE 2DROP ;

\ For a SEQUENCE return its end, i.e. just after where `` (;) '' sits.
: END-OF-SEQUENCE BEGIN NEXT-PARSE WHILE DROP REPEAT DROP ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ Like +! but ands.
: AND!  DUP @ ROT AND SWAP ! ;

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

\ ----------------------    Special classes of words --------------------

CREATE DROPS  HERE _ ,
' DROP       ,          ' 2DROP      ,
HERE SWAP !

CREATE DO'S  HERE _ ,
' (DO)       ,          ' (?DO)      ,
HERE SWAP !

CREATE LOOPS  HERE _ ,
' (LOOP)       ,          ' (+LOOP)      ,
HERE SWAP !

CREATE SPECIALS  HERE _ ,
' LEAVE       ,          ' CATCH      ,
HERE SWAP !

\ For DEA : it IS a trivial annihilator.
: IS-A-DROP DROPS IN-BAG? ;

\ For DEA : it IS the start of a do loop
: IS-A-DO DO'S IN-BAG? ;

\ For DEA : it IS the end of a do loop.
: IS-A-LOOP LOOPS IN-BAG? ;

\ For DEA : it IS special, i.e. it must be treated specially in expanding.
: IS-A-SPECIAL SPECIALS IN-BAG? ;


\ ----------------------    Keeping track of branching ----------------------

\ In the following a GAP is a pair START END with START inclusive and END
\ exclusive.

\ For DEA : it IS a branch, i.e. it is followed by a relative control target.
: IS-A-BRANCH   DUP 'LIT <>   SWAP >FFA @ FMASK-IL AND 0= 0= AND ;

\ The bag of addresses where a branch offset is stored.
MAX-BAG BAG BRANCHES
: !BRANCHES   BRANCHES !BAG ;

\ For a POSITION of a branch offset, find the target.
: >TARGET   @+ + ;

\ WORKING BUT NO LONGER USED. INVALUABLE DURING TESTIN.
\ For START if there is some branch at ADDRESS add it to ``BRANCHES''
: FILL-ONE-BRANCH DUP @ IS-A-BRANCH IF
    CELL+   BRANCHES BAG+!
    _ THEN DROP ;

\ For a SEQUENCE fill the ``BRANCHES'' set.
: FILL-BRANCHES !BRANCHES BEGIN DUP FILL-ONE-BRANCH NEXT-PARSE WHILE DROP REPEAT 2DROP ;

\ BRANCH and TARGET is unfree with respect to GAP, i.e. return :
\ 0 if this jump is either totally outside or totally inside the GAP.
\ 1 jumping to outside
\ 2 jumping to inside
: UNFREE-WRT?
    >R 2DUP = IF 2DROP DROP RDROP 0 EXIT THEN R> \ You may jump to the start of a gap always!
    D>R
    SWAP DR@ WITHIN        \ BRANCH inside
    SWAP DR> WITHIN         \ Target Inside
    2DUP = IF 2DROP 0 ELSE < 2 + THEN
;

\ Contains the worst case for all gaps.
VARIABLE WORST-CASE

\ For a GAP : it IS forbidden, i.e. there is some branch crossing the gap boundary.
: FORBIDDEN-GAP? 0 WORST-CASE !
BRANCHES DO-BAG
    I @ DUP >TARGET 2OVER UNFREE-WRT? WORST-CASE OR!
LOOP-BAG 2DROP WORST-CASE @ ;

\ For an ADDRESS : it is the TARGET of a branch.
: IS-A-BRANCH-TARGET
BRANCHES DO-BAG
    DUP I @ >TARGET = IF DROP -1 LEAVE THEN
LOOP-BAG   -1 = ;

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
MAX-BAG BAG MARKED-BRANCHES
: !MARKED-BRANCHES  MARKED-BRANCHES !BAG ;

\ For GAP and ADDRESS of entry in branches, if the branch is taken from inside
\ the gap, mark it for elimination from the table.
\ We can't remove them from the set right away because things get entangled.
: ELIMINATE-BRANCH-IN-GAP   >R   R@ @ DSSWAP WITHIN IF
    R@ MARKED-BRANCHES BAG+!
THEN RDROP ;

\ Delete from ``BRANCHES'' what is marked for elimination.
\ Must go back because otherwise we would disturb the later addresses.
: DELETE-MARKED-BRANCHES MARKED-BRANCHES @+
   BEGIN 2DUP < WHILE   1 CELLS -   DUP @ BRANCHES SET-   REPEAT 2DROP ;

\ For GAP adjust all branches sitting in ``BRANCHES'' and the set itself.
: ADJUST-BRANCHES !MARKED-BRANCHES  BRANCHES DO-BAG
    2DUP I @ ADJUST-BRANCH-FROM-LEFT
    2DUP I @ ADJUST-BRANCH-FROM-RIGHT
    2DUP I ELIMINATE-BRANCH-IN-GAP
LOOP-BAG 2DROP ;

\ For END of gap, shift the remainder to close the gap.
: SHIFT-GAP-SHUT
    DUP END-OF-SEQUENCE OVER - >R   DUP GAP-OFFSET @ +  R>   MOVE ;

\ Correct the branch-addresses higher than the START of a gap,
\ to reflect the position they have after closing the gap.
: MOVE-BRANCHES   BRANCHES DO-BAG
    DUP I @ < IF GAP-OFFSET @   I +! THEN
LOOP-BAG DROP ;

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


\ ----------------------    Special expansions ---------------

\ Sample code to replace a ``LEAVE'' exection token.
\ Not to be executed directly. Branch is still to be filled in.
: LEAVE-CODE UNLOOP BRANCH [ _ , ] ;

\ For DEA: it IS a ``LEAVE''.
: IS-A-LEAVE   'LEAVE = ;

\ \ Expand  the first statement (a ``LEAVE'' ) of SEQUENCE by a branch (plus other code as appropriate.)
\ : COPY-LEAVE   POSTPONE UNLOOP   POSTPONE BRANCH   HERE BRANCHES BAG+!
\   HERE LEAVES BAG+!    _ ,   ;

\ Leave code length. Don't count the (;). It must not be copied.
'LEAVE-CODE >DFA @   DUP END-OF-SEQUENCE SWAP - 1 CELLS -
CONSTANT LEAVE-LENGTH

\ Prepare the gap offset for expansion of a ``LEAVE''.
: SET-LEAVE-OFFSET  LEAVE-LENGTH 1 CELLS - GAP-OFFSET ! ;

\ Expand a leave at SEQUENCE. Return the expanded GAP .
: ENLARGE-LEAVE-GAP DUP CELL+ CORRECT-GAP ;

\ Fill a gap at ADDRESS with the expansion of a ``LEAVE''.
: FILL-LEAVE-GAP   'LEAVE-CODE >DFA @   SWAP   LEAVE-LENGTH MOVE ;

\ Transform a ``LEAVE'' gap at start of SEQUENCE.
\ Return the incremented SEQUENCE.
: LEAVE-GAP SET-LEAVE-OFFSET ENLARGE-LEAVE-GAP >R FILL-LEAVE-GAP R> ;

\ Increment ADDRESS (of a ``LEAVE'') until pointing after the corresponding
\ loop end. Return IT.
: FIND-LOOP ( Go on until past one loop ) 1 >R
    BEGIN NEXT-PARSE 0= 13 ?ERROR
        DUP IS-A-LOOP IF R> 1- >R THEN
        IS-A-DO IF R> 1+ >R THEN
    R@ 0= UNTIL RDROP ;

\ Fill in the offset of a leave-replacing branch that ends at SEQUENCE.
\ (The offset is one cell before this end.)
: FILL-LEAVE-BRANCH DUP FIND-LOOP OVER - SWAP 1 CELLS - ! ;

\ Expand the ``LEAVE'' at the start of SEQUENCE . Execution remains equivalent.
\ Return SEQUENCE incremented to after the code expanded.
: REPLACE-LEAVE-BY-BRANCH LEAVE-GAP DUP FILL-LEAVE-BRANCH
    DUP 1 CELLS - BRANCHES BAG+! ;

\ Try to apply any special expansion to the start of a SEQUENCE .
\ Always leave the new SEQUENCE be it just after the expansion or bumped
\ by one item.
: EXPAND-SPECIAL-ONE
    DUP @ IS-A-LEAVE IF REPLACE-LEAVE-BY-BRANCH ELSE
\    DUP @ IS-A-CATCH IF REPLACE-CATCH-BY-BRANCH ELSE
    NEXT-ITEM
    THEN
;

\ Expand some special xt's from SEQUENCE. In partical ``LEAVE''.
\ Return recompiled SEQUENCE (which is in fact the same address.)
\ It is required that the ``BRANCHES'' set has been filled.
: EXPAND-SPECIAL DUP BEGIN DUP ?NOT-EXIT WHILE EXPAND-SPECIAL-ONE REPEAT DROP ;

\ ----------------------    Expansion  ----------------------
\ the new expansion is supposed to take care of branches.

\ The bag of shifts ; Each pairs is an ADDRESS SHIFT.
\ All branches-parts (start or targets) higher than address are to be
\ offset by the corresponding shift. These offsets are cumulative.
MAX-BAG BAG SHIFTS     : !SHIFTS   SHIFTS !BAG ;

\ The set of exits : places where a branch has to be filled in
\ that replace an exit from a word that is expanded in line.
MAX-BAG BAG EXITS     : !EXITS   EXITS !BAG ;

\ The set of leaves : places where a branch has to be filled in
\ that replace a leave from a loop that is expanded in line.
MAX-BAG BAG LEAVES     : !LEAVES   LEAVES !BAG ;

\ If DEA is a BRANCH remember it in ``BRANCHES''
: REMEMBER-BRANCH   IS-A-BRANCH IF HERE CELL+ BRANCHES BAG+!  THEN ;

\ Copy an ``EXIT'' statement to the output sequence, replacing it by a branch.
: COPY-EXIT   HERE EXITS BAG+!
POSTPONE BRANCH   HERE BRANCHES BAG+!   _ ,   ;

\ For all the ``EXITS'' remembered, fill in the ``SHIFTS'' caused by expansion
\ of the ``EXIT''. To be called when ``HERE'' is where the ``EXIT'' must
\ branch to.
: HANDLE-EXITS-BRANCHES
EXITS DO-BAG
    I @ SHIFTS BAG+!    0 CELL+ SHIFTS BAG+!
LOOP-BAG ;

\ For all the ``EXITS'' expanded, fill in its branch offset.
\ The correction that will later be applied caused by shift,
\ must be precompenstated.
: HANDLE-EXITS-SHIFTS EXITS @+ -  \ Correction : #EXITS cells.
EXITS DO-BAG
HERE OVER +   I @ CELL+ -    I @ CELL+ !   CELL+
LOOP-BAG DROP ;

\ Keep data structures up to date, w.r.t. ``EXITS''
\ To be called when ``HERE'' is where the ``EXIT'' must branch to.
: HANDLE-EXITS HANDLE-EXITS-BRANCHES HANDLE-EXITS-SHIFTS ;

\ Copy a GAP containing a single statement (xt plus possible inline stuff)
\ to ``HERE'' take into account specialties associated with its DEA.
\ Leave the END of the gap.
: COPY-ONE >R  R@ REMEMBER-BRANCH
R@  'EXIT = IF SWAP DROP COPY-EXIT ELSE
>HERE THEN RDROP ;

\ For a forward branch at ADDRESS do all the corrections found in ``SHIFTS''.
\ Note that this accumulates changes, and the decision to apply a correction
\ depends on order and previous corrections applied.
: CORRECT-ONE-BRANCH-FORWARD
SHIFTS
@+ SWAP ?DO
    I @    OVER    DUP >TARGET 1+   WITHIN IF I CELL+ @ OVER +! THEN
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
BRANCHES DO-BAG
    I @ DUP @ 0< IF CORRECT-ONE-BRANCH-BACKWARD ELSE CORRECT-ONE-BRANCH-FORWARD THEN
LOOP-BAG ;

\ Copy the SEQUENCE of high level code to ``HERE'' , expanding the high level words.
: COPY-CONTENT   BEGIN DUP NEXT-PARSE WHILE  COPY-ONE REPEAT 2DROP DROP ;

\ Copy the SEQUENCE of high level code to ``HERE'' .
\ Do not initialise, or terminate.
: (EXPAND-ONE)
!EXITS
     HERE SWAP COPY-CONTENT
    DUP SHIFTS BAG+!
    HERE SWAP CELL+ -   SHIFTS BAG+!
    HANDLE-EXITS
;

\ For GAP and DEA : expand GAP to ``HERE'' filling in ``SHIFTS''.
\ Leave the END of the gap.
: EXPAND-ONE
    DUP HIGH-LEVEL? OVER IS-A-SPECIAL 0= AND IF >DFA @ (EXPAND-ONE) SWAP DROP
    ELSE REMEMBER-BRANCH >HERE THEN ;

\ Expand the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
\ Do not initialise, or terminate.
: (EXPAND)   BEGIN DUP NEXT-PARSE WHILE EXPAND-ONE REPEAT 2DROP DROP ;

\ Expand each constituent of SEQUENCE to ``HERE'' .
\ Leave a POINTER to equivalent linearised code.
: EXPAND HERE SWAP    !SHIFTS   (EXPAND) POSTPONE (;) CORRECT-BRANCHES
EXPAND-SPECIAL ;

\ ----------------------    Annihilaton ----------------------

\ For DEA return "it CAN be part of annihilatable code",
\ as far as its stack & side effects are concerned.
: ANNILABLE? DUP NS!?   SWAP SE@ NO-GOOD 0= AND ;

\ We are at a stable point, i.e. we consumed all the extra stuff,
\ that is placed in the annihilation chain. Maybe even more.
: ANNIL-STABLE?   VD @ MIN-DEPTH @ =  VD @ 1 <  AND ;

\ For ADDRESS and XT : it IS a branch going backwards.
: BACK-BRANCH?  IS-A-BRANCH IF 1 CELLS - @ 0 < ELSE DROP 0 THEN ;

\ Replace an ADDRESS after a branch code by: the branch TARGET.
: PASTBRANCH>TARGET   1 CELLS - >TARGET ;

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
: SWAP-ANNIL  MAX-ANNIL @ MAX-ANNIL-DEPTH @ D>R POST-ANNIL DR> VD ! ;

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
: FINISHED? MUST-HIGHER? 0= ANNIL-STABLE? AND ;

\ Investigate the start of SEQUENCE. Return the ADDRESS
\ to which it can be annihilated, else throw.
: (ANNIHILATE-SEQ)
    NEXT-PARSE 0= KAPUT??
    DUP ANNILABLE? 0= KAPUT??
    2DUP BACK-BRANCH? KAPUT??
    DUP 'BRANCH = IF SWAP PASTBRANCH>TARGET SWAP THEN
    DUP SE@ COMBINE-VD
    '0BRANCH = IF
        VD @
        OVER PASTBRANCH>TARGET FINISHED? 0= IF RECURSE THEN DROP
        VD !
    THEN
    FINISHED? 0= IF RECURSE THEN
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
    2DUP FORBIDDEN-GAP? 1 > IF DROP NEXT-ITEM ELSE
    ANNIHILATE-GAP THEN THEN THEN ;

\ Annihilate as much as possible from SEQUENCE.
\ Return recompiled SEQUENCE (which is in fact the same address.)
: ANNIHILATE DUP   ( DUP FILL-BRANCHES)
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
\ If a real noop is needed use ``NOP1''
'NOOP ALIAS NOP1        \ The same, except for the xt.

    'ALIGN-NOOPS ALIAS |                         \ Convenience alias.
    : ]L ] POSTPONE LITERAL ; IMMEDIATE \ Convenience alias.

: (MATCH-TABLE)                         |
\ `` MATCH-TABLE'' points here :
'P  EXECUTE             | P                        | \ Execute optimisation
'P  + 'P  +             | 'P  'P  + +              | \ Associativity optimisation
'P  + 'P  -             | 'P  'P  - +              |
'P  - 'P  +             | 'P  'P  - -              |
'P  - 'P  -             | 'P  'P  + -              |
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
    R> + -1 PROGRESS !
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
\ It is required that the ``BRANCHES'' set has been filled.
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
\ It is required that the ``BRANCHES'' set has been filled.
: DEAD-CODE DUP BEGIN DUP ?NOT-EXIT WHILE BRANCH-SPECIAL-ONE REPEAT DROP ;

\ ----------------------------------------------------------------
\ Optimise DEA by expansion plus applying optimations to the expanded code.
: OPT-EXPAND   >DFA DUP @  ^^
    EXPAND
    BEGIN !PROGRESS FOLD ^^ MATCH ^^ REORDER ^^ ANNIHILATE ^^ DEAD-CODE ^^
PROGRESS @ WHILE REPEAT
SWAP ! ;

\ For DEA remember that it has been optimised
: !OPTIMISED   FMASK-HO SWAP >FFA OR!  ;

\ For DEA : it IS eligable for high level optimisation.
: H-OPTIMISABLE?  DUP HIGH-LEVEL?  SWAP  >FFA @ FMASK-HO AND 0=  AND ;

\ Try and optimise the DEA with respect HL inlining.
\ Reach trough to underlying levels first.
:F OPTIMISE ;
:R OPTIMISE
    DUP H-OPTIMISABLE? IF
        DUP >DFA @ BEGIN NEXT-PARSE WHILE OPTIMISE REPEAT 2DROP
        DUP OPT-EXPAND
    THEN
    DUP ?FILL-SE?   DUP FILL-OB   !OPTIMISED ;
