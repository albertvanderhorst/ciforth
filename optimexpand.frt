( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optimexpand.frt,v 5.12 2020/07/18 09:31:45 albert Exp $)

\ This code expands the code in behalf of optimisation.
\ Expanding means replacing by the words it consist of.
\ This is mostly just inlining but control words are treated specially,
\ notably (DO) LEAVE EXIT RECURSE
\ In the end we have a sequence of low level words with only inline after
\ the words identified in analyser.frt :
\ LIT SKIP BRANCH 0BRANCH (DO) (?DO)
\ Code generation will need to handle those specially.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.


WANT SWAP-DP BAG DO-BAG BOUNDS :F 2>R NULL
WANT 0<>  <=  >= ,,

( Retract from BAG in same order. Leave ITEM. Use after !BAG )
\ This is tricky, it uses the filled pointer so while retracting
\ no other bag word can be used. Afterwards do !BAG.
: BAG+@   DUP >R @ @ 0 CELL+ R> +! ;

( ------------ PORTABILITY --------------------------------- )

\ It is assumed that all addresses are positive, so < etc. is used for
\ comparison of addresses with impunity. For 16 bit Forths unsigned
\ comparison may have to be carefully introduced.

\ ----------------------    ( From optimiser.frt)
\ Store a LOW HIGH range with hl-code in the dictionary.
: HL-CODE OVER - ,, ;

\ Compile a GAP in a temporary area. Return a POINTER to the
\ unnamed code.
: COMPILE-GAP   HERE DSSWAP HL-CODE POSTPONE (;) >ALLOC ;

\ Execute the CODE from a gap such as returned by ``COMPILE-GAP''.
\ This leaves a variable number of stack items.
: EXECUTE-GAP >R ;

\ Recompile the code from BEGIN END. Leave END as the new begin.
: >HERE SWAP 2DUP -  ,, ;

\ Make the code from BEGIN END empty, by leaving END END.
: EMPTY>  SWAP DROP DUP ;

\ For POINTER : it POINTS not yet to an ``EXIT''.
: ?NOT-EXIT   @ '(;) = NOT? ;
\ For POINTER : it POINTS not yet to a ``NOOP''.
: ?TILL-NOOP   @ 'NOOP = NOT? ;

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

\ Fetch the hl code from XT , return it as a Forth STRING.
: HL-CODE@  >DFA @ DUP BEGIN NEXT-PARSE SWAP DROP 0= UNTIL OVER - ;

\ ----------------------    MISCELLANEOUS

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

\ For an ITEM in a high level word, return the next ITEM.
\ So it skips also ``ITEM'' 's inline data.
: NEXT-ITEM NEXT-PARSE 2DROP ;

\ For a SEQUENCE return its end, i.e. just after where `` (;) '' sits.
: END-OF-SEQUENCE BEGIN NEXT-PARSE WHILE DROP REPEAT DROP ;

\ ----------------------    Special classes of words --------------------

CREATE DO'S  HERE _ ,
' (DO)       ,          ' (?DO)      ,
HERE SWAP !

\ \ For DEA : it IS the start of a do loop
: IS-A-DO DO'S IN-BAG? ;
\
\ For DEA : it IS the end of a do loop.
: IS-A-LOOP '(+LOOP) = ;
\
\ \ \ For DEA : it IS special, i.e. it must be treated specially in expanding.
: IS-A-SPECIAL 'CATCH = ;


\ ----------------------    Keeping track of branching ----------------------

\ In the following we are concerned with pieces of code that end in a jmp.
\ More or less in accordance with the dragon book, we call those basic blocks
\ in short BBLOCKS.
\ In the following a GAP is a pair START END with START inclusive and END
\ exclusive.

\ For DEA : it IS a branch, i.e. it is followed by a relative control target.
: IS-A-BRANCH   DUP 'LIT <>   SWAP >FFA @ FMASK-IL AND 0<> AND ;
\ : IS-A-BRANCH   DUP 'BRANCH = SWAP '0BRANCH = OR ;

\ For a POSITION of a branch offset, find the target.
: >TARGET   @+ + ;

\ BRANCH and TARGET is unfree with respect to GAP, i.e. return :
\ 0 if this jump is either totally outside or totally inside the GAP.
\ 1 jumping to outside
\ 2 jumping to inside
: UNFREE-WRT?
    >R 2DUP = IF 2DROP DROP RDROP 0 EXIT THEN R> \ You may jump to the start of a gap always!
    2>R
    SWAP 2R@ WITHIN        \ BRANCH inside
    SWAP 2R> WITHIN         \ Target Inside
    2DUP = IF 2DROP 0 ELSE < 2 + THEN
;

\ Contains the worst case for all gaps.
VARIABLE WORST-CASE

\ For a GAP : it IS forbidden, i.e. there is some branch crossing the gap boundary.
: FORBIDDEN-GAP? 0 WORST-CASE !
bblocks DO-BAG >BBLOCK
    ^branch-offset DUP >TARGET 2OVER UNFREE-WRT? WORST-CASE OR!
LOOP-BAG 2DROP WORST-CASE @ ;

\ For an ADDRESS : it is the TARGET of a branch.
: IS-A-BRANCH-TARGET
bblocks DO-BAG >BBLOCK
    DUP ^branch-offset >TARGET = IF DROP -1 LEAVE THEN
LOOP-BAG   -1 = ;

\ ----------------------    Closing a gap -------------------

\ The offset over which the gap is shifted shut, generally negative.
VARIABLE GAP-OFFSET

\ For GAP , adjust the branchoffset of the current bblock if it jumps from
\ left over the gap by  `GAP-OFFSET (that must have been filled in.)
: ADJUST-BRANCH-FROM-LEFT
branch-target <=   SWAP ^branch-offset >   AND IF
    GAP-OFFSET @ ^branch-offset +!
THEN ;

\ For GAP , adjust the branchoffset of the current bblock by `GAP-OFFSET
\ (that must have been filled in) if it jumps from left over the gap.
: ADJUST-BRANCH-FROM-RIGHT
^branch-offset <  SWAP branch-target >=   AND IF
    GAP-OFFSET @ NEGATE ^branch-offset +!
THEN ;

\ The set of branches that is marked for elimination from the set `bblocks.
MAX-BAG BAG MARKED-bblocks
: !MARKED-bblocks  MARKED-bblocks !BAG ;

\ For GAP if the branch of the current bblock is taken from inside
\ the gap, mark it for elimination from the table.
\ We can't remove them from the set right away because things get entangled.
: ELIMINATE-BRANCH-IN-GAP   ^branch-offset DSSWAP WITHIN IF this MARKED-bblocks BAG+! THEN ;

\ Delete from `bblocks what is marked for elimination.
\ Must go back because otherwise we would disturb the later addresses.
: DELETE-MARKED-bblocks MARKED-bblocks @+
   BEGIN 2DUP < WHILE   CELL-   DUP @ bblocks SET-   REPEAT 2DROP ;

\ For GAP adjust all branches sitting in `bblocks and the set itself.
: ADJUST-BRANCHES !MARKED-bblocks bblocks DO-BAG >BBLOCK
    2DUP ADJUST-BRANCH-FROM-LEFT
    2DUP ADJUST-BRANCH-FROM-RIGHT
    2DUP ELIMINATE-BRANCH-IN-GAP
LOOP-BAG 2DROP ;

\ For END of gap, shift the remainder to close the gap.
: SHIFT-GAP-SHUT
    DUP END-OF-SEQUENCE OVER - >R   DUP GAP-OFFSET @ +  R>   MOVE ;

\ Correct the branch-addresses higher than the START of a gap,
\ to reflect the position they have after closing the gap.
: MOVE-BRANCHES   bblocks DO-BAG >BBLOCK
    DUP ^branch-offset < IF GAP-OFFSET @   adjust-bend THEN
LOOP-BAG DROP ;

\ Correct the GAP, i.e. correct its end with ``GAP-OFFSET''.
\ The gaps content becomes invalid.
\ Adjust all branches over the gap and information about branches.
\ It is assumed that the gap is part of a sequence that ends at ``HERE'',
\ such that the dictionary may be adjusted.
\ Return the new GAP.
: CORRECT-GAP
    2DUP ADJUST-BRANCHES   DELETE-MARKED-bblocks OVER MOVE-BRANCHES
    DUP SHIFT-GAP-SHUT
    \ Correct the end. The start is the same.
    GAP-OFFSET @ DUP ALLOT +
;

\ ----------------------    Special expansions ---------------

\ Increment ADDRESS (of a ``LEAVE'') until pointing after the corresponding
\ loop end. Return IT.
: FIND-LOOP ( Go on until past one loop ) 1 >R
    BEGIN NEXT-PARSE 0= 13 ?ERROR
        DUP IS-A-LOOP IF R> 1- >R THEN
        IS-A-DO IF R> 1+ >R THEN
    R@ 0= UNTIL RDROP 2 CELLS + ;

\ ----------------------    Expansion  ----------------------

\ Add bblock ending at  addr  to `bblocks .
\ Make sure it is current in order to fill in other properties.
    VARIABLE end-expand  \ Where the previously created bblock ended.
: add-bblock-expand   new-bblock    end-expand @ bstart!   DUP end-expand ! 
    bend! ;   

\ the new expansion is supposed to take care of branches.
\ expanding :
\  expand hl code taking care of branches
\  LEAVE and EXIT , expand to BRANCH _ ,  Remember special targets
\   do loop and '(;).  Fix up those separate from normal branches

\ The bag of shifts ; Each pairs is an ADDRESS SHIFT.
\ All branches-parts (start or targets) higher than address are to be
\ offset by the corresponding shift. These offsets are cumulative.
MAX-BAG BAG SHIFTS     : !SHIFTS   SHIFTS !BAG ;

\ If DEA is a BRANCH remember it in `bblocks
: REMEMBER-BRANCH   IS-A-BRANCH IF HERE CELL+ CELL+ add-bblock-expand   THEN ;

\ Copy a GAP containing a single statement (xt plus possible inline stuff)
\ to ``HERE'' take into account specialties associated with its DEA.
\ Leave the END of the gap.
\ : COPY-ONE IF >HERE THEN ;     \ To strive for
: COPY-ONE DUP >R  REMEMBER-BRANCH
    R>  IF >HERE THEN ;


\ For a forward branch at ADDRESS do all the corrections found in ``SHIFTS''.
\ Note that this accumulates changes, and the decision to apply a correction
\ depends on order and previous corrections applied.
: CORRECT-ONE-BRANCH-FORWARD
SHIFTS
@+ SWAP ?DO
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
bblocks DO-BAG >BBLOCK
    ^branch-offset DUP @ 0< IF CORRECT-ONE-BRANCH-BACKWARD ELSE CORRECT-ONE-BRANCH-FORWARD THEN
LOOP-BAG ;

\ Expand the DEA to `HERE maintaining `SHIFTS
\ Note there are branches within the hl code, but they are never shifted.
: COPY-CONTENT   >DFA @ BEGIN DUP NEXT-PARSE WHILE  COPY-ONE REPEAT 2DROP DROP ;
\ However if they are no generated they will fail. Like so.
\ : COPY-CONTENT   DUP END-OF-SEQUENCE OVER - ,, ;

\ These replacements must contain only low-level words!
: NEW-DO   0 >R SWAP >R >R ;             \ _ instead of 0 doesn't work FIXME!
: NEW-?DO   2DUP - 0= 0BRANCH ;

\ Code has been filled starting at  oldhere  to replace code of  n  cells.
: fill-shifts   OVER SHIFTS BAG+!   CELLS + HERE SWAP -   SHIFTS BAG+! ;
\ All have GAP and DEA  , leave START of new gap.
: HANDLE-HIGH   HERE >R  COPY-CONTENT NIP
     R> 1 fill-shifts ;
: HANDLE-BRANCH   DROP   >HERE    HERE add-bblock-expand ;
: HANDLE-EXIT     DROP NIP
        HERE SHIFTS BAG+!   1 CELLS SHIFTS BAG+!
        'BRANCH ,   DUP END-OF-SEQUENCE OVER CELL+ - ,
        HERE add-bblock-expand ;
: HANDLE-LEAVE   DROP NIP
        HERE SHIFTS BAG+!   1 CELLS SHIFTS BAG+!
        'BRANCH ,   DUP FIND-LOOP OVER - ,   \ Branch to UNLOOP
        HERE add-bblock-expand ;
: HANDLE-DO       HERE >R   DROP NIP  'NEW-DO HL-CODE@ 1 CELLS - ,,
     R> 2 fill-shifts ;
: HANDLE-?DO       HERE >R    DROP NIP
     'NEW-?DO HL-CODE@ 1 CELLS - ,,     DUP CELL- @ ( Offset) ,
     R> 2 fill-shifts   HERE add-bblock-expand
     HERE >R   'NEW-DO HL-CODE@ 1 CELLS - ,,  R> 0 fill-shifts ;

\ For GAP and DEA : expand GAP to ``HERE'' filling in ``SHIFTS''.
\ Leave the END of the gap.
: EXPAND-ONE
    DUP 'BRANCH = IF HANDLE-BRANCH  EXIT THEN
    DUP '0BRANCH = IF HANDLE-BRANCH  EXIT THEN
    DUP '(DO) = IF HANDLE-DO EXIT THEN
    DUP '(?DO) = IF HANDLE-BRANCH  EXIT THEN
    DUP 'EXIT = IF HANDLE-EXIT EXIT THEN
    DUP 'LEAVE = IF HANDLE-LEAVE EXIT THEN
    \ DUP '(+LOOP) = IF HANDLE-BRANCH  EXIT THEN
    DUP IS-A-SPECIAL IF REMEMBER-BRANCH >HERE EXIT THEN
    \ default case.
    DUP HIGH-LEVEL? IF HANDLE-HIGH ELSE DROP >HERE THEN
;

\ Expand the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
\ Do not initialise, or terminate.
: (EXPAND)   HERE end-expand ! !bblocks
    BEGIN DUP NEXT-PARSE WHILE EXPAND-ONE REPEAT
    DROP DROP DROP ;

\ Expand each constituent of SEQUENCE to ``HERE'' leaving `EXIT and `LEAVE.
\ Leave a POINTER to equivalent linearised code.
: EXPAND-PLAIN HERE SWAP    !SHIFTS   (EXPAND) POSTPONE (;) CORRECT-BRANCHES ;

\ ------------------- end of expansions ------------------ l
