( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

REQUIRE $
  : \D POSTPONE \ ; IMMEDIATE
\  : \D ;            IMMEDIATE

\ ----------------------    ( From optimiser.frt)
\ Store a STRING with hl-code in the dictionary.
: HL-CODE, HERE OVER ALLOT SWAP CMOVE ;

\ For a parse ADDRESS return an incremented parse ADDRESS, its
\ CONTENT and a go on FLAG.
: NEXT-PARSE
   @+ >R   R@ CFA> >FFA @ FMASK-IL AND IF
       R@ CFA> 'SKIP = IF @+ + ALIGNED ELSE CELL+ THEN
   THEN
   R@
\D   R@ CFA> ID.
   R> '(;) <> ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;
\ ----------------------    ----------------------     ----------------------


\ For DEA : it HAS no side effects, input or output.
: NS?   >FFA @ FMASK-NS AND FMASK-NS = ;

\ How MANY stack cells on top contain a compile time constant?
VARIABLE CSC

\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE OPT-START

: !OPT-START   HERE OPT-START !   0 CSC ! ;

\ For STACKEFFECTNIBBLE : it IS no good, because it is unknown or variable.
: NO-GOOD DUP $F = SWAP 0= OR ;

\ For STACKEFFECTBYTE : it IS good, neither unknown or variable.
: SE-GOOD DUP $F AND NO-GOOD SWAP 4 RSHIFT NO-GOOD OR  0= ;

\ For STACKEFFECTBYTE : we CAN still optimise, because we know we have
\ sufficiantly constant stack cells.
: ENOUGH-POPS   DUP SE-GOOD   SWAP 4 RSHIFT 1- CSC @ > 0=  AND ;

\ Combine a STACKEFFECTBYTE into ``CSC''.
\ ``-'' works here because both nibbles have offset 1!
: COMBINE-CSC    SE:1>2 SWAP -   CSC +! ;

\ Execute at compile time the optimisable code we have collected from
\ ``OPT-START''
\ The result is supposedly ``CSC'' stack items.
: EXECUTE-DURING-COMPILE   POSTPONE (;)    OPT-START @ >R ;

\ Throw away the executable code that is to be replaced with optimised code.
: THROW-AWAY   OPT-START @ DP ! ;

\ Compile ``CSC'' constants instead of the code optimised away.
\ They sit on the stack now. We need to store backwards to reverse them.
: COMPILE-CONSTANTS \ CSC @ 0= 13 ?ERROR
    HERE >R   CSC @ CELLS 2 * ALLOT   HERE >R
    BEGIN R> R> 2DUP <> WHILE >R 2 CELLS - >R
        'LIT R@ !  R@ CELL+ ! REPEAT
    2DROP
    '(;) HERE !     \ To prevent too many crashes while testing.
;
\ WHAT ABOU
\ HERE BEGIN DUP R@ <> WHILE 'LIT SWAP -! -! REPEAT DROP

\ Recompile the code from BEGIN END. Leave END as the new begin.
: >HERE SWAP 2DUP -  HL-CODE, ;

\ Make the code from BEGIN END empty, by leaving END END.
: EMPTY>  SWAP DROP DUP ;

\ Optimisation is over. Run the optimisable code and compile constants
\ instead of them. "Cash the optimisation check."
: CASH
    OPT-START @ HERE <> IF
        !CSP EXECUTE-DURING-COMPILE THROW-AWAY COMPILE-CONSTANTS ?CSP
    THEN
;

\ Combining the effect of DEA into the current state, return
\ "the folding optimisation still HOLDS".
: CAN-FOLD?   NS? OVER SE@ ENOUGH-POPS AND ;

\ For BEGIN END DEA : if DEA allows it, add to optimisation,
\ else cash it and restart it. ``BEGIN'' ``END'' is the code copied.
\ (Mostly ``DEA'' plus inline belonging to it.)
\ Return a new BEGIN for the code to be moved, mostly the old ``END''.
:  ?OPT-FOLD?
    DUP CAN-FOLD?
    IF
        SE@ COMBINE-CSC               ( DEA -- )
        >HERE                        ( BEGIN END -- BEGIN' )
    ELSE
        DROP CASH  ( DEA -- )
        >HERE                        ( BEGIN END -- BEGIN' )
        !OPT-START
    THEN ;

\ Copy the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
\ Do not initialise, or terminate.
: (FOLD)
    BEGIN DUP
        NEXT-PARSE
    WHILE  ?OPT-FOLD?
    REPEAT DROP DROP DROP
;

\ Copy the SEQUENCE DEA of high level code to ``HERE'' ,  possibly folding it.
\ Leave a POINTER to the equivalent optimised code.
: FOLD   HERE SWAP    (FOLD)   CASH   POSTPONE (;)  ;

\ Optimise DEA regards folding.
: OPT-FOLD  !OPT-START   >DFA DUP @ FOLD   SWAP ! ;

\ For BEGIN END : copy the DEA's high level or low level code to here.
\ This is assuming only low level code is followed by in line stuff.
\ In high level coding doing this you must block optimisation.
\ Leave END.
: FOLD-HIGH/LOW    DUP HIGH-LEVEL? IF >DFA @ (FOLD) SWAP DROP
    ELSE ?OPT-FOLD? THEN ;

\ Expand the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
\ Do not initialise, or terminate.
: (EXPAND) BEGIN DUP NEXT-PARSE WHILE FOLD-HIGH/LOW REPEAT 2DROP DROP ;

\ Expand each constituent of SEQUENCE to ``HERE'' ,  possibly folding it.
\ Leave a POINTER to equivalent optimised code.
: EXPAND   HERE SWAP    (EXPAND)   CASH   POSTPONE (;)  ;


\ ----------------------------------------------------------------

\ Belongs in analyser.frt
HEX
800 CONSTANT FMASK-SP    \ Special optimisation possible. Pattern.
DECIMAL
\ ----------------------------------------------------------------
\ Add as many noops to the dictionary to have it aligned at an 8-cell
\ boundary.
: ALIGN-NOOPS   ALIGN  BEGIN HERE 8 CELLS MOD WHILE POSTPONE NOOP REPEAT ;
IMMEDIATE

\ Place holder
CREATE P

\ \ Create the code sequence for a literal with a place holder.
\ : 'P       ['] LIT COMPILE, ['] P COMPILE, ; IMMEDIATE
\ This looks more portable, but is it?

\ Create a table of optimisations :
\    ``8 CELLS'' code pattern    | ``8 CELLS'' optimised replacement |
\ The first pattern to start with NOOP is the end.
\ Not intended to be executed, but patterns are to be matched
\ from this definitions, and pieces to be copied out.
\ ``P'' is used as a placeholder for a data item to be copied to the
\ optimised side. Several P's must be replaced in order, even if escaped
\ by a ``LIT''
    'ALIGN-NOOPS ALIAS |                         \ Convenience alias.
    : ]L ] POSTPONE LITERAL ; IMMEDIATE \ Convenience alias.
: (MATCH-TABLE)                         |
\ `` MATCH-TABLE'' points here :
'P  EXECUTE     | P                     |       \ Execute optimisation
'P  + 'P  +     | 'P  'P  + +           |       \ Associativity optimisation
'P  + 'P  -     | 'P  'P  - +           |
'P  - 'P  +     | 'P  'P  SWAP - +      |
'P  - 'P  -     | 'P  'P  + -           |
'P  * 'P  *     | 'P  'P  * *           |
'P  OR 'P  OR   | 'P  'P  OR OR         |
'P  AND 'P  AND | 'P  'P  AND AND       |
'P  XOR 'P  XOR | 'P  'P  XOR XOR       |
[ 0 ]L +        |                       |       \ Shortcut evalutions
[ 0 ]L *        | DROP 0                |
[ 0 ]L OR       |                       |
[ 0 ]L AND      | DROP 0                |
[ 0 ]L XOR      |                       |
[ 1 ]L *        |                       |
[ 1 ]L /        |                       |
[ -1 ]L *       | NEGATE                |
[ -1 ]L /       | NEGATE                |
[ -1 ]L OR      | DROP -1               |
[ -1 ]L AND     |                       |
[ -1 ]L XOR     | INVERT                |
'P  LSHIFT 'P  LSHIFT | 'P  'P  + LSHIFT       |       \ Distributivity optimisation
'P  RSHIFT 'P  RSHIFT | 'P  'P  + RSHIFT       |
;

FMASK-SP 'EXECUTE >FFA OR!
FMASK-SP '+ >FFA OR!
FMASK-SP '- >FFA OR!
\ Optimalisation of this table is thoroughly forbidden!
FMASK-HOB '(MATCH-TABLE) >FFA OR!
\ Get rid of those auxiliary words.
'| HIDDEN       ']L HIDDEN

\ Here is your table
' (MATCH-TABLE) >DFA @ 1- 8 CELLS 1- OR 1+ CONSTANT MATCH-TABLE

\ \ Portability note.
\ \ You could have the table like this :
\ 'LIT , P , '+ ,    'LIT , P , '+ , ALIGN-NOOPS
\     'LIT , P ,    'LIT , P , '+ , '+ , ALIGN-NOOPS
\ \ and do POSTPONE's of items. This is still not portable however.


\ ----------------------------------------------------------------
( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------)
( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP DO I . 0 CELL+ +LOOP ;   ( Print non-empty SET )
: SET+@   DUP >R @ @ 0 CELL+ R> +! ;   ( retract from SET. Leave ITEM )

8 SET PEES
: !PEES PEES !SET ;
\ ----------------------------------------------------------------

\ From ARRAY fetch element Index. Return IT.
: [] CELLS + @ ;

\ From ARRAY fetch element ``I''. Return IT.
\ To be used within a loop
: [I] POSTPONE I POSTPONE [] ; IMMEDIATE

\ For a SEQUENCE and an ENTRY in ``MATCH-TABLE'' :
\ Return the LIMIT to where matched and the MATCH itself, or two zeros.
\ As a side effect, remember the place holders.
: ?MATCH    !PEES
    8 0 DO
        DUP [I] 'NOOP ^ = IF SWAP I CELLS + SWAP LEAVE THEN       \ Success
        DUP [I] 'P ^ = IF
            OVER [I] PEES SET+!
        ELSE OVER [I] OVER [I] ^ <> IF
            2DROP 0 0 LEAVE                                     \ Failure
        THEN THEN
    LOOP
;

\ For POINTER : it POINTS not yet to an ``EXIT''.
: ?TILL-EXIT   @ '(;) = 0= ;
\ For POINTER : it POINTS not yet to a ``NOOP''.
: ?TILL-NOOP   @ 'NOOP = 0= ;

\ This was a typical example of premature optimisation.
\ \ Seen the code SEQUENCE (its begin), return there MAY be a match in the table.
\ : OPT-SPECIAL?   DUP @ 'LIT = IF CELL+ CELL+ @ >FFA @ FMASK-SP AND 0= 0= ELSE
\     DROP 0 THEN ;

\ Match any entry of the table to SEQUENCE.
\ Return the LIMIT to where matched and the MATCH itself, else two zeros.
: ?MM   MATCH-TABLE
    BEGIN    2DUP ?MATCH DUP IF 2SWAP 2DROP EXIT THEN 2DROP
        16 CELLS + DUP ?TILL-EXIT WHILE REPEAT
    2DROP 0 0 ;

\ If ITEM is a place holder, replace it by the next placeholder DATA.
: ?PEE? DUP ID. DUP 'P = IF DROP PEES SET+@ THEN ;

\ Copy MATCH to ``HERE'' filling in the place holders.
: COPY-MATCH   !PEES   8 CELLS +
    BEGIN DUP ?TILL-NOOP WHILE DUP @ ?PEE? , CELL+ REPEAT
    DROP
;

\ For an ITEM in a high level word, return the next ITEM.
\ So it skips also ``ITEM'' 's inline data.
: NEXT-ITEM NEXT-PARSE 2DROP ;

\ For SEQUENCE : copy its first item to ``HERE'' possibly
\ replacing it by a match optimisation.
\ Leave sequence BEGIN' of what is still to be handled.
:  ?MATCH-EXEC?
        DUP ?MM DUP IF
            COPY-MATCH SWAP DROP
        ELSE
             2DROP DUP NEXT-ITEM >HERE
        THEN
;

\ Find optimisation patterns in the SEQUENCE of high level code
\ and perform optimisation while copying to ``HERE'' ,
\ Do not initialise, or terminate.
: (MATCH) BEGIN DUP ?TILL-EXIT WHILE ?MATCH-EXEC? REPEAT DROP ;

\ Optimise a SEQUENCE using pattern matching.
: OPTIMISE   HERE SWAP    (MATCH)   POSTPONE (;)  ;

\ ----------------------------------------------------------------
\ Optimise DEA by expansion plus applying optimations to the expanded code.
: OPT-EXPAND   !OPT-START   >DFA DUP @ EXPAND OPTIMISE    SWAP ! ;

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
\D : test1 1 2 + 3 4 AND OR ;
\D 'test1 OPTIMISE-O
\D "EXPECT `` F '' :" CR TYPE CRACK test1
\D : test2 1 2 SWAP ;
\D 'test2 OPTIMISE-O
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test2
\D : test3 1 2 'SWAP EXECUTE ;
\D 'test3 OPTIMISE-O
\D "EXPECT `` 1 2 SWAP '' :" CR TYPE CRACK test3
\D 'test3 OPT-FOLD
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test3
\D : A0 1 ;
\D : A1 A0 A0 + ;   : A2 A1 A1 + ;    : A3 A2 A2 + ;
\D : A4 A3 A3 + ;   : A5 A4 A4 + ;    : A6 A5 A5 + ;
\D : A7 A6 A6 + ;   : A8 A7 A7 + ;    : A9 A8 A8 + ;
\D
\D : B0 A9 A9 + ;
\D 'B0 OPTIMISE-O
\D "EXPECT `` 400 '' :" CR TYPE CRACK B0
