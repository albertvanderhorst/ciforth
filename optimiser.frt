( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

REQUIRE $
      : \D POSTPONE \ ; IMMEDIATE    : ^^ ;
\      : \D ;            IMMEDIATE : ^^ !CSP DUP CRACK-CHAIN ?CSP ;

\ ----------------------    ( From optimiser.frt)
\ Store a STRING with hl-code in the dictionary.
: HL-CODE, HERE OVER ALLOT SWAP CMOVE ;
\ Store a LOW HIGH range with hl-code in the dictionary.
: HL-RANGE, OVER - HL-CODE, ;

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

\ For an ITEM in a high level word, return the next ITEM.
\ So it skips also ``ITEM'' 's inline data.
: NEXT-ITEM NEXT-PARSE 2DROP ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ For a soft FLAG, returns a FORTH flag (-1/0)
: 0<>   0= 0= ;

\ ----------------------    ----------------------     ----------------------


\ For DEA : it HAS no side effects, input or output.
: NS?   >FFA @ FMASK-NS AND FMASK-NS = ;

\ How MANY stack cells on top contain a compile time constant?
VARIABLE CSC


\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE OPT-START

: !OPT-START   HERE OPT-START !   0 CSC ! ;

\ There has been made progress during the last optimisation.
VARIABLE PROGRESS            : !PROGRESS 0 PROGRESS ! ;

\ For STACKEFFECTNIBBLE : it IS no good, because it is unknown or variable.
: NO-GOOD DUP $F = SWAP 0= OR ;

\ For STACKEFFECTBYTE : it IS good, neither unknown or variable.
: SE-GOOD DUP $F AND NO-GOOD SWAP 4 RSHIFT NO-GOOD OR  0= ;

\ For STACKEFFECTBYTE : we KNOW we have enough pops, i.e. after applying
\ this stackeffect the virtual stack doesn't underflow.
: ENOUGH-POPS   DUP SE-GOOD   SWAP 4 RSHIFT 1- CSC @ > 0=  AND ;

\ ---------------------------------------------------------------------
\ INCLUDE nss.frt

\ For DEA : it HAS no side effects regards stack.
: NSST?   >FFA @ FMASK-ST AND FMASK-ST = ;

FMASK-ST '.S >FFA OR!
FMASK-ST 'DEPTH >FFA OR!

\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE SWAPPER-START

\ The minimum step depth we have encountered.
VARIABLE MIN-DEPTH

: REMEMBER-DEPTH CSC @   MIN-DEPTH @   MIN MIN-DEPTH ! ;

: !SWAPPER-START   HERE SWAPPER-START !   CSC @  MIN-DEPTH ! ;

\ Combine a STACKEFFECTBYTE into ``CSCPP''.
\ Remember : both nibbles have offset 1!
: COMBINE-CSC    SE:1>2 SWAP 1- NEGATE CSC +! REMEMBER-DEPTH 1- CSC +! ;

\ For DEA we are still in the swappable code, i.e. we don't dig below
\ the constant stack entries, and we have no stack side effects that kill.
\ At this point ``CSC'' contains the remaining stack depth, i.e. the
\ number of constants not touched by the swappable code. Those can be
\ placed after the swappable code.
: STILL-SWAPPING? DUP NSST? 0= SWAP SE@ ENOUGH-POPS AND ;

\ We are at an stable point. i.e. we consumed all the constants,
\ we may have replaced ourselves. Or we can't swap anyway.
: STABLE? CSC @ MIN-DEPTH @ = MIN-DEPTH 1 < OR ;

\ For DEA : adding it would result in a not yet stable sequence.
\ Otherwise the optimisation is known to end here or there is no optimisation.
: NOT-YET-STABLE?
DUP STILL-SWAPPING? IF SE@ COMBINE-CSC STABLE? 0= ELSE -1 MIN-DEPTH !
DROP 0 THEN ;

\ From ADDRESS collect all code to be executed before (some of the) constants
\ collected. Leave ADDRESS LENGTH (in address units.)
: COLLECT-SWAPPER !SWAPPER-START
    DUP ?NOT-EXIT 0= IF 0 EXIT THEN
    DUP BEGIN NEXT-PARSE SWAP NOT-YET-STABLE? AND WHILE REPEAT
     OVER -   STABLE? AND
;

\ Assuming there has been folding, increment SEQUENCE to point past all
\ constants at its start. Return incrementer SEQUENCE.
: COUNT-LIT BEGIN DUP @ 'LIT = WHILE 2 CELLS + 1 CSC +! REPEAT ;
\ For SEQUENCE , return the POINTER to first constant (``LIT'')
: FIND-LIT BEGIN DUP @ 'LIT <> OVER ?NOT-EXIT AND WHILE NEXT-ITEM  REPEAT ;

VARIABLE CODE-MARKER

\ For SEQUENCE , return the STRING that is swappable.
\ 0 length : no good.
: (FIND-SWAPPABLE) FIND-LIT !OPT-START DUP CODE-MARKER ! COUNT-LIT COLLECT-SWAPPER ;

\ For SEQUENCE , return the STRING that is swappable.
\ FIXME : ?NOT-EXIT can be dropped here.
\ 0 length : there is none.
: FIND-SWAPPABLE BEGIN (FIND-SWAPPABLE) OVER ?NOT-EXIT OVER 0= AND WHILE DROP REPEAT ;

\ Get from SEQUENCE four boundaries, delineating 3 areas. Return A B C D.
\ The order to be compiled is A-B C-D B-C .
: GET-PIECES   DUP  FIND-SWAPPABLE   ( D) + >R
    CODE-MARKER @ ( H.) DUP CSC @ 2 * CELLS + R> ;

\ Inspect A B C and report into ``PROGRESS'' whether there is any
\ optimisation by swapping. For that A-B and C-D must be both non-empty.
\ Leave A B C.
: ?PROGRESS   >R 2DUP <> R> SWAP  >R 2DUP <> R>   AND   PROGRESS OR! ;

\ For addresses A B C D compile A-B ordinary sequence, C-D no stack side
\ effect sequence, B-C postponable constants sequence
: COMPILE-PIECES ( C) OVER >R   2SWAP   ( B) DUP >R
    HL-RANGE,  HL-RANGE,   R> R> HL-RANGE, ;

\ Reorder a SEQUENCE to delay constants as much as possible.
\ Return rearragned SEQUENCE.
: REORDER HERE SWAP
    BEGIN GET-PIECES ?PROGRESS DUP >R COMPILE-PIECES R> DUP ?NOT-EXIT 0= UNTIL DROP
POSTPONE (;)  ;

\ ---------------------------------------------------------------------

\ Execute at compile time the optimisable code we have collected from
\ ``OPT-START''
\ The result is supposedly ``CSC'' stack items.
: EXECUTE-DURING-COMPILE   POSTPONE (;)    OPT-START @ >R ;

\ Throw away the executable code that is to be replaced with optimised code.
: THROW-AWAY   OPT-START @ DP ! ;

\ Compile a NUMBER of constants instead of the code optimised away.
\ They sit on the stack now, under ``NUMBER''.
\ We need to store backwards to reverse them.
: COMPILE-CONSTANTS \ DUP 0= 13 ?ERROR
    HERE >R  CELLS 2* ALLOT   HERE >R
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
        !CSP
        EXECUTE-DURING-COMPILE
        THROW-AWAY
        CSC @ COMPILE-CONSTANTS
        ?CSP
    THEN
;

\ Combining the effect of DEA into the current state, return
\ "the folding optimisation still HOLDS".
: CAN-FOLD?   DUP NS? SWAP SE@ ENOUGH-POPS AND ;

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
        DROP ( DUP COLLECT-SWAPPER) CASH  ( DEA -- )
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
: FOLD   !OPT-START HERE SWAP    (FOLD)   CASH   POSTPONE (;)  ;

\ Optimise DEA regards folding.
: OPT-FOLD    >DFA DUP @ FOLD   SWAP ! ;

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
: EXPAND   !OPT-START HERE SWAP    (EXPAND)   CASH   POSTPONE (;)  ;


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
    'ALIGN-NOOPS ALIAS |                         \ Convenience alias.
    : ]L ] POSTPONE LITERAL ; IMMEDIATE \ Convenience alias.

: (MATCH-TABLE)                         |
\ `` MATCH-TABLE'' points here :
'P  EXECUTE             | P                       |       \ Execute optimisation
'P  + 'P  +             | 'P  'P  + +             |       \ Associativity optimisation
'P  + 'P  -             | 'P  'P  - +             |
'P  - 'P  +             | 'P  'P  SWAP - +        |
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
( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------)
( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP DO I . 0 CELL+ +LOOP ;   ( Print non-empty SET )
: SET+@   DUP >R @ @ 0 CELL+ R> +! ;   ( retract from SET. Leave ITEM )

STRIDE SET PEES
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
    STRIDE 0 DO
        DUP [I] 'NOOP = IF SWAP I CELLS + SWAP LEAVE THEN       \ Success
        DUP [I] 'P = IF
            OVER [I] PEES SET+!
        ELSE OVER [I] OVER [I] <> IF
            2DROP 0 0 LEAVE                                     \ Failure
        THEN THEN
    LOOP
;

\ This was a typical example of premature optimisation.
\ \ Seen the code SEQUENCE (its begin), return there MAY be a match in the table.
\ : OPT-SPECIAL?   DUP @ 'LIT = IF CELL+ CELL+ @ >FFA @ FMASK-SP AND 0<> ELSE
\     DROP 0 THEN ;

\ Match any entry of the table to SEQUENCE.
\ Return the LIMIT to where matched and the MATCH itself, else two zeros.
: ?MM   MATCH-TABLE
    BEGIN    2DUP ?MATCH DUP IF 2SWAP 2DROP EXIT THEN 2DROP
        STRIDE 2 * CELLS + DUP ?NOT-EXIT WHILE REPEAT
    2DROP 0 0 ;

\ If ITEM is a place holder, replace it by the next placeholder DATA.
: ?PEE? DUP 'P = IF DROP PEES SET+@ THEN ;

\ Copy MATCH to ``HERE'' filling in the place holders.
: COPY-MATCH   !PEES   STRIDE CELLS +
    BEGIN DUP ?TILL-NOOP WHILE DUP @ ?PEE? , CELL+ REPEAT
    DROP
;

\ For SEQUENCE : copy its first item to ``HERE'' possibly
\ replacing it by a match optimisation.
\ Leave sequence BEGIN' of what is still to be handled.
:  ?MATCH-EXEC?
        DUP ?MM DUP IF
            COPY-MATCH SWAP DROP    -1 PROGRESS !
        ELSE
             2DROP DUP NEXT-ITEM >HERE
        THEN
;

\ Find optimisation patterns in the SEQUENCE of high level code
\ and perform optimisation while copying to ``HERE'' ,
\ Do not initialise, or terminate.

: (MATCH) BEGIN DUP ?NOT-EXIT WHILE ?MATCH-EXEC? REPEAT DROP ;

\ Optimise a SEQUENCE using pattern matching.
: OPTIMISE   HERE SWAP    (MATCH)   POSTPONE (;)  ;

\ ----------------------------------------------------------------
\ Optimise DEA by expansion plus applying optimations to the expanded code.
: OPT-EXPAND   >DFA DUP @  ^^
    BEGIN !PROGRESS EXPAND ^^ OPTIMISE ^^ REORDER ^^ PROGRESS @ WHILE REPEAT
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
