( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

REQUIRE $
\   : \D POSTPONE \ ; IMMEDIATE
  : \D ;            IMMEDIATE

HEX
\ ----------------------    ( From analyser.frt)
100 CONSTANT FMASK-HO    \ This definition has been high level optimised.
200 CONSTANT FMASK-HOB   \ This definition cannot be high level optimised.
DECIMAL
\ ----------------------    ( From optimiser.frt)
\ Store a STRING with hl-code in the dictionary.
: HL-CODE, HERE OVER ALLOT SWAP CMOVE ;

\ For a parse ADDRESS return an incremented parse ADDRESS, its
\ CONTENT and a go on FLAG.
: NEXT-PARSE
   @+ >R   R@ CFA> >FFA @ FMASK-IL AND IF CELL+ THEN
\   R@ CFA> 'SKIP = IF @+ + ALIGNED THEN
   R@
\D   R@ CFA> ID.
   R> '(;) <> ;

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

\ For DEA: it IS B-optimisable.
: B-INLINABLE?   DUP >FFA @   FMASK-HOB AND   0=   SWAP HIGH-LEVEL? AND ;

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
: COMBINE-SE    SE:1>2 SWAP -   CSC +! ;

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
        !CSP EXECUTE-DURING-COMPILE THROW-AWAY ^ COMPILE-CONSTANTS ?CSP
    THEN
;

\ Combining the effect of DEA into the current state, return
\ "the folding optimisation still HOLDS".
: CAN-FOLD?   NS? OVER SE@ ENOUGH-POPS AND ;

VARIABLE CURRENT-DEA

\ For BEGIN END : if ``CURRENT-DEA'' allows it, add to optimisation,
\ else cash it and restart it. ``BEGIN'' ``END'' is the code copied.
\ (Mostly ``DEA"" plus inline belonging to it.)
\ Return a new BEGIN for the code to be moved, mostly the old ``END''.
:  ?OPT-FOLD?
    CURRENT-DEA @ DUP CAN-FOLD?
    IF ^
        SE@ COMBINE-SE               ( DEA -- )
        >HERE                        ( BEGIN END -- BEGIN' )
    ELSE ^
        DROP CASH  ( DEA -- )
        >HERE                        ( BEGIN END -- BEGIN' )
        !OPT-START
    THEN ;

\ FIXME : all special optimisations must be tried only if the
\ the special optimisations flag is on, not always.

\ FIXME : the stack effect is no good, not uniform.

\ Combining the effect of DEA into the current state, return
\ "the ``EXECUTE'' optimisation IS possible"
: CAN-EXEC?   'EXECUTE = CSC @ 0 > AND ;

\ For ``CURRENT-DEA'' try the execute optimisation.
\ For BEGIN END: leave sequence BEGIN' END' of what is still to be handled.
:  ?OPT-EXEC?
    CURRENT-DEA @ CAN-EXEC? IF
        CASH
        HERE 1 CELLS - @  -2 CELLS ALLOT ,
        !OPT-START
        EMPTY>
    THEN ;

\ Copy the SEQUENCE of high level code to ``HERE'' ,  possibly optimizing it.
: (EXPAND)
    BEGIN DUP CR &N EMIT ^
        NEXT-PARSE
    WHILE CURRENT-DEA !
        CR &E EMIT ^ ?OPT-EXEC?
        CR &F EMIT ^ ?OPT-FOLD?
    REPEAT CR &L EMIT ^ DROP DROP DROP
;

\ Copy the SEQUENCE of high level code to ``HERE'' ,  possibly folding it.
: EXPAND
    !OPT-START
    (EXPAND)
    ^ CASH   ^ POSTPONE (;) ^
;

\ Try and optimise the DEA with respect to method `B' (HL inlining.)
\ Reach trough to underlying levels.
CREATE OPTIMISE-O

\ For all elements of DEA attempt a ``OPTIMISE-O'' .
\ Leave a flag indicating that the DEA itself is b-optimisable.
: OPTIMISE-O1 DUP HIGH-LEVEL? OVER >FFA @ FMASK-HO AND 0= AND IF
    -1 >R
    >DFA @ BEGIN NEXT-PARSE WHILE
    CFA> DUP OPTIMISE-O
    B-INLINABLE? R> AND >R REPEAT
    2DROP R>  \D .S
    ELSE
       DROP 0  \D .S
    THEN
;

\ Concatenate the code of all elements of DEA , turning it into
\ a collapsed HL definition. This must be allowed or we crash.
: OPTIMISE-O2 DUP >R HERE >R
    >DFA @ !OPT-START
    BEGIN NEXT-PARSE ^ WHILE >DFA @ (EXPAND) REPEAT
    2DROP CASH POSTPONE (;)
    R> R@ >DFA !
    FMASK-HO R> >FFA OR! ;

\ Resolve OPTIMISE-O
: (OPTIMISE-O)
    DUP OPTIMISE-O1 IF OPTIMISE-O2 _ THEN DROP ;
'(OPTIMISE-O)   'OPTIMISE-O 3 CELLS MOVE

\ Optimise DEA regards folding.
: OPT-FOLD  >DFA HERE    OVER @ EXPAND   SWAP ! ;
\D : test 1 SWAP 3 2 SWAP ;
\D 'test OPT-FOLD
\D "EXPECT `` 1 SWAP 2 3 '' :" CR TYPE CRACK test
\D : test1 1 2 + 3 4 * OR ;
\D 'test1 OPT-FOLD
\D "EXPECT `` F '' :" CR TYPE CRACK test1
\D : test2 1 2 SWAP ;
\D 'test2 OPT-FOLD
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test2
\D : test3 1 2 'SWAP EXECUTE ;
\D 'test3 OPT-FOLD
\D "EXPECT `` 1 2 SWAP '' :" CR TYPE CRACK test3
\D 'test3 OPT-FOLD
\D "EXPECT `` 2 1 '' :" CR TYPE CRACK test3
