( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

REQUIRE $

\ How MANY stack cells on top contain a compile time constant?
VARIABLE CSC

\ From WHERE do we have optimisable code. (Ends at ``HERE'')
VARIABLE OPT-START           0 OPT-START !


: !OPT-START   HERE OPT-START !   0 CSC ! ;

\ For STACKEFFECTNIBBLE : it IS no good, because it is unknown or variable.
: NO-GOOD DUP $F = SWAP 0= OR ;

\ For STACKEFFECTBYTE : it IS good, neither unknown or variable.
: SE-GOOD DUP $F AND NO-GOOD SWAP 4 RSHIFT NO-GOOD OR  0= ;

\   Start the optimisation if we didn't already.
: ?OPT-START    OPT-START @ 0= IF   !OPT-START THEN ;

\ For STACKEFFECTBYTE : we CAN still optimise, because we know we have
\ sufficiantly constant stack cells.
: STILL-OPTIMISE   DUP SE-GOOD   SWAP 4 RSHIFT 1- CSC @ > 0=  AND ;


\ Combine a STACKEFFECTBYTE into ``CSC''.
\ ``-'' works here because both nibbles have offset 1!
: COMBINE-SE    SE:1>2 SWAP -   CSC +! ;

\ Treat the DEA that is know to be ``NS''. Combine it to the
\ optimisation, if possible. Return: we ARE still optimising.
: TREAT-NS
    ?OPT-START
    SE@ DUP STILL-OPTIMISE IF
        COMBINE-SE  -1
    ELSE
        DROP 0 OPT-START ! 0
    THEN
;

\ Execute at compile time the ``NONAME'' word: the optimisable code we have collected from
\ ``OPT-START''
\ The result is supposedly ``CSC'' stack items.
: NONAME ;
: EXECUTE-DURING-COMPILE
       POSTPONE (;)    OPT-START @ 'NONAME >DFA !    NONAME ;


\ Throw away the executable code that has been optimised.
: THROW-AWAY   OPT-START @ DP ! ;

\ Compile ``CSC'' constants instead of the code optimised away.
\ They sit on the stack now. We need a buffer to reverse them.
CREATE BUFFER 16 ALLOT
: COMPILE-CONSTANTS
    BUFFER CSC @ CELLS BOUNDS DO I ! 1 CELLS +LOOP
    BUFFER CSC @ 1- CELLS OVER + DO POSTPONE LIT I @ , -1 CELLS +LOOP
    '(;) HERE !     \ To prevent too many crashes while testing.
;

\ Optimisation is over. Run the optimisable code and compile constants
\ instead.
: TERMINATE-EXECUTE-REPLACE
    OPT-START @ HERE <> IF
        EXECUTE-DURING-COMPILE THROW-AWAY COMPILE-CONSTANTS
        !OPT-START
    THEN
;

\ Copy the range START END to the dictionary.
: COPY-WORD-INLINE OVER HL-CODE, ;

\ For DEA : it HAS no side effects, input or output.
: NS?   >FFA FMASK-NS AND FMASK-NS = ;

\ Copy the SEQUENCE of high level code to ``HERE'' expanding it and
\ possibly folding it.
: EXPAND
    !OPT-START
    BEGIN DUP >R
        NEXT-PARSE
    WHILE
        DUP
NS? DUP IF SWAP TREAT-NS AND THEN
        0= IF TERMINATE-EXECUTE-REPLACE THEN
        DUP R> OVER - HL-CODE,
    REPEAT 2DROP RDROP
    POSTPONE (;)
;
