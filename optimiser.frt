( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ This code does the folding such as descibed in the optimiser section of
\ the generic ciforth documentation.

\ It assumes the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.

REQUIRE $

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
: STILL-OPTIMISE   DUP SE-GOOD   SWAP 4 RSHIFT 1- CSC @ > 0=  AND ;


\ Combine a STACKEFFECTBYTE into ``CSC''.
\ ``-'' works here because both nibbles have offset 1!
: COMBINE-SE    SE:1>2 SWAP -   CSC +! ;

\ Combine DEA's effect into the current state, return "we CAN optimise"
: CAN-COMBINE?   NS? OVER SE@ STILL-OPTIMISE AND ;

\ Treat the DEA that is know to be ``NS''. Combine it to the
\ optimisation, if possible. Return: we ARE still optimising.
\ Add DEA to the optimisation chain, if possible. Leave it WAS possible.
:  ?TREAT-NS?
    DUP CAN-COMBINE? IF
        SE@ COMBINE-SE  -1
    ELSE
        DROP 0
    THEN ;

\ Execute at compile time the ``NONAME'' word: the optimisable code we have collected from
\ ``OPT-START''
\ The result is supposedly ``CSC'' stack items.
: NONAME ;
: EXECUTE-DURING-COMPILE
       POSTPONE (;)    OPT-START @ 'NONAME >DFA !    NONAME ;


\ Throw away the executable code that is to be replaced with optimised code.
: THROW-AWAY   OPT-START @ DP ! ;

\ Compile ``CSC'' constants instead of the code optimised away.
\ They sit on the stack now. We need a buffer to reverse them.
CREATE BUFFER 16 ALLOT
: COMPILE-CONSTANTS CSC @ 0= 13 ?ERROR
    BUFFER CSC @ CELLS BOUNDS ?DO I ! 1 CELLS +LOOP
    BUFFER CSC @ 1- CELLS OVER + ?DO POSTPONE LIT I @ , -1 CELLS +LOOP
    '(;) HERE !     \ To prevent too many crashes while testing.
;

\ Optimisation is over. Run the optimisable code and compile constants
\ instead.
: TERMINATE-EXECUTE-REPLACE
    OPT-START @ HERE <> IF   ." TER"
        EXECUTE-DURING-COMPILE THROW-AWAY COMPILE-CONSTANTS
    THEN
;



VARIABLE AAP
\ For DEA : handle its optimisation or the cashing and restart of
\ the optimisation.
:  OPT/NOOPT
    DUP CAN-COMBINE?
    DUP 0= AAP !
    IF
        SE@ COMBINE-SE
    ELSE
        DROP TERMINATE-EXECUTE-REPLACE
    THEN ;

\ Copy the SEQUENCE of high level code to ``HERE'' ,  possibly folding it.
: EXPAND
    !OPT-START
    BEGIN DUP >R
        NEXT-PARSE
    WHILE
        OPT/NOOPT
        R> 2DUP -  HL-CODE,      \ HERE H.
        AAP @ IF !OPT-START THEN
    REPEAT 2DROP RDROP
    TERMINATE-EXECUTE-REPLACE   POSTPONE (;)
;

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
