( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ The model is that a hl definition is a sequence of execution
\ tokens. You can get the dea from an execution token.
\ Also ' is assumed to give an execution token.

REQUIRE ALIAS
REQUIRE ASSEMBLERi86
\ : \D POSTPONE \ ; IMMEDIATE
: \D ;            IMMEDIATE
\D REQUIRE DUMP

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ ______________________________________________________________________
\ Add things to our Forth such that the flags simulate am optimizable Forth
\ The most important flag of them all, not used here.
HEX
40 CONSTANT SMASK   \ Words has side effects.

10 CONSTANT RMASK   \ Return stack anomalies
20 CONSTANT WMASK   \ Working register used within code.
80 CONSTANT IMASK   \ Data is following in line.

100 CONSTANT BR-MASK  \ Definition has been B-optimised.

\ The following mask must be carefully reconsidered if any new
\ flags appear in the above.
RMASK WMASK IMASK OR OR CONSTANT AO-MASK
RMASK WMASK IMASK OR OR CONSTANT BO-MASK
DECIMAL

\D IMASK    ' BRANCH >FFA OR!
\D IMASK    ' 0BRANCH >FFA OR!
\D IMASK    ' (LOOP) >FFA OR!
\D IMASK    ' (+LOOP) >FFA OR!
\D IMASK    ' (DO) >FFA   OR!
\D IMASK    ' (?DO) >FFA  OR!
\D IMASK    ' LIT >FFA    OR!
\D \ Just block optimisation here, because we are not ready for
\D \ all out optimisation :
\D AO-MASK  ' I >FFA      OR!
\D AO-MASK  ' . >FFA      OR!

\ ______________________________________________________________________

\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  ASSEMBLER  LODS, W1|   JMPO, D0| [AX] ; PREVIOUS

\ The common content of all high level definitions.
\ Catch before ``NEXT'' is optimised!
'NEXT @ CONSTANT DOCOL

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

\ The sequence of bytes that forms next, as a string.
\ (This means you can $@ it.)
CREATE NEXT-IDENTIFICATION 0 , NEXT
HERE NEXT-IDENTIFICATION CELL+ -   NEXT-IDENTIFICATION !
\D ." Expect NEXT-IDENTIFICATION to contain NEXT :" CR
\D : DUMP-STRING
\D     OVER H. SPACE DUP H. DUMP ;
\D  NEXT-IDENTIFICATION $@ DUMP-STRING

\ For some code at ADDRESS, find the start ADDRESS of its ``NEXT''.
: >NA   BEGIN DUP NEXT-IDENTIFICATION $@ CORA WHILE 1+ REPEAT ;

\D ." Expect 1 :" 'DROP >CFA @ DUP >NA SWAP - . CR

\ Fetch the code from XT , return it as a Forth STRING.
: CODE@  @ DUP >NA OVER - ;

\ Store a STRING with code in the dictionary.
: CODE, HERE OVER ALLOT SWAP CMOVE ;

\D ." Expect POP AX instruction :" CR
\D 'DROP CODE@ DUMP-STRING

\ Leave an incremented ADDRESS and the CONTENT.
'$@ ALIAS @+

\ For DEA: it IS A-optimisable.
: A-OPTIMISABLE?   >FFA @   AO-MASK AND   0=   ;

\ Try and optimise the DEA with respect to method `A' (inlining.)
\ Reach trough to underlying levels.
: OPTIMISE-A ;

\ For a parse ADDRESS return an incremented parse ADDRESS, its
\ CONTENT and a go on FLAG.
: NEXT-PARSE
   @+ >R   R@ CFA> >FFA @ IMASK AND IF CELL+ THEN
   R@
\D   R@ CFA> ID.
   R> '(;) <> ;

\ For all elements of DEA attempt a a ``OPTIMISE-A'' .
\ Leave a flag indicating that the DEA itself is a-optimisable.
: OPTIMISE-A1 DUP HIGH-LEVEL? IF
    -1 >R
    >DFA @ BEGIN NEXT-PARSE WHILE
    CFA> DUP OPTIMISE-A
    A-OPTIMISABLE? R> AND >R REPEAT
    2DROP R>  \D .S
    ELSE
       DROP 0  \D .S
    THEN
;

\ Concatenate the code of all elements of DEA , turning it into
\ a code definition. This must be allowed or we crash.
: OPTIMISE-A2 DUP >R HERE >R
    >DFA @
    BEGIN $@ DUP '(;) <> WHILE CODE@ HERE OVER ALLOT SWAP CMOVE REPEAT
    2DROP NEXT
    R> R> >CFA ! ;

\ Resolve OPTIMISE-A
: (OPTIMISE-A)
    DUP OPTIMISE-A1 IF OPTIMISE-A2 _ THEN DROP ;
'(OPTIMISE-A) >DFA @   'OPTIMISE-A >DFA !

\D : OOPS NOOP NOOP NOOP ;
\D ." Expect no crash:" 'OOPS OPTIMISE-A OOPS ." no crash"

\D 'OOPS CODE@ DUMP-STRING

\D \ -------------- sample code --------------------------
\D \ This is the code we seek to optimise.
\D ( AA : Nesting_benchmark )
\D : A0 ;
\D : A1 A0 A0 ;   : A2 A1 A1 ;    : A3 A2 A2 ;
\D : A4 A3 A3 ;   : A5 A4 A4 ;    : A6 A5 A5 ;
\D : A7 A6 A6 ;   : A8 A7 A7 ;    : A9 A8 A8 ;
\D
\D : B0 A9 A9 ;
\D : B1 B0 B0 ;   : B2 B1 B1 ;    : B3 B2 B2 ;
\D : B4 B3 B3 ;   : B5 B4 B4 ;    : B6 B5 B5 ;
\D : B7 B6 B6 ;   : B8 B7 B7 ;    : B9 B8 B8 ;
\D : BA B9 B9 ;
\D
\D : TEST 0 DO BA LOOP ;
\D : Q 0 DO 10000 TEST I . LOOP ;
\D \ -------------- end of sample code --------------------------
\D 'Q OPTIMISE-A
\D ." Expect  0 1 2 3 4 :" 5 Q CR

\ For some hl-code at ADDRESS, return the ADDRESS where (;) sits.
: >SA   BEGIN DUP @ '(;) <> WHILE CELL+ REPEAT ;

\ Fetch the hl code from XT , return it as a Forth STRING.
: HL-CODE@  CFA> >DFA @ DUP >SA OVER - ;

\ Store a STRING with hl-code in the dictionary.
'CODE, ALIAS HL-CODE,

\ For DEA: it IS B-optimisable.
: B-INLINABLE?   DUP >FFA @   BO-MASK AND   0=   SWAP HIGH-LEVEL? AND ;

\ Copy the SEQUENCE of high level code to ``HERE'' expanding it.
: EXPAND
        BEGIN NEXT-PARSE WHILE HL-CODE@ HL-CODE, REPEAT 2DROP
        POSTPONE (;)
;

\ Optimise DEA
: OPTIMISE-B
    DUP HIGH-LEVEL? IF
        DUP >R HERE >R
        DUP >DFA @ EXPAND
        BR-MASK OVER >FFA OR!
        R> R> >DFA !
   THEN DROP ;

\D : NOOP ;
\D ." Expect no crash:" 'NOOP OPTIMISE-B NOOP ." no crash"

\D : OOPS NOOP NOOP NOOP ;
\D ." Expect no crash:" 'OOPS OPTIMISE-B OOPS ." no crash"

\D 'OOPS HL-CODE@ DUMP-STRING
\ Try and optimise the DEA with respect to method `B' (HL inlining.)
\ Reach trough to underlying levels.
: OPTIMISE-B ;

\ For all elements of DEA attempt a ``OPTIMISE-B'' .
\ Leave a flag indicating that the DEA itself is b-optimisable.
: OPTIMISE-B1 DUP HIGH-LEVEL? OVER >FFA @ BR-MASK AND 0= AND IF
    -1 >R
    >DFA @ BEGIN NEXT-PARSE WHILE
    CFA> DUP OPTIMISE-B
    B-INLINABLE? R> AND >R REPEAT
    2DROP R>  \D .S
    ELSE
       DROP 0  \D .S
    THEN
;

\ Concatenate the code of all elements of DEA , turning it into
\ a collapsed HL definition. This must be allowed or we crash.
: OPTIMISE-B2 DUP >R HERE >R
    >DFA @
    BEGIN NEXT-PARSE WHILE HL-CODE@ HL-CODE, REPEAT
    2DROP POSTPONE (;)
    R> R@ >DFA !
    BR-MASK R> >FFA OR! ;

\ Resolve OPTIMISE-B
: (OPTIMISE-B)
    DUP OPTIMISE-B1 IF OPTIMISE-B2 _ THEN DROP ;
'(OPTIMISE-B) >DFA @   'OPTIMISE-B >DFA !



\D \ -------------- sample code --------------------------
\D \ This is the code we seek to optimise.
\D ( AA : Nesting_benchmark )
\D : A0 ;
\D : A1 A0 A0 ;   : A2 A1 A1 ;    : A3 A2 A2 ;
\D : A4 A3 A3 ;   : A5 A4 A4 ;    : A6 A5 A5 ;
\D : A7 A6 A6 ;   : A8 A7 A7 ;    : A9 A8 A8 ;
\D
\D : B0 A9 A9 ;
\D : B1 B0 B0 ;   : B2 B1 B1 ;    : B3 B2 B2 ;
\D : B4 B3 B3 ;   : B5 B4 B4 ;    : B6 B5 B5 ;
\D : B7 B6 B6 ;   : B8 B7 B7 ;    : B9 B8 B8 ;
\D : BA B9 B9 ;
\D
\D : TEST 0 DO BA LOOP ;
\D : Q 0 DO 10000 TEST I . LOOP ;
\D \ -------------- end of sample code --------------------------
\D ( 'BA OPTIMISE-B)
\D  'Q OPTIMISE-B
\D ." Expect  0 1 2 3 4 :" 5 Q CR
