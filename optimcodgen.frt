( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)
: \D POSTPONE \ ; IMMEDIATE
\D REQUIRE DUMP
REQUIRE ASSEMBLERi86


\ Define a ``NEXT'' sequence. It must have the exact code that is in
\ the kernel.
: NEXT  ASSEMBLER  LODS, W1|   JMPO, D0| [AX] ; PREVIOUS

\ The common content of all high level definitions.
\ Catch before ``NEXT'' is optimised!
'NEXT >CFA @ CONSTANT DOCOL

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

\ Fetch the code from DEA , return it as a Forth STRING.
: CODE@  >CFA @ DUP >NA OVER - ;
\D ." Expect POP AX instruction :" CR
\D 'DROP CODE@ DUMP-STRING
\ The most important flag of them all, not used here.
HEX 40 CONSTANT SMASK   \ Words has side effects.

HEX 10 CONSTANT RMASK   \ Return stack anomalies
HEX 20 CONSTANT WMASK   \ Working register used within code.
HEX 80 CONSTANT IMASK   \ Data is following in line.

\ The following mask must be carefully reconsidered if any new
\ flags appear in the above.
RMASK WMASK IMASK OR OR CONSTANT AO-MASK \
DECIMAL

\ For DEA: it IS A-optimisable.
: A-OPTIMISABLE?   >FFA @   AO-MASK AND   0=   ;

\ Forward definition.
: OPTIMISE-A ;

\ For a parse ADDRESS return an incremented parse ADDRESS, its
\ CONTENT and a go on FLAG.
: NEXT-PARSE
   $@ >R   R@ >FFA @ IMASK AND IF CELL+ THEN
   R@
   R@ ID.
   R> '(;) <> ;

\ For all elements of DEA attempt a a ``OPTIMISE-A'' .
\ Leave a flag indicating that the DEA itself is a-optimisable.
: OPTIMISE-A1 DUP HIGH-LEVEL? IF
    -1 >R
    >DFA @ BEGIN NEXT-PARSE WHILE
    DUP OPTIMISE-A
    A-OPTIMISABLE? R> AND >R REPEAT
    2DROP R>
    ELSE
       DROP 0
    THEN
;

\ Concatenate the code of all elements of DEA , turning it into
\ a code definition. This must be allowed or we crash.
: OPTIMISE-A2 DUP >R HERE >R
    >DFA @
    BEGIN $@ DUP '(;) <> WHILE CODE@ HERE OVER ALLOT SWAP CMOVE REPEAT
    2DROP NEXT
    R> R> >CFA ! ;

\ Try and optimise the DEA with respect to method `A' (inlining.)
\ Reach trhough to underlying levels.
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
\D ' (LOOP) >FFA IMASK  TOGGLE
\D ' (DO) >FFA IMASK  TOGGLE
\D ' LIT >FFA IMASK  TOGGLE
\D \ Just block optimisation here, because we are not ready for
\D \ all out optimisation :
\D ' I >FFA AO-MASK  TOGGLE
\D ' . >FFA AO-MASK  TOGGLE
EXIT
\D 'Q OPTIMISE-A
\D ." Expect  1 2 3 4 5 :" 5 Q CR
