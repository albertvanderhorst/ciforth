( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)
: \D ; IMMEDIATE
\D REQUIRE DUMP
REQUIRE ASSEMBLERi86



\ The sequence of bytes that ``NEXT'' consists of, padded with zero's.
\ It is a string (You can $@ it.)
\ Work around the problem that the built in assembler doesn't generate
\ the exact code that is in the kernel.
: NEXT  ASSEMBLER  LODS, W1|   JMPO, D0| [AX] ; PREVIOUS

\ The common content of all high level definitions.
\ Catch before ``NEXT'' is optimised!
'NEXT >CFA @ CONSTANT DOCOL

\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

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
HEX 10 CONSTANT RMASK   \ No return stack anomalies
HEX 20 CONSTANT WMASK   \ Working register not used within code.

\ The following mask must be carefully reconsidered if any new
\ flags appear in the above.
RMASK WMASK OR CONSTANT AO-MASK \
DECIMAL

\ For DEA: it IS A-optimisable.
: A-OPTIMISABLE?   >FFA @   AO-MASK AND   0=   ;

\ Forward definition.
: OPTIMISE-A ;

\ Scan the elements of DEA and return whether all ARE a-optimisable.
: OPTIMISE-A1 DUP HIGH-LEVEL? IF
    -1 >R
    >DFA @ BEGIN $@ DUP '(;) <> WHILE
    DUP OPTIMISE-A
    A-OPTIMISABLE? R> AND >R REPEAT
    2DROP R>
    ELSE
       DROP 0
    THEN
;

\ Concatenate the code of all elements of DEA , turning it into
\ a code definition. Only do this if it is allowed!
: OPTIMISE-A2 DUP >R HERE >R
    >DFA @
    BEGIN $@ DUP '(;) <> WHILE CODE@ HERE OVER ALLOT SWAP CMOVE REPEAT
    2DROP NEXT
    R> R> >CFA ! ;

\ Optimise the DEA with respect to method `A' (inlining.)
: (OPTIMISE-A)
    DUP OPTIMISE-A1 IF OPTIMISE-A2 _ THEN DROP ;
'(OPTIMISE-A) >DFA @   'OPTIMISE-A >DFA !

\D : OOPS NOOP NOOP NOOP ;
\D ." Expect no crash:" 'OOPS (OPTIMISE-A) OOPS ." no crash"

EXIT
\D 'OOPS CODE@ DUMP-STRING
\D ." Expect true :" 'OOPS OPTIMISE-A .

\ For all elements of DEA do a ``OPTIMISE-A'' .
\ Leave a flag indicating that all elements
\ are A-OPTIMISABLE
: OPTIMISE-A1      ;

\ All elements of DEA are `A-OPTIMISABLE'.
\ Concatenate all the code contents.
: OPTIMISE-A2                       ;
