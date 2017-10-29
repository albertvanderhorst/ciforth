( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ The model is that a hl definition is a sequence of execution
\ tokens. You can get the dea from an execution token.
\ Also ' is assumed to give an execution token.


\ This file assumes in Forth all stack effects have been filled in,
\ and some special properties and optimisation bits.

\ List here: FMASK-IL

WANT ALIAS
\ : \D POSTPONE \ ; IMMEDIATE
  : \D ;            IMMEDIATE
\D REQUIRE DUMP
\D : DUMP-STRING
\D     OVER H. SPACE DUP H. DUMP ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ ______________________________________________________________________
\ Add things to our Forth such that the flags simulate am optimizable Forth
\ The most important flag of them all, not used here.
HEX
40 CONSTANT SMASK   \ Words has side effects.

10 CONSTANT RMASK   \ Return stack anomalies
20 CONSTANT WMASK   \ Working register used within code.

100 CONSTANT BR-MASK  \ Definition has been B-optimised.

\ The following mask must be carefully reconsidered if any new
\ flags appear in the above.
RMASK WMASK FMASK-IL OR OR CONSTANT AO-MASK
RMASK WMASK FMASK-IL OR OR CONSTANT BO-MASK
DECIMAL

\D FMASK-IL    ' BRANCH >FFA OR!
\D FMASK-IL    ' 0BRANCH >FFA OR!
\D FMASK-IL    ' (LOOP) >FFA OR!
\D FMASK-IL    ' (+LOOP) >FFA OR!
\D FMASK-IL    ' (DO) >FFA   OR!
\D FMASK-IL    ' (?DO) >FFA  OR!
\D FMASK-IL    ' LIT >FFA    OR!
\D \ Just block optimisation here, because we are not ready for
\D \ all out optimisation :
\D AO-MASK  ' I >FFA      OR!
\D AO-MASK  ' . >FFA      OR!

\ ______________________________________________________________________


\ For DEA : it IS high-level.
: HIGH-LEVEL? >CFA @ DOCOL = ;

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
   @+ >R   R@ >FFA @ FMASK-IL AND IF CELL+ THEN 
\   R@ 'SKIP = IF @+ + ALIGNED THEN 
   R@
\D   R@ ID. 
   R> '(;) <> ;

\ For some hl-code at ADDRESS, return the ADDRESS where (;) sits.
: >SA   BEGIN NEXT-PARSE SWAP DROP 0= UNTIL ;
\ Doesn't worl anymore after the optimisalisation, very puzzling.
\ \D : JAN 14 BEGIN 12 WHILE "AAA" TYPE REPEAT 18 ;
\D : JAN 14 BEGIN 12 WHILE &A EMIT REPEAT 18 ;   \ Alternative test.
\D 'JAN >DFA @ >SA 

\ Fetch the hl code from XT , return it as a Forth STRING.
: HL-CODE@  >DFA @ DUP >SA OVER - ; 

\ Store a STRING with hl-code in the dictionary.
: HL-CODE, HERE OVER ALLOT SWAP CMOVE ;

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
    DUP OPTIMISE-B 
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
\D : BA A9 A9 ;
\D
\D : TEST 0 DO BA LOOP ;
\D : Q 0 DO 10000 TEST I . LOOP ;
\D \ -------------- end of sample code --------------------------
\D ( 'BA OPTIMISE-B)
\D  'Q OPTIMISE-B
\D ." Expect  0 1 2 3 4 :" 5 Q CR


\D \ -------------- sample code --------------------------
\D \ This is the code we seek to optimise. ( Nesting_benchmark )
\D : A0A CELL+ ;   : A0B 1- ;
\D : A0 A0A A0B ;   ( 3 + )
\D : A1 A0 A0 ;   : A2 A1 A1 ;    : A3 A2 A2 ;
\D : A4 A3 A3 ;   : A5 A4 A4 ;    : A6 A5 A5 ;
\D : A7 A6 A6 ;   : A8 A7 A7 ;    : A9 A8 A8 ;
\D
\D : B0 A9 A9 ;   ( 3072 + )
\D
\D : TEST 1000 0 DO ['] B0 I DROP EXECUTE LOOP ; ( 3,072,000 + )
'TEST OPTIMISE-B    CRACK TEST
\D : Q 4 >R BEGIN TEST R> 1- DUP WHILE >R REPEAT RDROP ; ( 12,216,000 + )
'Q OPTIMISE-B    CRACK Q
\D \ -------------- end of sample code --------------------------
