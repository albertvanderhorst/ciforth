
ONLY FORTH DEFINITIONS

: new-h. HEX: . ;
'new-h. 'H. 3 CELLS MOVE

: SHOW-IT
"####################" TYPE CR
"BEFORE" TYPE CR
DUP CRACKED
"AFTER" TYPE CR
DUP OPTIMISE   DUP CRACKED HIDDEN ;
: A-MARKER ;

: test 1 SWAP 3 2 SWAP ;
'test SHOW-IT

: test1 1 2 + 3 4 * OR ;
'test1 SHOW-IT

: test2 2 1 ;
'test2 SHOW-IT

: test3 1 2 'SWAP EXECUTE ;
'test3 SHOW-IT

: A0 1 ;
: A1 A0 A0 + ;   : A2 A1 A1 + ;    : A3 A2 A2 + ;
: A4 A3 A3 + ;   : A5 A4 A4 + ;    : A6 A5 A5 + ;
: A7 A6 A6 + ;   : A8 A7 A7 + ;    : A9 A8 A8 + ;
: B0 A9 A9 + ;
'B0 SHOW-IT
HIDE A0 HIDE A1 HIDE A2 HIDE A3 HIDE A4 HIDE A5
HIDE A6 HIDE A7 HIDE A8 HIDE A9

'SWAP CONSTANT (SWAP)
: ((SWAP)) (SWAP) ;
: WRONG 'EXECUTE ((SWAP)) ;
: RIGHT 'SWAP 'EXECUTE ;
: DOIT 1 2 WRONG RIGHT EXECUTE EXECUTE ;
'DOIT SHOW-IT

: test4 1 2 3 4 BASE ! ;
'test4 SHOW-IT

: test5 SWAP 1 2 3 5 BASE ! SWAP ;
'test5 SHOW-IT

\ Test of annihilating.
: test6 BASE @ IF SWAP THEN 2DROP ;
'test6 SHOW-IT

\ Test of annihilating.
: test6A BASE @ IF SWAP ELSE DROP THEN 2DROP ;
'test6A SHOW-IT

\ Test of match. Cannot be optimised.
: test7  IF 2 + THEN 3 + ;
'test7 SHOW-IT

\ Reorder.
: test8 SWAP IF 3 THEN 5 7 9 BASE ! SWAP ;
'test8 SHOW-IT

\ Forward branch around expansion.
: (test1) + AND OR LSHIFT ;
: test9 IF (test1) THEN ;
'test9 SHOW-IT

\ Forward branch around expansion.
\ This failed because of using sets instead of bags.
: (testA) + AND OR LSHIFT ;
: testA IF (testA) (testA) THEN ;
'testA SHOW-IT

\ Backward branch around expansion.
: (testB) + AND OR LSHIFT ;
: testB BEGIN (testB) AGAIN ;
'testB SHOW-IT

\ Backward branch around expansion.
: (testC) + AND OR LSHIFT ;
: testC BEGIN (testC) (testC) AGAIN ;
'testC SHOW-IT

\ Annihilator involving a fetch.
: testD IF SWAP ELSE DROP BASE @ THEN 2DROP ;
'testD SHOW-IT

\ Annihilator
: testE + DROP ;
'testE SHOW-IT

\ Annihilator
: testF IF 1 ELSE 2 THEN DROP ;
'testF SHOW-IT

\ Backward branch around annihilator.
: testG BEGIN IF 1 ELSE 2 THEN DROP AGAIN ;
'testG SHOW-IT

: (testGA)  'DROP 1 ;
: testGA  (testGA)  SWAP EXECUTE ;
'testGA SHOW-IT

\ Backward branch around multiple annihilators.
: testH
    BEGIN
        IF 1 ELSE 2 THEN DROP
        IF 5 ELSE 6 THEN DROP
    AGAIN ;
'testH SHOW-IT

\ Annihilation
: A9 300 + ;
: testHA 10 = IF 2* THEN DROP  A9 A9     ;
'testHA SHOW-IT

\ \ Expansion with EXITs present.
"testI still fails:" TYPE CR
\ Both exits jump to the same place in the extension.
: (TESTI)  IF AND EXIT THEN ROT ;
: testI    (TESTI) (TESTI) XOR ;
'testI SHOW-IT

\ Expansion with EXITs present.
: (TESTJ)  IF AND EXIT ELSE OR EXIT THEN SWAP ;
: testJ    (TESTJ) XOR ;
'testJ SHOW-IT

\ Expansion with LEAVEs present.
: (TESTK) DO ROT IF LEAVE THEN SWAP LOOP 2DUP ;
: testK    (TESTK) ;
'testK SHOW-IT

\ Expansion with LEAVEs present.
: (TESTK2) DO ROT IF LEAVE THEN SWAP DO LOOP LOOP 2DUP ;
: testK2    (TESTK2) ;
'testK2 SHOW-IT

\ Interfering LEAVEs and EXITs.
: (TESTL) DO
    DUP IF LEAVE ELSE UNLOOP EXIT THEN SWAP
    2DUP IF LEAVE ELSE UNLOOP EXIT THEN 2SWAP
LOOP ROT ;
: testL    (TESTL) 2OVER ;
SEE (TESTL)
'testL SHOW-IT

\ Patterns, combined with inlining.    FIXME! crashes
" TESTM crashes, omitted" TYPE CR
0 CONSTANT z
1 CONSTANT o
: A0-A CELL+ ;   : A0-B 1- ;
: A1 A0-A A0-B ;   : A2 A1 z + A1  ;    : A3 A2 o * A2  ;
: A4 A3 A3     ;   : A5 A4 z + A4  ;    : A6 A5 o * A5  ;
: A7 A6 A6     ;   : A8 A7 z + A7  ;    : A9 A8 o * A8  ;

: B0-A A9   BASE @ DUP ;    : B0-B   10 = IF 2* THEN DROP  A9 A9     ;

: B1 B0-A B0-B ;   : B2 B1 z + B1 ;    : B3 B2 o * B2 ;
: B4 B3 B3 ;       : B5 B4 z + B4 ;    : B6 B5 o * B5 ;
: B7 B6 B6 ;       : B8 B7 z + B7 ;    : B9 B8 o * B8 ;
: C0 B9 B9 ;
: testM C0 ;
SEE testM
\ 'testM SHOW-IT

HIDE A1 HIDE A2 HIDE A3
HIDE A4 HIDE A5 HIDE A6 HIDE A7
HIDE A8 HIDE A9

: testN BEGIN ROT WHILE IF DROP THEN REPEAT 2OVER ;
'testN SHOW-IT

\ These empty branches are a special cases in the peephole optimiser.
: testO1 IF THEN ;
'testO1 SHOW-IT
: testO1A IF 1 ELSE THEN 2 ;
'testO1A SHOW-IT

\ These test don't make a lot of sense yet.
: testO2 IF ROT ELSE THEN ;
'testO2 SHOW-IT
: testO3 0 IF ROT THEN ;
'testO3 SHOW-IT
: testO4 -1 IF ROT THEN ;
'testO4 SHOW-IT
: testO5 1 IF ROT THEN ;
'testO5 SHOW-IT

\ ---------------------------------------------------------------------------

\ The following compiled directly to the optimised code as per above.
\ This has not been maintained. So now it serves as a check for
\ what cannot be compiled.

: test 1 SWAP 2 3 ;
'test SHOW-IT

: test1 $0F ;
'test1 SHOW-IT

: test2 2 1 ;
'test2 SHOW-IT

: test3 2 1 ;
'test3 SHOW-IT

: A0 1 ;
: A1 A0 A0 + ;   : A2 A1 A1 + ;    : A3 A2 A2 + ;
: A4 A3 A3 + ;   : A5 A4 A4 + ;    : A6 A5 A5 + ;
: A7 A6 A6 + ;   : A8 A7 A7 + ;    : A9 A8 A8 + ;
: B0 [ A9 A9 + ] LITERAL ;

'B0 SHOW-IT

: DOIT 2 1 ;

'DOIT SHOW-IT

: test4 4 BASE ! 1 2 3 ;
'test4 SHOW-IT

: test5 SWAP 5 BASE ! 1 3 2 ;
'test5 SHOW-IT

\ Test of annihilating.
: test6 BASE @ IF SWAP THEN 2DROP ;
'test6 SHOW-IT

\ Test of match. Cannot be optimised.
: test7  IF 2 + THEN 3 + ;
'test7 SHOW-IT

\ Reorder.
: test8 SWAP IF 3 THEN 9 BASE ! 7 5 ;
'test8 SHOW-IT

\ Expansion.
: test9 IF + AND OR LSHIFT THEN ;
'test9 SHOW-IT

: testA IF + AND OR LSHIFT + AND OR LSHIFT THEN ;
'testA SHOW-IT

: testB BEGIN + AND OR LSHIFT AGAIN ;
'testB SHOW-IT

: testC BEGIN + AND OR LSHIFT + AND OR LSHIFT AGAIN ;
'testC SHOW-IT

: testD DROP DROP DROP ;
'testD SHOW-IT

: testE DROP DROP ;
'testE SHOW-IT

: testF DROP ;
'testF SHOW-IT

: testG BEGIN DROP AGAIN ;
'testG SHOW-IT

: testH BEGIN DROP DROP AGAIN ;
'testH SHOW-IT

: testI
IF AND BRANCH [ (FORWARD >R ] THEN ROT [ R> FORWARD) ]
IF AND BRANCH [ (FORWARD >R ] THEN ROT [ R> FORWARD) ]
;

'testI SHOW-IT

: testJ
IF AND BRANCH [ (FORWARD >R ]
ELSE OR BRANCH [ (FORWARD >R ]
THEN SWAP
[ R> FORWARD) R> FORWARD) ]
;

'testJ SHOW-IT

: testK
DO ROT IF RDROP  RDROP  RDROP  BRANCH [ (FORWARD >R ] THEN SWAP LOOP [ R> FORWARD) ] 2DUP
;

'testK SHOW-IT

: testK2
DO ROT IF RDROP  RDROP  RDROP  BRANCH [ (FORWARD >R ] THEN SWAP DO LOOP LOOP [ R> FORWARD) ] 2DUP
;
'testK2 SHOW-IT

: testL DO
DUP IF RDROP  RDROP  RDROP  BRANCH [ (FORWARD >R ] ELSE BRANCH [ (FORWARD >R ] THEN SWAP
2DUP IF RDROP  RDROP  RDROP  BRANCH [ (FORWARD >R ] ELSE BRANCH [ (FORWARD >R ] THEN 2SWAP
LOOP
[ R> R> FORWARD) R> R> FORWARD) >R >R ]
ROT
[ R> FORWARD) R> FORWARD) ]
2OVER
;

'testL SHOW-IT

: testM $120000 + ;
'testM SHOW-IT

: testN [ (BACK >R ] BEGIN ROT WHILE 0BRANCH [ R> BACK) ] DROP REPEAT 2OVER ;
'testN SHOW-IT

\ These empty branches are taking care off by the annihilator.
: testO1 IF THEN ;
'testO1 SHOW-IT
: testO2 IF ROT THEN ;
'testO2 SHOW-IT
: testO3 ;
'testO3 SHOW-IT
: testO4 ROT ;
'testO4 SHOW-IT
: testO5 ROT ;
'testO5 SHOW-IT
CR
