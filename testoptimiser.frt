: LINE  20 0 DO &# EMIT LOOP CR ;
: SHOW-IT LINE
"BEFORE" TYPE DUP CRACKED DUP OPTIMISE-O "AFTER" TYPE CRACKED ;


: test 1 SWAP 3 2 SWAP ;
'test SHOW-IT

: test1 1 2 + 3 4 * OR ;
'test1 SHOW-IT
: test2 1 2 SWAP ;
'test2 SHOW-IT
: test3 1 2 'SWAP EXECUTE ;
'test3 SHOW-IT

LINE
" \ BEFORE :
: A0 1 ;
: A1 A0 A0 + ;   : A2 A1 A1 + ;    : A3 A2 A2 + ;
: A4 A3 A3 + ;   : A5 A4 A4 + ;    : A6 A5 A5 + ;
: A7 A6 A6 + ;   : A8 A7 A7 + ;    : A9 A8 A8 + ;
: B0 A9 A9 + ;
" 2DUP TYPE EVALUATE

'B0 OPTIMISE-O
"AFTER: 'B0 OPTIMISE-O" TYPE
CRACK A0   CRACK A1  CRACK A5 CRACK B0

LINE
" \ BEFORE :
'SWAP CONSTANT (SWAP)
: ((SWAP)) (SWAP) ;
: WRONG 'EXECUTE ((SWAP)) ;
: RIGHT 'SWAP 'EXECUTE ;
: DOIT 1 2 WRONG RIGHT EXECUTE EXECUTE ;
" 2DUP TYPE EVALUATE

'DOIT OPTIMISE-O
"AFTER: 'DOIT OPTIMISE-O"  TYPE
CRACK DOIT

: test4 1 2 3 4 BASE ! ;
'test4 SHOW-IT

: test5 SWAP 1 2 3 5 BASE ! SWAP ;
'test5 SHOW-IT

\ This example must not be handled by folding, but by annihilating.
: test6 BASE @ IF SWAP THEN 2DROP ;
'test6 SHOW-IT

\ This example must not be handled by a match.
: test7  IF 2 + THEN 3 + ;
'test7 SHOW-IT

: test8 SWAP IF 3 THEN 5 7 9 BASE ! SWAP ;
'test8 SHOW-IT

: (test1) + AND OR LSHIFT ;
: test9 IF (test1) THEN ;
'test9 SHOW-IT

: (test1) + AND OR LSHIFT ;
: testA IF (test1) (test1) THEN ;
'testA SHOW-IT

: (test1) + AND OR LSHIFT ;
: testB BEGIN (test1) AGAIN ;
'testB SHOW-IT

: (test1) + AND OR LSHIFT ;
: testC BEGIN (test1) (test1) AGAIN ;
'testC SHOW-IT

: testD IF SWAP ELSE DROP BASE @ THEN 2DROP ;
'testD SHOW-IT

: testE + DROP ;
'testE SHOW-IT

: testF IF 1 ELSE 2 THEN DROP ;
'testF SHOW-IT

: testG BEGIN IF 1 ELSE 2 THEN DROP AGAIN ;
'testG SHOW-IT

: testH BEGIN
IF 1 ELSE 2 THEN DROP
IF 5 ELSE 6 THEN DROP
AGAIN ;
'testH SHOW-IT
