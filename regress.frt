( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\   For INDEX1 and INDEX2 and TABLE, return corresponding ADDRESS1
\   and ADDRESS2 .
: PAIR[] >R   CELLS R@ + SWAP   CELLS R@ + SWAP   RDROP ;

\ For ADDRESS1 and ADDRESS2 return CONTENT1 and CONTENT2.
: PAIR@   >R @ R> @ ;

\ Regression test
CREATE INT-TABLE
9 , 4 , 3 , 7 , 0 , 8 , 2 , 6 , 1 , 5 ,

: W INT-TABLE CR 10 CELLS BOUNDS  DO I @ . 1 CELLS +LOOP CR ;


W
\ How to compare two things of ``INT-TABLE'' by the 2 INDICES
: MY-<   INT-TABLE PAIR[]   PAIR@   < ;

\ Swap the contents of two things of ``INT-TABLE'' by the 2 INDICES
: <-->    INT-TABLE PAIR[]   0 CELL+   EXCHANGE ;

0 9 'MY-<   '<-->   SORT

\ Expect nice order.
W

: A0 S" nine" ;
: A1 S" four" ;
: A2 S" three" ;
: A3 S" seven" ;
: A4 S" zero" ;
: A5 S" eight" ;
: A6 S" two" ;
: A7 S" six" ;
: A8 S" one" ;
: A9 S" five" ;

CREATE STRING-TABLE
' A0 ,   ' A1 ,   ' A2 ,   ' A3 ,   ' A4 ,
' A5 ,   ' A6 ,   ' A7 ,   ' A8 ,   ' A9 ,


: W2 STRING-TABLE CR 10 CELLS BOUNDS  DO I @ EXECUTE TYPE SPACE 1 CELLS +LOOP CR ;

W2
: $<  STRING-TABLE PAIR[]   @ EXECUTE ROT @ EXECUTE   2SWAP COMPARE 0 < ;

\ Swap the contents of two things of ``STRING-TABLE'' by the 2 INDICES
: $<-->    STRING-TABLE PAIR[]   0 CELL+   EXCHANGE ;


0 9 '$<   '$<-->   SORT

W2
