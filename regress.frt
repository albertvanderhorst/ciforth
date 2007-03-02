( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

REQUIRE BOUNDS
REQUIRE QSORT
REQUIRE PAIR[]

\ For ADDRESS1 and ADDRESS2 return CONTENT1 and CONTENT2.
: PAIR@   >R @ R> @ ;

\ Regression test
CREATE INT-TABLE
9 , 4 , 3 , 7 , 0 , 8 , 2 , 6 , 1 , 5 ,

: W INT-TABLE CR 10 CELLS BOUNDS  DO I @ . 1 CELLS +LOOP CR ;


\ How to compare two things of ``INT-TABLE'' by the 2 INDICES
: MY-<   INT-TABLE PAIR[]   PAIR@   < ;

\ Swap the contents of two things of ``INT-TABLE'' by the 2 INDICES
: <-->    INT-TABLE PAIR[]   0 CELL+   EXCHANGE ;

\ Expect order in the second print.
W
0 9 'MY-<   '<-->   QSORT
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

: $<  STRING-TABLE PAIR[]   @ EXECUTE ROT @ EXECUTE   2SWAP COMPARE 0 < ;

\ Swap the contents of two things of ``STRING-TABLE'' by the 2 INDICES
: $<-->    STRING-TABLE PAIR[]   0 CELL+   EXCHANGE ;


W2
0 9 '$<   '$<-->   QSORT
W2

CREATE DOUBLE-TABLE
9.0 , ,  4.0 , ,  3.0 , ,  7.0 , ,  0.0 , ,  8.0 , ,  2.0 , ,  6.0 , ,  1.0 , ,  5.0 , ,

: WD CR 10 0 DO DOUBLE-TABLE I 2 * CELLS + 2@ D. LOOP CR ;


: ADDRESSES DOUBLE-TABLE >R  2 * CELLS R@ + SWAP   2 * CELLS R@ + SWAP   RDROP ;
: MY-D<  ADDRESSES   2@ DNEGATE ROT   2@ D+ SWAP DROP 0< ;

\ Swap the contents of two things of ``STRING-TABLE'' by the 2 INDICES
: MY-D<-->    ADDRESSES   2 CELLS   EXCHANGE ;

WD
0 9 'MY-D<   'MY-D<-->   QSORT
WD


" nine  | fout  | three | seven | zero  | eight | two   | six   | one   | five  |"

CREATE x-TABLE , ,

: WT CR x-TABLE 2@ TYPE CR ;

: x-ADDRESSES x-TABLE 2@ DROP >R  8 * R@ + SWAP   8 * R@ + SWAP   RDROP ;

: x<  x-ADDRESSES   8 CORA 0< ;

\ Swap the contents of two things of ``x-TABLE'' by the 2 INDICES
: x<-->  x-ADDRESSES 8 EXCHANGE ;

WT
0 9 'x< 'x<--> QSORT
WT

\ Usage
\ cat  qsort.frt regress.frt | lina -e > test
\ diff test testresults
