( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)


\ Regression test
CREATE INT-TABLE
9 , 4 , 3 , 7 , 0 , 8 , 2 , 6 , 1 , 5 ,

: W INT-TABLE CR 10 CELLS BOUNDS  DO I @ . 1 CELLS +LOOP CR ;

\ Expect nice order.

W
\ How to compare two things by the ADDRESSES
: MY-<   >R @ R> @ < ;

\ Swap the contents of cells at ADDRESS1 and ADDRESS2.
: <-->    0 CELL+ EXCHANGE ;

INT-TABLE   DUP 10 1- CELLS +   'MY-<   '<-->   SORT

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


: W2 STRING-TABLE CR 10 CELLS BOUNDS  DO I @ EXECUTE TYPE 1 CELLS +LOOP CR ;

W2
\  : $<  EXECUTE ROT EXECUTE 2SWAP 2DUP TYPE 2OVER TYPE COMPARE .S 0 < ;
: $<  @ EXECUTE ROT @ EXECUTE 2SWAP COMPARE 0 < ;



STRING-TABLE   DUP 10 1- CELLS +   '$<   '<-->   SORT

W2
