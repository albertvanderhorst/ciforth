( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)


: \D ;

1 LOAD
REQUIRE COMPARE

: NOT    0= ;

: DEFER CREATE 0 , DOES> @ EXECUTE ;
: IS   (WORD) FOUND >BODY ! ;

\ Exchange the content at ADDRESS1 and ADDRESS2 over a fixed LENGTH.
: EXCHANGE 0 ?DO   OVER I +     OVER I +  OVER C@   OVER C@
                   >R SWAP C!  R> SWAP C! LOOP 2DROP ;

\D "Expect aap : " TYPE : aap "aap" TYPE ; DEFER iets 'aap IS iets iets


\  Set PRECEDES for different datatypes or sort order.
DEFER PRECEDES  ' < IS PRECEDES

\  For sorting character strings in increasing order:
: SPRECEDES         ( addr addr -- flag )
    >R COUNT R> COUNT COMPARE 0< ;
\   ' SPRECEDES IS PRECEDES


\ Swap the contenst of cells at ADDRESS1 and ADDRESS2.
: <-->    0 CELL+ EXCHANGE ;

DEFER (<-->)    ' <--> IS (<-->)

\ For ADDRESS return a next lower ADDRESS that is aligned.
\ This may work only on two complement machines.
: ALIGN-DOWN   -1 CELLS AND ;

: CELL-  ( addr -- addr' )  1 CELLS - ;

: PARTITION         ( lo hi -- lo_1 hi_1 lo_2 hi_2 )
    2DUP OVER - 2/  ALIGN-DOWN +  @ >R  ( R: median)
    2DUP BEGIN      ( lo_1 hi_2 lo_2 hi_1)
         SWAP BEGIN  DUP @ R@  PRECEDES WHILE  CELL+  REPEAT
         SWAP BEGIN  R@ OVER @  PRECEDES WHILE  CELL-  REPEAT
         2DUP > NOT IF  2DUP (<-->)  >R CELL+ R> CELL-  THEN
    2DUP > UNTIL    ( lo_1 hi_2 lo_2 hi_1)
    R> DROP                            ( R: )
    SWAP ROT        ( lo_1 hi_1 lo_2 hi_2)
    ;

: QSORT             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;

: SORT              ( addr n -- )
    DUP 2 < IF  2DROP  EXIT THEN
    1- CELLS OVER + ( addr addr+{n-1}cells) QSORT ( ) ;

\ AUXILIARY
