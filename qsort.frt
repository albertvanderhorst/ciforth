( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)


: \D ;

1 LOAD
REQUIRE COMPARE

: NOT    0= ;

: DEFER CREATE 0 , DOES> @ EXECUTE ;
: IS   (WORD) FOUND >BODY ! ;

\D "Expect aap : " TYPE : aap "aap" TYPE ; DEFER iets 'aap IS iets iets


\  Set PRECEDES for different datatypes or sort order.
DEFER PRECEDES  ' < IS PRECEDES

\  For sorting character strings in increasing order:
: SPRECEDES         ( addr addr -- flag )
    >R COUNT R> COUNT COMPARE 0< ;
\   ' SPRECEDES IS PRECEDES

: EXCHANGE          ( addr_1 addr_2 -- )
    DUP @ >R  OVER @ SWAP !  R> SWAP ! ;

\ For ADDRESS return a next lower ADDRESS that is aligned.
\ This may work only on two complement machines.
: ALIGN-DOWN   -1 CELLS AND ;

: CELL-  ( addr -- addr' )  1 CELLS - ;

: PARTITION         ( lo hi -- lo_1 hi_1 lo_2 hi_2 )
    2DUP OVER - 2/  ALIGN-DOWN +  @ >R  ( R: median) 
    2DUP BEGIN      ( lo_1 hi_2 lo_2 hi_1)
         SWAP BEGIN  DUP @ R@  PRECEDES WHILE  CELL+  REPEAT
         SWAP BEGIN  R@ OVER @  PRECEDES WHILE  CELL-  REPEAT
         2DUP > NOT IF  2DUP EXCHANGE  >R CELL+ R> CELL-  THEN
    2DUP > UNTIL    ( lo_1 hi_2 lo_2 hi_1)
    R> DROP                            ( R: )
    SWAP ROT        ( lo_1 hi_1 lo_2 hi_2)
    ;

: QSORT             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2OVER 2OVER  - +         ( . . . . lo_1 hi_1+lo_2-hi_2)
        < IF  2SWAP  THEN    ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;

: SORT              ( addr n -- )
    DUP 2 < IF  2DROP  EXIT THEN
    1- CELLS OVER + ( addr addr+{n-1}cells) QSORT ( ) ;
