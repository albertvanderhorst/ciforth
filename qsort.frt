( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)


: \D ;

1 LOAD
REQUIRE COMPARE

: NOT    0= ;

: DEFER-ERROR    -1 13 ?ERROR ;

: DEFER CREATE ' DEFER-ERROR , DOES> @ EXECUTE ;

\ Do not use this.
: IS   (WORD) FOUND >BODY ! ;

\ Exchange the content at ADDRESS1 and ADDRESS2 over a fixed LENGTH.
: EXCHANGE 0 ?DO   OVER I +     OVER I +  OVER C@   OVER C@
                   >R SWAP C!  R> SWAP C! LOOP 2DROP ;

\D "Expect aap : " TYPE : aap "aap" TYPE ; DEFER iets 'aap IS iets iets

DEFER *<

\  For sorting character strings in increasing order:
: SPRECEDES         ( addr addr -- flag )
    >R COUNT R> COUNT COMPARE 0< ;
\   ' SPRECEDES IS *<


DEFER *<-->

\ For ADDRESS return a next lower ADDRESS that is aligned.
\ This may work only on two complement machines.
: ALIGN-DOWN   -1 CELLS AND ;

: CELL-  ( addr -- addr' )  1 CELLS - ;

: PARTITION         ( lo hi -- lo_1 hi_1 lo_2 hi_2 )
    2DUP + 2/   >R  ( R: median)
    2DUP BEGIN      ( lo_1 hi_2 lo_2 hi_1)
         SWAP BEGIN  DUP R@ *< WHILE  1+  REPEAT
         SWAP BEGIN  R@ OVER *< WHILE  1-  REPEAT
         2DUP > NOT IF
            \ Do we have a new position for our pivot?
            OVER R@ = IF RDROP DUP >R ELSE
            DUP  R@ = IF RDROP OVER >R THEN THEN
            2DUP *<-->
            >R 1+ R> 1-
        THEN
    2DUP > UNTIL    ( lo_1 hi_2 lo_2 hi_1)
    RDROP                            ( R: )
    SWAP ROT        ( lo_1 hi_1 lo_2 hi_2)
    ;

: QSORT             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;

\ lo hi xt-c xt-e
: SORT   '*<--> >BODY !   '*< >BODY !   QSORT ;

\ AUXILIARY
