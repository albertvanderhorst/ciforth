( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

: \D ;

1 LOAD
REQUIRE COMPARE

\ ----------------- if this is not present ------------------
: NOT    0= ;

: DEFER-ERROR    -1 13 ?ERROR ;

: DEFER CREATE ' DEFER-ERROR , DOES> @ EXECUTE ;

\ Do not use this.
: IS   (WORD) FOUND >BODY ! ;

\D "Expect aap : " TYPE : aap "aap" TYPE ; DEFER iets 'aap IS iets iets


\ ----------------- auxiliary -------------------------------

\ Exchange the content at ADDRESS1 and ADDRESS2 over a fixed LENGTH.
: EXCHANGE 0 ?DO   OVER I +     OVER I +  OVER C@   OVER C@
                   >R SWAP C!  R> SWAP C! LOOP 2DROP ;

\   For INDEX1 and INDEX2 and TABLE, return corresponding ADDRESS1
\   and ADDRESS2 .
: PAIR[] >R   CELLS R@ + SWAP   CELLS R@ + SWAP   RDROP ;

\ ----------------- quick sort proper -----------------------

\ Compare item N1 and N2. Return ``N1'' IS lower and not equal.
DEFER *<
\ Exchange item N1 and N2. 
DEFER *<-->

\ Sort the range LOW to HIGH inclusive observing *< and *<-->
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

\ Sort the range LOW to HIGH inclusive observing 
\ ``LOW'' and ``HIGH'' must be indices compatible with the current
\ values of *< and *<-->
: QSORT             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;

\ Sort the range FIRST to LAST (inclusive) of item compared by the xt
\ COMPARING and exchanged by the xt EXHANGING.
\ All indices in this range must be proper to pass to both of the xt's.
\ The xt's are filled in into *< and *<--> and must observe the 
\ interface.
\ After the call we have that : 
\ ``For FIRST<=I<J<=LAST      I J *<--> EXECUTE leaves TRUE.''
: SORT   '*<--> >BODY !   '*< >BODY !   QSORT ;
