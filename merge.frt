REQUIRE COMPARE

VOCABULARY AAP AAP DEFINITIONS
: K ;
: C ;
: B ;

VOCABULARY NOOT NOOT DEFINITIONS
: N ;
: M ;
: A ;

VOCABULARY MIES MIES DEFINITIONS
: SC ;
: QL ;
: RB ;
: PK ;

: >N   >LFA @ ;
: .CHAIN
BEGIN DUP ID. >N DUP 0= UNTIL DROP ;

: GET-NAME >NFA @ $@   ;

: LINK! >LFA ! ;

\ For LINK1 and LINK2, return LINK1 and LINK2 plus "link1 IS lower".
: *< OVER >R DUP GET-NAME    R> GET-NAME    COMPARE 0 > ;
: ?? OVER ID. DUP ID. CR ;
: OLD-MERGE
   DUP 0= IF DROP ELSE
     ??
     *< 0= IF SWAP THEN OVER >R RECURSE R@ ! R> THEN ;
\ For LINK1 ( > ) LINK2 return LINK1 LINK2' advanced but still link1 < link2'
: FIND-END BEGIN DUP >R >N DUP IF
*< 0= ELSE 0 THEN WHILE RDROP REPEAT DROP R> ;

\ Merge LINK1 ( > ) LINK2.
: (MERGE)
    BEGIN FIND-END DUP >R  DUP >N >R
        LINK! R> R> OVER 0= UNTIL 2DROP ;

\ Merge LINK1 and LINK2, leave merged LINK.
: MERGE   *< IF SWAP THEN   DUP >R (MERGE) R> ;

\ Cut LINK in two return remaining ULINK and SLINK (first part in ascending order)
: SNIP DUP >R
      BEGIN DUP >N  DUP IF ?? *< ELSE 0 THEN WHILE SWAP DROP REPEAT
      >R   0 SWAP LINK!
      R> R> ;
