REQUIRE COMPARE

VOCABULARY AAP AAP DEFINITIONS
: K ;
: C ;
: B ;

\ VOCABULARY NOOT NOOT DEFINITIONS
: N ;
: M ;
: A ;

\ VOCABULARY MIES MIES DEFINITIONS
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

\ For UNLINK leave SLINKP SLINKP UNLINK  or SLINKP 0 .
: SNIP2   SNIP SWAP 1 SWAP DUP IF SNIP SWAP 1 SWAP THEN ;

\ Keep on merging as long as the top of the stack contains
\ two SLINKP 's of the same level. Shrinking the stack.
: TRY-MERGES  BEGIN >R  OVER R@ = WHILE SWAP DROP MERGE R> 1+ REPEAT R> ;

\ Try to merge LINK1 LEVEl1 and LINK2P LEVEL2 .
\ This is done if the level2 is lower.
\ If so LINK LEVEL TRUE plus one else leave IT FALSE

\ Keep on merging as long as the top of the stack contains
\ two SLINKP 's , to be seen from a non-zero level. Shrinking the stack.
: SHRINK DROP BEGIN OVER WHILE SWAP DROP MERGE REPEAT ;

\ Expand UNLINK into ZERO SLINKP .... SLINKP
: EXPAND    0 SWAP BEGIN SNIP2
    >R TRY-MERGES R> DUP WHILE REPEAT DROP ;

\ For linked LIST , leave a sorted LIST1.
: MERGE-SORT EXPAND SHRINK SWAP DROP ;

\ Sort the WORDLIST
: SORT-WID CELL+ DUP >N MERGE-SORT SWAP LINK! ;

\ Sort the vocabulary given by its XT.
: SORT-VOC >BODY SORT-WID ;
