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

\ For LINK1 and LINK2, return LINK1 and LINK2 plus "link1 IS lower".
    : GET-NAME >NFA @ $@   ;  \ Aux. For EL, return NAME.
: NAMES< OVER >R DUP GET-NAME    R> GET-NAME    COMPARE 0 > ;

\ For LINK1 and LINK2, return LINK1 and LINK2 plus "link1 IS lower".
VARIABLE *<     'NAMES< *< !
: LL< *< @ EXECUTE ;

\ For ELEMENT return POINTER to next element of list.
VARIABLE *>N    '>LFA *>N !

\ For ELEMENT return next ELEMENT of list.
: >N   *>N @ EXECUTE @ ;
\ For LIST and ELEMENT, hang the list off the element.
: LINK! *>N @ EXECUTE ! ;

\ ------------- DEBUG and OLD ----------------
: .CHAIN   BEGIN DUP ID. >N DUP 0= UNTIL DROP ;
: ?? OVER ID. DUP ID. CR ;
: OLD-MERGE
   DUP 0= IF DROP ELSE
     ??
     LL< 0= IF SWAP THEN OVER >R RECURSE R@ ! R> THEN ;

\ --------------------------------------------

\ For LINK1 ( > ) LINK2 return LINK1 LINK2' advanced but still link1 > link2'
: FIND-END BEGIN DUP >R >N DUP IF LL< 0= ELSE 0 THEN WHILE RDROP REPEAT DROP R> ;

\ Merge LINK1 ( > ) LINK2.
: (MERGE)
    BEGIN FIND-END DUP >R  DUP >N >R
        LINK! R> R> OVER 0= UNTIL 2DROP ;

\ Merge LINK1 and LINK2, leave merged LINK.
: MERGE   LL< IF SWAP THEN   DUP >R (MERGE) R> ;

\ Cut ULINK in two. Return SLINK (first part in ascending order)
\ and remaining ULINK.
: SNIP DUP
      BEGIN DUP >N  DUP IF LL< ELSE 0 THEN WHILE SWAP DROP REPEAT
      >R   0 SWAP LINK! R> ;

\ Keep on merging as long as the top of the stack contains
\ two slinkp 's of the same level. Shrinking the stack.
\ One loop goes from LINK1 LEVEl1 and LINK2P LEVEL1 to LINK3 LEVEL (level1+1). .
: TRY-MERGES  BEGIN >R  OVER R@ = WHILE SWAP DROP MERGE R> 1+ REPEAT R> ;

\ Keep on merging as long as the top of the stack contains
\ two slinkp 's , i.e no end-sentinel. Shrinking the stack to one slink.
: SHRINK DROP BEGIN OVER WHILE SWAP DROP MERGE REPEAT ;

\ Expand zero ulink into zero slinkp .... slinkp
: EXPAND   BEGIN SNIP >R 1 TRY-MERGES R> DUP WHILE REPEAT DROP ;

\ For linked LIST , leave a sorted LIST1.
\ The zero is merely an end-sentinel.
: MERGE-SORT 0 SWAP EXPAND SHRINK SWAP DROP ;

\ Sort the WORDLIST
: SORT-WID CELL+ DUP >N MERGE-SORT SWAP LINK! ;

\ Sort the vocabulary given by its XT.
: SORT-VOC >BODY SORT-WID ;
