

REQUIRE [IF]
REQUIRE ELAPSED

\ Merging linked lists.
\ Nomenclature :
\   ULIST   The head of a linked list that is unsorted.
\   LIST    The head of a linked list that ends in a null pointer (0)
\           It is sorted with respect to *< .
\   LISTP  The head of a linked list that is sorted, plus the level.
\   ELEM    An element of a linked list, at the same time the head of
\            the remainder of the list.

REQUIRE COMPARE

\ Contains an execution token with the effect:
\ For ELEM1 and ELEM2, return ELEM1 and ELEM2 plus "elem1 IS lower".
VARIABLE *<
: LL< *< @ EXECUTE ;

\ Contains an execution token with the effect:
\ For ELEM return POINTER to next element of list.
VARIABLE *>N

\ For ELEM return next ELEM of list.
: >N   *>N @ EXECUTE @ ;
\ For LIST and ELEM , hang the list off the element.
: LINK! *>N @ EXECUTE ! ;

\ For LIST1 ( > ) LIST2 return LIST1 LIST2' advanced but still list1 > list2'
\ (Point where list1 is to be inserted.)
: FIND-WHERE
    BEGIN DUP >R >N DUP IF LL< 0= ELSE 0 THEN WHILE RDROP REPEAT DROP R> ;

\ Merge LIST1 ( > ) LIST2.
: (MERGE)
    BEGIN FIND-WHERE DUP >R  DUP >N >R
        LINK! R> R> OVER 0= UNTIL 2DROP ;

\ For the sorted list ONE and the sorted list TWO , where the
\ starting element of one is lexicographically later than
\ the starting element of two, return list ONE and a linked LIST
\ that is two with its first part removed.
\ The starting element is still lexicographically later,
\ but maximally so, and the next element no longer is.
\ VERBOSE This is the point where list TWO is to be merged in.
\ VERBOSE You get such a list by merging or selection, in the first
\ VERBOSE place.
: MERGE   LL< IF SWAP THEN   DUP >R (MERGE) R> ;

[DEFINED] NOBEL 0= [IF]  \ Start with natural runs.


\ Cut ULIST in two. Return LIST (first part in ascending order)
\ and remaining ULIST.
: SNIP DUP
      BEGIN DUP >N  DUP IF LL< ELSE 0 THEN WHILE SWAP DROP REPEAT
      >R   0 SWAP LINK! R> ;

\ Keep on merging as long as the top of the stack contains
\ two lisp 's of the same level. Shrinking the stack.
\ One loop goes from LIST1 LEVEL1 and LIST2 LEVEL1 to LIST3 LEVEL (level1+1). .
: TRY-MERGES  BEGIN >R  OVER R@ = WHILE SWAP DROP MERGE R> 1+ REPEAT R> ;

\ Keep on merging as long as the top of the stack contains
\ two lisp 's , i.e no end-sentinel. Shrinking the stack to one slist.
: SHRINK DROP BEGIN OVER WHILE SWAP DROP MERGE REPEAT ;

\ Expand zero, ulist into zero lisp .... lisp
: EXPAND   BEGIN SNIP >R 1 TRY-MERGES R> DUP WHILE REPEAT DROP ;

\ For compare XT, next XT, linked LIST , leave a sorted LIST1.
\ The zero is merely an end-sentinel.
: MERGE-SORT   *>N !  *< !   0 SWAP EXPAND SHRINK SWAP DROP ;

[ELSE] ( Nobel )        \ Start with one element runs.

( l - l/2 l/2')
: SPLIT/   DUP DUP >R
    BEGIN >N DUP IF >N THEN DUP WHILE  R> >N >R REPEAT DROP
    R@ >N   0 R> LINK! ;

: SPLIT/ DUP DUP
BEGIN >N DUP IF >N THEN DUP WHILE
      SWAP >N SWAP REPEAT DROP
      DUP >N SWAP    0 SWAP LINK! ;

( l -- l')
: (MERGE-SORT) DUP >N IF SPLIT/ RECURSE SWAP RECURSE MERGE THEN ;

( xt-compare xt-next list -- sorted-list )
: MERGE-SORT   *>N !  *< !   (MERGE-SORT) ;

[THEN]

\ ----------------- Example : wordlists ------------------

\ Note that xt's form a linked list
\ For XT1 and XT2 return XT1 and XT2 plus "xt IS lower".
    : GET-NAME >NFA @ $@   ;  \ Aux. For EL, return NAME.
: NAMES< DUP >R OVER GET-NAME    R> GET-NAME    COMPARE 0 < ;

\ Sort the WORDLIST. This head of the list doesn't take part in the
\ sorting (expect for the link field) , so it may be a dummy.
: SORT-WID >LFA DUP >R   @ 'NAMES< '>LFA MERGE-SORT   R> ! ;

\ Sort the vocabulary given its vocabulary XT.
: SORT-VOC >WID SORT-WID ;


: MEASURE MARK-TIME 'FORTH SORT-VOC ELAPSED .mS ;

\ ------------- DEBUG and OLD ----------------
\ VOCABULARY AAP AAP DEFINITIONS
\ VOCABULARY AAP AAP DEFINITIONS
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

'>LFA *>N !
'NAMES< *< !

: .CHAIN   BEGIN DUP ID. >N DUP 0= UNTIL DROP ;
: ?? OVER ID. DUP ID. CR ;
: OLD-MERGE
   DUP 0= IF DROP ELSE
     ??
     LL< 0= IF SWAP THEN OVER >R RECURSE R@ ! R> THEN ;

\ --------------------------------------------
