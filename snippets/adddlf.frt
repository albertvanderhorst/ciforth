\ Copyright (2003): Albert van der Horst by GNU Public License
\ $Id$
: \D ( POSTPONE \ ) ;  IMMEDIATE


\D : ID... DUP IF ID. ELSE DROP "(null)" TYPE THEN ;

\ Facility for filling in double link fields in Forth

NAMESPACE THINGY THINGY DEFINITIONS
REQUIRE SORT-VOC
'FORTH SORT-VOC
REQUIRE ALIAS
REQUIRE H.
\ '>SFA ALIAS >XFA

\ For DEA, return the NUMBER of dea's, including itself,  after it
\ in the same dictionary,
: dictionary-length  0 SWAP BEGIN SWAP 1+ SWAP >LFA @ DUP 0= UNTIL DROP ;

\ For DEA and N , return DEA n links down.
: >LFAS 0 DO >LFA @ LOOP ;

\ FIXME, not yet For DEA and N , put the end-sentinel in the last xfa (there is only one.)
\ For DEA and N , copy lfa to xfa.
: links-same   1 <> ABORT" only length 1 allowed."
      DUP >LFA @ SWAP >XFA ! ;
\   0 SWAP >XFA ! ;

\ Split DEA COUNT into DEA1 COUNT1 DEA2 COUNT2 approximately half the vocs.
: VOC/2 DUP 2/ DUP >R - 2DUP >LFAS R> ;

\ For DICTPART1 and DICTPART2 make the first dea of dictpart1 point to
\ dictpart2. Return DICTPART2 and DICTPART3 (dictpart1 trimmed of the first dea).
\ Unless dictpart3, then drop it.
: !LINK!   2SWAP >R >R   OVER R@ >XFA !   R> >LFA @   R> 1-
    DUP 0= IF 2DROP THEN ;

\ fill all xfa's of NAMESPACE
: FILL-XFA   0 SWAP   >WID >LFA @   DUP dictionary-length
    BEGIN DUP WHILE DUP 1 = IF links-same ELSE VOC/2 !LINK! THEN REPEAT DROP ;

\ For DEA and N , copy the lfa to the xfa.
: dump 1- 0 ?DO DUP >XFA @ ID...  >LFA @ LOOP >XFA @ ID... CR CR ;

: dump1 .S 1- .S 0 .S ?DO .S DUP .S >LFA .S @ .S ID... .S  .S >LFA .S @ .S LOOP .S >LFA .S @ .S
ID... .S CR .S CR .S ;


\D CREATE AAP CREATE NOOT CREATE MIES  CREATE HUIS
\D CREATE AAP1 CREATE NOOT1 CREATE MIES1  CREATE HUIS1
\D CREATE AAP2 CREATE NOOT2 CREATE MIES2  CREATE HUIS2
\D CREATE AAP3 CREATE NOOT3 CREATE MIES3  CREATE HUIS3
\D 'ENVIRONMENT >WID >LFA @   DUP dictionary-length dump1
\D 'ENVIRONMENT FILL-XFA
\D 'ENVIRONMENT >WID >LFA @   DUP dictionary-length dump
\D
\D 'FORTH >WID >LFA @   DUP dictionary-length dump1
\D 'FORTH FILL-XFA
\D 'FORTH >WID >LFA @   DUP dictionary-length dump


\ For DEA:"it IS a prefix".
: PREFIX?   >FFA @ 8 AND ;

REQUIRE COMPARE
\ Compare STRING and the name of the DEA, return
\ X lower (<0) , same(0), higher (>0).
\ The caller must make sure that the DEA has a valid name,
\ unlike the old (MATCH)
: MATCH2   >R 2DUP R@ >NFA @ $@ COMPARE R> SWAP ;

\ Like ``MATCH2'', also accepts a null dea, and returns null then.
\ And skips silently over dummy entries.
: MATCH3
\ BEGIN DUP WHILE DUP >FFA @ 1 AND WHILE >LFA @ REPEAT  THEN
    DUP IF MATCH2 ELSE 0 THEN ;

\ For NAME DEA, return NAME DEA2
\ dea2 is where the search stops, a match, mismatch or zero.
\ At all line endings the stack contains NAME DEAx
: FIND2-a
    BEGIN
    MATCH3 0 > WHILE  \ While lexographically lower
        DUP >R >XFA @  \ Attempt far link, but keep backtrackpointer
        MATCH3 0< IF   \ Far link is lexicgraphically higher?
\D              DUP .S ID... "FAR LINK REJECTED: NAME TOO LOW " TYPE
                DROP R> >LFA @    \ Backtrack
        ELSE
\D              DUP .S DUP IF ID... " FOLLOWING FAR LINK " TYPE
\D      ELSE
\D              DROP " AT END OF LIST (null) " TYPE THEN
                RDROP          \ Drop backtrack pointer
        THEN
    REPEAT
;
\ Horizontal format
\ : FIND2-a    BEGIN   MATCH3 0 > WHILE   DUP >R   >XFA @ MATCH3
\     0< IF   DROP R> >LFA @   ELSE   RDROP   THEN   REPEAT ;

\ For NAME in DEA /null, return DEA, or null if the name doesn't match.
: FIND2-b   MATCH3 IF DROP 0 THEN   >R 2DROP R> ;
\ Find NAME in WID, return DEA or zero if not found. Assume it is sorted.
: FIND2   FIND2-a FIND2-b ;

\D REQUIRE DO-DEBUG DO-DEBUG
\D 'FORTH FILL-XFA
\D "DROP" 'FORTH >WID >LFA @ FIND2   ID... CR
\D "POPE" 'FORTH >WID >LFA @ FIND2   . CR
\D "!" 'FORTH >WID >LFA @ FIND2      ID... CR
\D "~~~~" 'FORTH >WID >LFA @ FIND2   . CR
