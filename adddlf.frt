\ Copyright (2003): Albert van der Horst by GNU Public License
\ $Id$

: \D ( POSTPONE \ ) ;  IMMEDIATE

\ Facility for filling in double link fields in Forth

VOCABULARY THINGY THINGY DEFINITIONS
REQUIRE SORT-VOC
'FORTH SORT-VOC
REQUIRE ALIAS
REQUIRE H.
'>SFA ALIAS >XFA

\ For DEA, return the NUMBER of dea's, including itself,  after it
\ in the same dictionary,
: dictionary-length  0 SWAP BEGIN SWAP 1+ SWAP >LFA @ DUP 0= UNTIL DROP ;

\ For DEA and N , return DEA n links down.
: >LFAS 0 DO >LFA @ LOOP ;

\ For DEA and N , copy the lfa to the xfa.
: links-same   0 DO DUP >LFA @ OVER >XFA ! >LFA @ LOOP DROP ;

\ Split DEA COUNT into DEA1 COUNT1 DEA2 COUNT2 approximately half the vocs.
: VOC/2 DUP 2/ DUP >R - 2DUP >LFAS R> ;

\ For DICTPART1 and DICTPART2 make the first dea of dictpart1 point to
\ dictpart2. Return DICTPART2 and DICTPART3 (dictpart1 trimmed of the first dea).
: !LINK!   2SWAP >R >R   OVER R@ >XFA !   R> >LFA @   R> 1- ;

\ fill all xfa's of VOCABULARY
: FILL-XFA   0 SWAP   >WID >LFA @   DUP dictionary-length
    BEGIN DUP WHILE DUP 3 < IF links-same ELSE VOC/2 !LINK! THEN REPEAT DROP ;

\ For DEA and N , copy the lfa to the xfa.
: dump 1- 0 DO DUP >XFA @ ID.  >LFA @ LOOP >LFA @ H. CR CR ;

: dump1 1- 0 DO DUP >LFA @ ID.  >LFA @ LOOP >LFA @ H. CR CR ;


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


\ Like ``MATCH2'', only takes a null dea, and returns null then.
: MATCH3 DUP IF MATCH2 ELSE 0 THEN ;

'2DROP '?PAIRS 3 CELLS MOVE


\ For NAME DEA, return NAME DEA2
\ dea2 is where the search stops, a match, mismatch or zero.
: FIND2-a
    BEGIN
    MATCH3 0 > WHILE
        DUP >R >XFA @
        MATCH3 0< IF
\D              DUP .S ID. "REJECTED " TYPE
                DROP R> >LFA @
        ELSE
\D              DUP .S DUP IF ID. "OKAY " TYPE ELSE DROP "ATEND" TYPE THEN
                RDROP
        THEN
    REPEAT
;
\ Find NAME in WID, return DEA or zero. Assume it is sorted.
: FIND2-b   MATCH3 IF DROP 0 THEN   >R 2DROP R> ;
\ Find NAME in WID, return DEA or zero. Assume it is sorted.
: FIND2   FIND2-a FIND2-b ;

DO-DEBUG
\D 'FORTH FILL-XFA
\D "DROP" 'FORTH >WID >LFA @ FIND2   ID. CR
\D "POPE" 'FORTH >WID >LFA @ FIND2   . CR
\D "!" 'FORTH >WID >LFA @ FIND2      ID. CR
\D "~~~~" 'FORTH >WID >LFA @ FIND2   . CR
