  ciforth.lab  $Revision$ (c) Albert van der Horst
 : EMPTY STACK
 : DICTIONARY FULL
 : FIRST ARGUMENT MUST BE OPTION
 : ISN'T UNIQUE
 : EMPTY NAME FOR NEW DEFINITION
 : DISK RANGE ?
 : FULL STACK
 : DISC ERROR !
 : UNRESOLVED FORWARD REFERENCE
 : NOT A WORD, NOR A NUMBER OR OTHER DENOTATION
 : WORD IS NOT FOUND
 : NOT RECOGNIZED ( Try lina -h)
 : ERROR, NO FURTHER INFORMATION
 : SAVE/RESTORE MUST RUN FROM FLOPPY
 : CANNOT FIND WORD TO BE POSTPONED
 : CANNOT FIND WORD TO BE COMPILED
 : COMPILATION ONLY, USE IN DEFINITION
 : EXECUTION ONLY
 : CONDITIONALS NOT PAIRED
 : DEFINITION NOT FINISHED
 : IN PROTECTED DICTIONARY
 : USE ONLY WHEN LOADING
 : OFF CURRENT EDITING SCREEN
 : DECLARE VOCABULARY
 : LIST EXPECTS DECIMAL
 : AS: PREVIOUS INSTRUCTION INCOMPLETE
 : AS: INSTRUCTION PROHIBITED IRREGULARLY
 : AS: UNEXPECTED FIXUP/COMMAER
 : AS: DUPLICATE FIXUP/UNEXPECTED COMMAER
 : AS: COMMAERS IN WRONG ORDER
 : AS: DESIGN ERROR, INCOMPATIBLE MASK
 : AS: PREVIOUS OPCODE PLUS FIXUPS INCONSISTENT































( ************** configuration *******************************)














\
( CONFIG ?LEAVE-BLOCK ?16 ?32 ?LI ?PC ?MS ?FD ?HD ) \ A1oct05


: ?LEAVE-BLOCK IF SRC CELL+ @ IN ! THEN ;
: CONFIG CREATE , DOES> @ ?LEAVE-BLOCK ;
0 CELL+ 2 <> CONFIG ?16
0 CELL+ 4 <> CONFIG ?32
0 CELL+ 8 <> CONFIG ?64
 "LINOS"  PRESENT? 0=   CONFIG ?LI   \ Linux
 "BIOS31" PRESENT? 0=   CONFIG ?WI   \ DPMI ("windows")
 "BDOSN"  PRESENT? 0=   CONFIG ?MS   \ MS-DOS
 "BIOSN"  PRESENT? 0=   CONFIG ?PC   \ Possibly stand alone
 "LBAPAR" PRESENT? 0=   CONFIG ?HD   \ Hard disk, modern
 "SEC-RW" PRESENT? 0=   CONFIG ?FD   \ Floppy or hard disk old

\
( **************ISO language extension ***********************)
                  EXIT

An ISO language extension is either an ISO word, implemented in
a possibly non-portable way, or a word that is not defined in
the standard but that is implemented by using only standard
words.
Sometimes it is an ``in''tension, a loadable extension required
for ISO, that in my opinion should never be loaded.






\
( DEADBEEF leading_hex_digit) CF: \ AvdH A1oct13
\ Are denotations starting with "Z" already known?
"Z" 'DENOTATION >WID (FIND) SWAP DROP SWAP DROP ?LEAVE-BLOCK
\ Apparently not, so:
REQUIRE ALIAS
CURRENT @   'DENOTATION >WID CURRENT !  '3
\ Make sure Forth understands DEADBEEF as a hex number
(   DUP ALIAS A   DUP ALIAS B   DUP ALIAS C   DUP ALIAS D    )
(   DUP ALIAS E   DUP ALIAS F ) DUP ALIAS G   DUP ALIAS H
    DUP ALIAS I   DUP ALIAS J   DUP ALIAS K   DUP ALIAS L
    DUP ALIAS M   DUP ALIAS N   DUP ALIAS O   DUP ALIAS P
    DUP ALIAS Q   DUP ALIAS R   DUP ALIAS S   DUP ALIAS T
    DUP ALIAS U   DUP ALIAS V   DUP ALIAS W   DUP ALIAS X
    DUP ALIAS Y   DUP ALIAS Z
DROP   CURRENT !
\ Use  'DENOTATION >WID CURRENT ! instead of DEFINITIONS
( NIP PICK TUCK ) \ AvdH A1oct22
\ Obscure stack manipulations.
: NIP SWAP DROP ;
: PICK 1+ CELLS DSP@ + @ ;
: TUCK SWAP OVER ;











( 2>R 2R> 2R@ ) \ AvdH A1oct22





\ Double return stack manipulations
: 2>R POSTPONE SWAP POSTPONE >R POSTPONE >R ; IMMEDIATE
: 2R> POSTPONE R>  POSTPONE R> POSTPONE SWAP  ;  IMMEDIATE
: 2R@ POSTPONE 2R> POSTPONE 2DUP POSTPONE 2>R ; IMMEDIATE






( COMPARE BOUNDS ALIGN UNUSED ) CF: \ AvdH A1oct04
\ ISO
 : COMPARE ROT 2DUP SWAP - >R
     MIN CORA DUP IF RDROP ELSE DROP R> THEN ;
\ In general use
: BOUNDS   OVER + SWAP ;

: UNUSED DSP@ HERE - ;
"ALIGNED" PRESENT? ?LEAVE-BLOCK
\ ISO
: ALIGNED    1-   0 CELL+ 1- OR   1+ ;
: ALIGN   DP @   ALIGNED   DP ! ;



\
( --manifest TRUE FALSE NULL NULL$ NONE ) \ AvdH A1oct15
\ Define some manifest constants.
-1 CONSTANT TRUE       \ Flag
0 CONSTANT FALSE       \ Flag
0 CONSTANT NULL        \ Invalid address
: NULL$ 0 0 ;          \ Invalid string
-1 CONSTANT NONE       \ Invalid index, where valid is pos.








\
( U> 0> U.R ) \ AvdH A2oct23



: U> SWAP U< ;
: 0> 0 > ;
: U.R 0 SWAP D.R ;









( DEFER IS ) \ AvdH A2apr22

\ Default action for not filled in deferred word.
: DEFER-ERROR    -1 13 ?ERROR ;

\ Define a word, with a changable behaviour. "deferred" word".
: DEFER CREATE 'DEFER-ERROR , DOES> @ EXECUTE ;

\ Fill XT in as the behaviour of the named deferred word.
\ This ugly word is state-smart!
: IS   (WORD) FOUND >BODY   STATE @ IF
    POSTPONE LITERAL POSTPONE ! ELSE ! THEN ; IMMEDIATE



\
( [IF] ] [ELSE] [THEN] [DEFINED] [UNDEFINED] ) \ AvdH A2oct22
REQUIRE COMPARE
\ From ANSI manual.
: SKIPPING
1 BEGIN (WORD) DUP WHILE
   2DUP "[IF]" COMPARE 0= IF 2DROP 1+ ELSE
   2DUP "[ELSE]" COMPARE 0= IF 2DROP 1- DUP IF 1+ THEN ELSE
      "[THEN]" COMPARE 0= IF 1- THEN THEN THEN
   ?DUP 0= IF EXIT THEN
REPEAT 2DROP DROP ;
: [IF] 0= IF SKIPPING THEN ; IMMEDIATE
: [ELSE] SKIPPING ; IMMEDIATE
: [THEN] ; IMMEDIATE

: [DEFINED] (WORD) 2DUP REQUIRED PRESENT? ;
: [UNDEFINED] [DEFINED] 0= ;
( VALUE TO FROM ) \ AvdH A1oct22


VARIABLE TO-MESSAGE   \ 0 : FROM ,  1 : TO .
: FROM 0 TO-MESSAGE ! ;
\ ISO
: TO 1 TO-MESSAGE ! ;
\ ISO
: VALUE CREATE , DOES> TO-MESSAGE @ IF ! ELSE @ THEN
    0 TO-MESSAGE ! ;






( ORDER .WID .VOCS BUFFER ) \ AvdH A1sep25
\ Print all vocabularies names in existence.
: .VOCS 'ID. FOR-VOCS ;
\ Print a voc's name from the WID)
: .WID 0 CELL+ - BODY> ID. ;
: SEARCH-ORDER CONTEXT ;  \ For backward compatibility.
\ Print the current search order by vocabulary names
: ORDER CONTEXT BEGIN $@ DUP .WID 'ONLY >WID = UNTIL
DROP &[ EMIT SPACE CURRENT @ .WID &] EMIT ;
\ This is a BUFFER compatible with FIG-Forth.
: BUFFER   (BUFFER) CELL+ CELL+ ;




\
( SET !SET SET? SET+! .SET set_utility) \ AvdH 2K2may15

( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP ?DO I ? 0 CELL+ +LOOP ;   ( Print SET )


( Remove entry at ADDRESS from SET. )
: SET-REMOVE   >R   DUP CELL+ SWAP  R@ @ OVER -   MOVE
    -1 CELLS R> +! ;
( For VALUE and SET : value IS present in set.)
: IN-SET? $@ SWAP ?DO
   DUP I @ = IF DROP -1 UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;
( BAG !BAG BAG? BAG+! BAG@- BAG-REMOVE ) \ AvdH A3apr25
REQUIRE @+
( Build a bag "x" with X items. )
: BAG   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !BAG   DUP CELL+ SWAP ! ;   ( Make the BAG empty )
: BAG?   @+ = 0= ;   ( For the BAG : it IS non-empty )
: BAG+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Push ITEM to the BAG )
: BAG@- 0 CELL+ NEGATE OVER +! @ @ ;   ( From BAG: pop ITEM )
( Remove entry at ADDRESS from BAG. )
: BAG-REMOVE   >R   DUP CELL+ SWAP  R@ @ OVER -   MOVE
    -1 CELLS R> +! ;





( DO-BAG LOOP-BAG .BAG BAG-WHERE IN-BAG? BAG- ) \ AvdH A3apr25
\ Loop over a bag, see ``.BAG'' for an example.
: DO-BAG  POSTPONE @+ POSTPONE SWAP POSTPONE ?DO ; IMMEDIATE
: LOOP-BAG 0 CELL+ POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
: .BAG   DO-BAG I ? LOOP-BAG ; ( Print BAG )
( For VALUE and BAG : ADDRESS of value in bag/nill.)
: BAG-WHERE DO-BAG DUP I @ = IF DROP I UNLOOP EXIT THEN
    LOOP-BAG  DROP 0 ;
( For VALUE and BAG : value IS present in bag.)
: IN-BAG? BAG-WHERE 0= 0= ;
( Remove VALUE from BAG. )
: BAG-   DUP >R   BAG-WHERE   R> BAG-REMOVE ;
( Add VALUE to bag, used as a SET, i.e. no duplicates.)
: SET+   2DUP IN-BAG? IF 2DROP ELSE BAG+! THEN ;
(   : BAG+ BAG+! ;    : SET- BAG- ;                           )

( BIN-SEARCH binary_search_by stack ) \ AvdH
( nmin nmax xt -- nres )
\ See description in next screen.
\
VARIABLE COMP \ Execution token of comparison word.
: BIN-SEARCH    >R
    BEGIN       \ Loop variant IMAX - IMIN
      2DUP  <> WHILE
      2DUP + 2/  ( -- ihalf )
      DUP R@ EXECUTE IF
         1+  SWAP ROT DROP \ Replace IMIN
      ELSE
         SWAP DROP \ Replace IMAX
      THEN
    REPEAT
DROP RDROP ;
( binary_search_description )
EXIT
( BIN-SEARCH    : n IMIN, n IMAX, xt COMP -- n IRES )
Uses a comparison routine with execution token `COMP'
`COMP' must have the stack diagram ( IT -- flag) , where flag
typically means that IT compares lower or equal to some fixed
value. It may be  TRUE , FALSE or undefined for `IMIN' , but
it must be monotonic down in the range [IMIN,IMAX), i.e.
if IMIN<=IX<=IY<IMAX then if IX COMP gives false, IY COMP
cannot give true.

BIN-SEARCH finds the first index `IT' between `IMIN'
and `IMAX' (exclusive) for which `COMP' returns false.
or else ``IMAX''.
An empty range is possible, (`IMIN' and `IMAX' are equal.)
See also  binary_search_test in the examples section.
( EXCHANGE PAIR[] --qsort-auxiliary ) \ AvdH A2apr22

\ Exchange the content at ADDR1 and ADDR2 over a fixed LENGTH.
: EXCHANGE 0 ?DO   OVER I +     OVER I +  OVER C@   OVER C@
                 >R SWAP C!  R> SWAP C! LOOP 2DROP ;

\ For INDEX1 and INDEX2 and TABLE, return corresponding
\ ADDRESS1 and ADDRESS2 .
: PAIR[] >R   CELLS R@ + SWAP   CELLS R@ + SWAP   RDROP ;






\
( QSORT ) \ AvdH A2apr22
REQUIRE DEFER   REQUIRE +THRU
\ Compare item N1 and N2. Return ``N1'' IS lower and not equal.
DEFER *<
\ Exchange item N1 and N2.
DEFER *<-->

1 2 +THRU








( QSORT_part2 ) \ AvdH A2apr22
\ Partition inclusive range LO HI leaving LO_1 HI_1 LO_2 HI_2.
: PARTITION   2DUP + 2/   >R  ( R: median)
    2DUP BEGIN      ( lo_1 hi_2 lo_2 hi_1)
       SWAP BEGIN  DUP R@ *< WHILE  1+  REPEAT
       SWAP BEGIN  R@ OVER *< WHILE  1-  REPEAT
       2DUP > 0= IF
          \ Do we have a new position for our pivot?
          OVER R@ = IF RDROP DUP >R ELSE
          DUP  R@ = IF RDROP OVER >R THEN THEN
          2DUP *<-->
          >R 1+ R> 1-
      THEN
    2DUP > UNTIL    ( lo_1 hi_2 lo_2 hi_1)
    RDROP                            ( R: )
    SWAP ROT ;      ( lo_1 hi_1 lo_2 hi_2)
( QSORT_part3 ) \ AvdH A2apr22
\ Sort the range LOW to HIGH inclusive observing
\ ``LOW'' and ``HIGH'' must be indices compatible with the
\   current values of *< and *<-->
: (QSORT)             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;
\ Sort the range FIRST to LAST (inclusive) of item compared
\ by the xt COMPARING and exchanged by the xt EXHANGING.
\ All indices in this range must be proper to pass to the xt's.
\ The xt's are filled in into *< and *<--> and must observe the
\ interface.
\ After the call we have that :
\ ``For FIRST<=I<J<=LAST      I J *<--> EXECUTE leaves TRUE.''
: QSORT   IS *<-->   IS *<   (QSORT) ;
\ (MERGE)       \ AvdH A3dec02

\ For EL1 and EL2, return EL1 and EL2 plus "el1 IS lower".
VARIABLE *<M   : LL< *<M @ EXECUTE ;
VARIABLE *->M   \ Contains XT
\ For EL return next EL of list.
: >N   *->M @ EXECUTE @ ;
\ For LIST and EL , hang the list off the element.
: LINK! *->M @ EXECUTE ! ;
\ For LIST1 ( > ) LIST2 return LIST1 (>) LIST2' advanced
: FIND-END   BEGIN DUP >R >N   DUP IF LL< 0= ELSE 0 THEN
    WHILE RDROP REPEAT DROP R> ;
\ Merge LIST1 ( > ) LIST2.
: (MERGE)   BEGIN FIND-END   DUP >R  DUP >N >R LINK!
    R> R>   OVER 0= UNTIL 2DROP ;

( MERGE-SORT )  REQUIRE (MERGE)  \ AvdH A3dec02
\ Merge SLIST1 and SLIST2, leave merged SLIST.
: MERGE   LL< IF SWAP THEN   DUP >R (MERGE) R> ;
\ Cut ULIST in two. Return SLIST and remaining ULIST.
: SNIP DUP   BEGIN DUP >N   DUP IF LL< ELSE 0 THEN
    WHILE SWAP DROP REPEAT   >R 0 SWAP LINK! R> ;
\ Keep on merging as long as two slistp 's have the same level.
: TRY-MERGES   BEGIN >R  OVER R@ =
    WHILE SWAP DROP MERGE R> 1+ REPEAT R> ;
\ Keep on merging until end-sentinel.
: SHRINK DROP BEGIN OVER WHILE SWAP DROP MERGE REPEAT ;
\ Expand zero, ulist into zero slistp .... slistp
: EXPAND BEGIN SNIP >R 1 TRY-MERGES R> DUP WHILE REPEAT DROP ;
\ For compare XT, next XT, linked LIST , leave a sorted LIST1.
: MERGE-SORT   *->M !  *<M !   0 SWAP EXPAND SHRINK SWAP DROP ;

( SORT-WID SORT-VOC )           \ AvdH A3dec02
REQUIRE COMPARE         REQUIRE MERGE-SORT
\ Note that xt's form a linked list
\ For XT1 and XT2 return XT1 and XT2 plus "xt IS lower".
    : GET-NAME >NFA @ $@   ;  \ Aux. For EL, return NAME.
: NAMES< DUP >R OVER GET-NAME    R> GET-NAME    COMPARE 0 < ;

\ Sort the WORDLIST alphabetically on names.
\ This head of the list doesn't take part in the
\ sorting (expect for the link field) , so it may be a dummy.
: SORT-WID >LFA DUP >R   @ 'NAMES< '>LFA MERGE-SORT   R> ! ;

\ Sort the vocabulary given its vocabulary XT.
\ Don't use it on FORTH : it links through DENOTATIONS .
: SORT-VOC >WID SORT-WID ;

( CRC-MORE CRC ) ?32 \ AvdH
REQUIRE BOUNDS   REQUIRE NEW-IF    HEX
\ Well the polynomial
EDB8,8320 CONSTANT CRC32_POLYNOMIAL

\ Auxiliary table with values for single bytes.
CREATE CRCTable
100 0 DO   I 8 0 DO
    DUP >R   1 RSHIFT   R> 1 AND IF CRC32_POLYNOMIAL XOR THEN
LOOP ,   LOOP
\ For initial CRC and BUFFER COUNT pair, leave the updated CRC
: CRC-MORE   BOUNDS DO  DUP I C@ XOR 0FF AND CELLS CRCTable + @
   SWAP 8 RSHIFT XOR   LOOP ;
\ For BUFFER COUNT pair, leave the CRC .
: CRC   -1 ROT ROT CRC-MORE INVERT ;
DECIMAL
( STACK PUSH POP --a_free_stack ) \ AvdH A1sep26
: STACK CREATE HERE CELL+ , CELLS ALLOT DOES> ;
100 STACK DEBUG-STACK
: PUSH DEBUG-STACK @ SWAP OVER ! 1 CELLS +  DEBUG-STACK ! ;
: POP DEBUG-STACK @ 1 CELLS - DUP @ SWAP DEBUG-STACK ! ;











( -LEADING DROP-WORD                   ) \ AvdH A3mar21
REQUIRE COMPARE
 : -LEADING ( $T,$C -$T,$C   Like -TRAILING, removes)
    BEGIN                        ( heading blanks )
      OVER C@ BL = OVER 0= 0=  AND
    WHILE
      1 - SWAP 1 + SWAP
    REPEAT  ;


\ From a STRING remove the first word. Leave the rest STRING.
: DROP-WORD   -LEADING BL $S 2DROP ;




( RAND ) HEX \ EDN 1991JAN21, pg 151
REQUIRE TICKS
VARIABLE SEED
( . -- . ) ( Use the nanosecond counter to start)
: RANDOMIZE TICKS DROP SEED ! ;

( -- N  Leave a random number )
: RAND SEED @ 107465 * 234567 + DUP SEED ! ;

( N -- R Leave a random number < N)
: CHOOSE RAND UM* SWAP DROP ;
( Swap the number at ADDRESS1 with one at ADDRESS2 )
: @SWAP  OVER @   OVER @   SWAP   ROT !   SWAP ! ;
( RANDOM-SWAP ( R N -- )
( 1 - CHOOSE 1+ CELLS OVER + @SWAP ;)  DECIMAL
RANDOMIZE
( TIME&DATE ) CF: \ AH A30610
REQUIRE +THRU
1 2 +THRU













( TIME&DATE ) ?LI \ AH A30610
: SSE   0 0 0 13 LINOS ; ( Seconds since epoch: 1970/1/1)
: |   OVER , + ;   : 5m   31 | 30 | 31 | 30 | 31 | ;
CREATE TABLE ( start of month within leap period) 0
    31 | 28 | 5m 5m   31 | 28 | 5m 5m   31 | 29 | 5m 5m
    31 | 28 | 5m 5m   DROP    : T[] CELLS TABLE + @ ;
\ For DAYS within leap return MONTHS
: MONTHS   >R 0 BEGIN R@ OVER T[] > WHILE 1+ REPEAT 1- RDROP ;
\ For DAYS within leap period return DAY MONTH YEARS
: SPLIT-LEAP  DUP MONTHS DUP >R T[] - 1+  R> 12 /MOD >R 1+ R> ;
\ For TIME return SEC MIN HOUR DAYS
: SPLIT-OFF-TIME   0 60 UM/MOD   60 /MOD   24 /MOD ;
\ For DAYS return DAY MONTH YEAR
: SPLIT-OFF-DATE  1461 /MOD >R SPLIT-LEAP   R> 4 * + 1970 + ;
\ Return current  SEC MIN HOUR DAY MONTH YEAR
: TIME&DATE   SSE   SPLIT-OFF-TIME   SPLIT-OFF-DATE ;
( TIME&DATE ) ?PC \ AH A30612

HEX

\ Return current  DAY MONTH YEAR
: DATE    2A00 _ _ _ BDOSO DROP SWAP >R >R 2DROP
    R> 100 /MOD   R> ;

\ Return current  SEC MIN HOUR
: TIME    2C00 _ _ _ BDOSO DROP SWAP >R >R 2DROP
    R> 100 /   R> 100 /MOD ;

\ Return current  SEC MIN HOUR DAY MONTH YEAR
: TIME&DATE TIME DATE ;

DECIMAL
( **************Non ISO language extension *******************)















( $ ESC SI SO hex_numbers_denotation ) \ AvdH A1apr15
'DENOTATION >WID CURRENT !
\ DEFINITIONS PREVIOUS doesn't work because DEF.. is
\ found in the DENOTATION wordlist (!)
: $ BASE @ >R HEX (NUMBER) R> BASE ! POSTPONE SDLITERAL ;
12 LATEST >FFA !
DEFINITIONS

\ Some constants
$1B CONSTANT ESC    $0F CONSTANT SI   $0E CONSTANT SO






( +THRU ) \ AvdH A1oct05
\ Load current block plus N1 to current block plus N2.
: +THRU   SRC @ 2 CELLS - @ >R
    R@ + SWAP   R> + SWAP
    THRU ;











(  H. B. DH. BASE? <HEX ) \ AvdH A1oct04

CREATE BASE' 0 ,
 : <HEX   BASE @ BASE' ! HEX ;       ( 0/1  SWITCH TO HEX)
 : HEX>   BASE' @ BASE !     ;       ( 1/0  AND BACK)
( Add a . after 4 digits )
 : 4?  1+ 4 MOD 0= IF &, HOLD THEN ;
( Generate string with hex format of DOUBLE of LEN digits)
 : (DH.) <HEX <# 1- 0 DO # I 4? LOOP # #> HEX> ;
 : DH. 4 CELLS (DH.) TYPE ; (  print DOUBLE in hex )
 : H.  S>D 2 CELLS (DH.) TYPE ; ( print SINGLE in hex )
 : B.  S>D 2 (DH.) TYPE ; ( print BYTE in hex )

 : BASE?  BASE @ B. ;                ( 0/0 TRUE VALUE OF BASE)


(  ALIAS @+ HIDE INCLUDE IVAR ) CF: \ A3apr25

: ALIAS  (WORD) (CREATE) LATEST 3 CELLS MOVE ;

'$@ ALIAS @+

: HIDE (WORD) FOUND DUP 0= 11 ?ERROR HIDDEN ;

\ : FORGET (WORD) FOUND DUP 0= 11 ?ERROR FORGOTTEN ;
: IVAR CREATE , ;

"INCLUDED" PRESENT? 0= ?LEAVE-BLOCK

: INCLUDE (WORD) INCLUDED ;


( SLITERAL PARSE STRING $. $? Elementary_string) \ AvdH A10ct08

\ ISO
: SLITERAL POSTPONE SKIP $, POSTPONE LITERAL POSTPONE $@ ;
IMMEDIATE
\ ISO
: PARSE (PARSE) ;

\ Other words
 : $. TYPE ;
 : $? $@ $. ;


: STRING CREATE &" (PARSE) $, DROP DOES> $@ ;


( MS@ TICKS TICKS-PER-SECOND ) \ AvdH A2oct21
CF:   REQUIRE +THRU
1 2 +THRU

TICKS











( MS@ TICKS TICKS-PER-SECOND ) \ AvdH A2oct21
\ Assuming we run on an 486 or better, and a 32 bits Forth
REQUIRE ASSEMBLERi86 HEX
CODE  TEST-EF POPF, PUSHF, NEXT END-CODE
DECIMAL
1 21 LSHIFT CONSTANT ID-FLAG
: MS@ 0 0 0 43 LINOS 10 * ;
ID-FLAG TEST-EF 0 TEST-EF XOR ID-FLAG AND 0= ?LEAVE-BLOCK
HEX CODE CPUID POP|X, AX| 0F C, A2 C, PUSH|X, DX| NEXT END-CODE
1 CPUID 10 AND 0= DECIMAL ?LEAVE-BLOCK HEX
CODE TICKS 0F C, 31 C, 50 C, 52 C, NEXT END-CODE
DECIMAL TICKS DNEGATE 1000 MS TICKS D+ DROP
  CONSTANT TICKS-PER-SECOND
\ For a TIME in ticks: it IS in the past.
: PAST? DNEGATE TICKS D+ SWAP DROP 0< 0= ;
: MS@ TICKS TICKS-PER-SECOND 1000 / M/MOD DROP SWAP DROP ;
( MS@ TICKS TICKS-PER-SECOND ) ?PC ?32 \ AvdH A2oct21
\ The idea is to use the timer ticks on the pc.
\ Until then the following works for 486 and better.
REQUIRE ASSEMBLERi86 HEX
\  CODE TICKS 0F C, 31 C, PUSH, AX| PUSH, DX| NEXT END-CODE

DECIMAL
." What is the speed of your Pentium (in Mhz)?"
PAD DUP 80 ACCEPT EVALUATE CONSTANT TICKS-PER-SECOND

\ For a TIME in ticks: it IS in the past.
: PAST? DNEGATE TICKS D+ SWAP DROP 0< 0= ;

: MS@ TICKS TICKS-PER-SECOND 1000 / M/MOD DROP SWAP DROP ;


( MARK-TIME .mS .uS ELAPSED ) \ AvdH A2oct21
REQUIRE TICKS
DECIMAL
\ Mark a point in time by leaving its tick COUNT.
: MARK-TIME TICKS ;
\ Print a TIME interval, given in uS as ms.
: .mS SPACE 0 <# # # # [CHAR] . HOLD #S #> TYPE ." mS "  ;
\ Print a TIME interval, given in uS, as us.
: .uS SPACE . ." uS "  ;
\ For the TIME (in ticks) on the stack return ELAPSED time
\ since then, in uS.
: ELAPSED   DNEGATE TICKS D+ DROP
    1,000,000 TICKS-PER-SECOND */ ;
DECIMAL


( MEASURE-PRIME test_for_TIME ) \ AvdH A1oct05
 : TASK ;
REQUIRE ASSEMBLERi86 \ Otherwise nesting gets too deep
REQUIRE DO-PRIME-ISO   REQUIRE MARK-TIME


: MEASURE-PRIME
  TICKS DO-PRIME-ISO DROP ELAPSED
  CR  ." THE ISO BYTE BENCHMARK LASTED " .mS  ;

  MEASURE-PRIME

CR ." FORGET ``MEASURE-PRIME'' Y/N" KEY DUP EMIT
&Y <>  ?LEAVE-BLOCK
  FORGET TASK

( FAR-DP SWAP-DP scratch_dictionary_area ) \ AvdH A1oct21
VARIABLE FAR-DP         \ Alternative DP
DSP@ 1 RSHIFT HERE 1 RSHIFT + ALIGNED FAR-DP !
\ Use alternative dictionary area or back.
: SWAP-DP   DP @ FAR-DP @   DP ! FAR-DP ! ;
\ Remove all words from the scratch area.
: TRIM   HERE 'FORGET-VOC FOR-VOCS DROP ;









( T] T[ ) \ AvdH A1oct04
REQUIRE SWAP-DP

\ Compile at temporary place : remember old HERE and STATE.
: T] STATE @ 0= IF SWAP-DP HERE THEN STATE @ ] ;

\ Execute code at old HERE , restore STATE and dictionary.
: T[ 0= IF POSTPONE (;) SWAP-DP POSTPONE [ >R THEN ; IMMEDIATE








( NEW-IF interpreting__control_words ) \ AvdH A1oct04
: NEW-IF ;
REQUIRE T[
: IF           T] POSTPONE IF                    ; IMMEDIATE
: DO           T] POSTPONE DO                    ; IMMEDIATE
: ?DO          T] POSTPONE ?DO                   ; IMMEDIATE
: BEGIN        T] POSTPONE BEGIN                 ; IMMEDIATE
: THEN            POSTPONE THEN      POSTPONE T[ ; IMMEDIATE
: LOOP            POSTPONE LOOP      POSTPONE T[ ; IMMEDIATE
: +LOOP           POSTPONE +LOOP     POSTPONE T[ ; IMMEDIATE
: REPEAT          POSTPONE REPEAT    POSTPONE T[ ; IMMEDIATE
: UNTIL           POSTPONE UNTIL     POSTPONE T[ ; IMMEDIATE




( if do ?do begin then loop +loop repeat until ) \ AvdH A1oct04
\ The same but without the annoying message.
REQUIRE T[
: if           T] POSTPONE IF                    ; IMMEDIATE
: do           T] POSTPONE DO                    ; IMMEDIATE
: ?do          T] POSTPONE ?DO                   ; IMMEDIATE
: begin        T] POSTPONE BEGIN                 ; IMMEDIATE
: then            POSTPONE THEN      POSTPONE T[ ; IMMEDIATE
: loop            POSTPONE LOOP      POSTPONE T[ ; IMMEDIATE
: +loop           POSTPONE +LOOP     POSTPONE T[ ; IMMEDIATE
: repeat          POSTPONE REPEAT    POSTPONE T[ ; IMMEDIATE
: until           POSTPONE UNTIL     POSTPONE T[ ; IMMEDIATE
: else            POSTPONE ELSE                  ; IMMEDIATE



( OLD: RESTORED POSTFIX ) \ AvdH A2jun12
\ WARNING: use these facilities only with high level words.

\ Compile the current execution behaviour of "name".
\ This behaviour remains the same if "name" is revectored.
: OLD:   (WORD) FOUND >DFA @ POSTPONE LITERAL   POSTPONE >R
    POSTPONE CO ; IMMEDIATE
\ Have the original behaviour of DEA restored.
: RESTORED   DUP >PHA SWAP >DFA ! ;
\ Do nothing for one call of ``(WORD)''
: (WORD)-NEW   '(WORD) RESTORED ;
\ Make the following defining word postfix for one execution.
\ The name must be a string constant on the stack
\ Use only while compiling, or you crash the system
: POSTFIX ( ?COMP ) '(WORD)-NEW >DFA @ '(WORD) >DFA ! ;

( Z$@ CTYPE C$.S ) \ AvdH A3mar20

\ For a CSTRING (pointer to zero ended chars) return a STRING.
: Z$@ DUP BEGIN COUNT 0= UNTIL 1- OVER - ;

\ Print a CSTRING.
: CTYPE Z$@ TYPE ;

\ Print a zero-pointer ended ARRAY of ``CSTRINGS'' . Abuse $@.
: C$.S BEGIN $@ DUP WHILE CTYPE CR REPEAT 2DROP ;






( ARGC ARG[] SHIFT-ARGS SRC>EXEC ) \ AvdH A3mar20


CF:   REQUIRE +THRU

1 4 +THRU










( ARG ARGC ARGV ARG[] SHIFT-ARGS ENV ) ?LI \ AvdH A3mar20
REQUIRE Z$@   REQUIRE COMPARE
\ Return the NUMBER of arguments passed by Linux
: ARGC   ARGS @   @ ;
\ Return the argument VECTOR passed by Linux
: ARGV   ARGS @   CELL+ ;
\ Return the environment POINTER passed by Linux
: ENV   ARGS @   $@ 1+ CELLS + ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[] CELLS ARGV + @ Z$@ ;
\ Return POINTER behind the end-0 of the environment.
: ENV0 ENV BEGIN $@ WHILE REPEAT ;
\ Shift the arguments, so as to remove argument 1.
: SHIFT-ARGS   -1 ARGS @ +!
    ARGV CELL+ >R   R@ CELL+   R@ ENV0 R> CELL+ - MOVE ;

( SRC>EXEC   ) ?LI \ AvdH A3mar20

\ Given a source file NAME, return the binary file NAME.
: SRC>EXEC   4 -   2DUP + ".frt" CORA IF 2DROP "a.out" THEN ;












( ARG$ ARGC ARG[] SHIFT-ARGS ) ?PC \ AvdH A3mar25
HEX    REQUIRE DROP-WORD
\ Return argument STRING for (prot) DOS.
: ARG$   80 COUNT -LEADING -TRAILING ;

\ Return the NUMBER of arguments.
: ARGC  ARG$ 80 1 DO DUP 0= IF I LEAVE THEN DROP-WORD LOOP
   >R 2DROP R> ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[]   >R ARG$   R@ 1 < 0D ?ERROR
    R> 1 ?DO DROP-WORD LOOP   -LEADING BL $S 2SWAP 2DROP ;

\ Shift the arguments, so as to remove argument 1. Keep cr!
: SHIFT-ARGS   ARG$ DROP-WORD   80 $!-BD   ^M ARG$ + C! ;
DECIMAL

( SRC>EXEC ) ?PC                \ AvdH A3mar20
HEX
\ Given a source file NAME, return the binary file NAME.
: SRC>EXEC   PAD $!   PAD $@ + 4 - >R
   R@ 4 + R@ 1 + DO I C@ 20 INVERT AND I C! LOOP \ Uppercase
   R> ".FRT" CORA IF "AOUT.EXE" ELSE
   -4 PAD +!   ".EXE" PAD $+!   PAD $@ THEN ;
DECIMAL








( GET-ENV ) CF: ?LI \ AvdH A3mar20
\ This must be defined on MS-DOS too

REQUIRE Z$@   REQUIRE COMPARE   REQUIRE ENV

\ For SC and ENVSTRING leave SC / CONTENT and GOON flag.
: (MENV)   DUP 0= IF   DROP 2DROP 0. 0   ELSE
    Z$@ &= $S 2SWAP >R >R 2OVER COMPARE
    IF   RDROP RDROP 1   ELSE   2DROP R> R> 0   THEN THEN ;
( Find a STRING in the environment, -its VALUE or NULL string)
: GET-ENV ENV BEGIN $@ SWAP >R (MENV) WHILE R> REPEAT RDROP ;





( SAVE-SYSTEM TURNKEY ) \ AvdH
CF:   REQUIRE +THRU
1 3 +THRU













( SAVE-SYSTEM TURNKEY ) ?LI HEX \ AvdH
\ The magic number marking the start of an ELF header
 CREATE MAGIC 7F C, &E C, &L C, &F C,
\ Return the START of the ``ELF'' header.
 : SM BM BEGIN DUP @ MAGIC @ <> WHILE 1 CELLS - REPEAT ;
\ Return the VALUE of ``HERE'' when this forth started.
 : HERE-AT-STARTUP  'DP >DFA @ +ORIGIN @ ;
\ Save the system in a file with NAME .
 : SAVE-SYSTEM
\ Increment the file and dictionary sizes
  HERE HERE-AT-STARTUP - DUP SM 20 + +!      SM 44 + +!
   U0 @   0 +ORIGIN   40 CELLS  MOVE \ Save user variables
\ Now write it. Consume NAME here.
   SM    HERE OVER -   2SWAP   PUT-FILE ;  DECIMAL
\ Save a system to do ACTION in a file with NAME .
: TURNKEY  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ;
( --save_system_turnkey ) ?PC HEX \ AvdH
\ Write an MSDOS ``EXEHEADER'' structure over the PSP.
VARIABLE HEAD-DP  \ Fill in pointer

\ Add a 16 bit WORD to the header.
: W, HEAD-DP @ >R   DUP R@ C!   8 RSHIFT R@ 1+ C!
    R> 2 + HEAD-DP ! ;

\ Return the SIZE of the system, including header,
: SIZE HERE 10 + 1- 1FF OR 1+ ;  \ 10 byte stack. Whole pages.

\ Fill in ``EXEHEADER'' struct, leave POINTER to where checksum
\ must be filled in.
: EXEHEADER   0 HEAD-DP !  5A4D W,   0 W,   SIZE  200 / W,
    0 W,   10 W,   10 W,   10 W,   SIZE 10 - 10 / W,   10 W,
    HERE   0 W,   100 W,   -10 W,  SIZE W,  0  W, ;
( SAVE-SYSTEM TURNKEY ) CF: ?PC HEX \ AvdH

\ Fill in checksum at the required POSITION in the header.
: CHECKSUM   HEAD-DP !
    0   SIZE 0 DO I @ + 2 +LOOP  NEGATE   W, ;
: SAVE-PSP   0 PAD 100 CMOVE ;
: RESTORE-PSP  PAD 0 100 CMOVE ;
\ Save the system in a file with NAME .
: SAVE-SYSTEM   10 ALLOT SAVE-PSP ( startup stack)
   U0 @   0 +ORIGIN   40 CELLS  MOVE \ Save user variables
   EXEHEADER CHECKSUM   0 SIZE 2SWAP PUT-FILE   RESTORE-PSP ;

\ Actually this is the same than in Linux.
\ Save a system to do SOMETHING in a file with NAME .
: TURNKEY  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ;

( CVA  aux_for_threads ) \ AvdH A2jul5
HEX   10000 CONSTANT RTS     40 CELLS CONSTANT US

\ Clone the stack frame to ``RTS'' lower in memory.
\ This area extends up to ``R0 @ US + '' .
\ The Forth is left running in the new area.
: CVA  DSP@   DUP RTS -   U0 @ DSP@ - US +   MOVE
       DSP@ RTS - DSP!    RSP@ RTS - RSP!
       4 -1 DO   RTS NEGATE U0 I CELLS +   +!   LOOP ;






\
( THREAD-PET KILL-PET PAUSE-PET ) CF: ?LI \ A2nov16
REQUIRE CVA   HEX
\ Exit a thread. Indeed this is exit().
: EXIT-PET 0 _ _ 1 LINOS ;
\ Do a preemptive pause. ( more or less 1 MS )
: PAUSE-PET 0 0 1000 0 DSP@ 8E LINOS5 DROP ;

\ Create a thread with dictionary SPACE. Execute XT in thread.
: THREAD-PET CREATE S0 @ 2 CELLS - , R0 @ , 0 , CVA ALLOT
    DOES> >R  ( xt) R@ @ CELL+ !   R@ CELL+ @  ( R0) R@ @ !
    112 R@ @ _ 78 LINOS DUP 0< IF THROW THEN
    DUP IF ( Mother) R> 2 CELLS + !
    ELSE ( Child) DROP RSP! EXECUTE EXIT-PET THEN ;
\ Kill a THREAD-PET , preemptively. Throw errors.
: KILL-PET >BODY 2 CELLS + @ 9 _ 25 LINOS ?ERRUR ;
DECIMAL
( TASK-TABLE NEXT-TASK PAUSE-COT) HEX \ AvdH A2jul5
REQUIRE SET
100 SET TASK-TABLE   VARIABLE TASK-POINTER
: THIS TASK-POINTER @ ;
\ Make first task current.
: SET-FIRST-TASK TASK-TABLE CELL+ TASK-POINTER ! ;

\ Add running task and make it current.
TASK-TABLE !SET   _ TASK-TABLE SET+!   SET-FIRST-TASK

\ Switch to next task, only administration.
: NEXT-TASK   THIS CELL+ TASK-POINTER !
    THIS TASK-TABLE @ = IF SET-FIRST-TASK THEN ;
\ Switch from current task to next one.
: PAUSE-COT
    DSP@ >R RSP@ THIS !   NEXT-TASK   THIS @ RSP! R> DSP! ;
( EXIT-COT THREAD-COT ) HEX \ AvdH A2jul5
REQUIRE TASK-TABLE   REQUIRE CVA

\ Exit: remove current task, then chain to first one.
: EXIT-COT  THIS TASK-TABLE SET-REMOVE
    SET-FIRST-TASK   THIS @ RSP! R> DSP! ;

\ Create a thread with dictionary SPACE. Execute XT in thread.
: THREAD-COT   CREATE R0 @ 3 CELLS - , S0 @ , CVA ALLOT
    DOES> >R
    R@ CELL+ @          R@ @   !
    >DFA @              R@ @ CELL+   !
    'EXIT-COT >DFA @    R@ @ CELL+ CELL+ !
    R> @ TASK-TABLE SET+! ;

\
( NESTED-COMPILE ) \ AvdH A2oct28

\ Isolate the latest word from the dictionary. Leave its DEA.
: UNLINK-LATEST LATEST CURRENT @ >LFA DUP @ >LFA @ SWAP ! ;

\ Link DEA into the dictionary, as the latest.
: LINK-LATEST LATEST OVER >LFA ! CURRENT @ >LFA ! ;

\ Save all compilation information on the return stack. It is
\ restored upon exit of the calling word.
: NESTED-COMPILE R> DPL @ >R UNLINK-LATEST >R
  STATE @ >R   >R CO    R> STATE ! R> LINK-LATEST R> DPL !
  ;


\
\ LATEST-WORD (WORD-BACK)             \ A2oct28 AvdH
\ Trim a possible leading &' from a word.
: TRIM' OVER C@ &' = IF 1- SWAP 1+ SWAP THEN ;
\ Fpr POINTER into/past word, return START of word.
: (WORD-BACK) BEGIN 1- DUP C@ ?BLANK 0= UNTIL 1+
    BEGIN 1- DUP C@ ?BLANK UNTIL 1+ ;
\ Return SC the latest word in the input.
: LATEST-WORD IN @ (WORD-BACK) SRC @ MAX IN ! (WORD) ( TRIM') ;
\ The compiled program can't run.
VARIABLE FAILED    0 FAILED !
\ The compiled program can run, after reload.
VARIABLE SECOND-PASS 0 SECOND-PASS !
: .SUCCESS "REQUIRE " TYPE TYPE CR ;
: .FAILURE "Find out about " TYPE TYPE CR -1 FAILED ! ;


( REMEDY FIX-DEA FIX-NMB )             \ A2oct28 AvdH
REQUIRE SWAP-DP    REQUIRE LATEST-WORD   REQUIRE NESTED-COMPILE
\ Try to add the current, missing word to the dictionary: DEA.
: REMEDY NESTED-COMPILE   POSTPONE [ SWAP-DP LATEST-WORD 2DUP
 REQUIRED SWAP-DP   2DUP PRESENT? IF 2DUP .SUCCESS FOUND ELSE
.FAILURE 'NOOP THEN ;
\ Make words that look like malformed numbers (like 2R> )
\ compile without error, but with run time errors.
\ Loading the same code another time will give correct code.
: FIX-NMB REMEDY 0 DSP@ 3 CELLS + ! DROP -1 IN +!
-1 SECOND-PASS !   -1 POSTPONE LITERAL   13 POSTPONE LITERAL
 POSTPONE ?ERROR   " Recompile!" TYPE CR ;
\ Fix up errors caused by unknown words, if the library can
\ resolve them. At least go on compiling.
: FIX-DEA REMEDY 0 DSP@ 3 CELLS + ! DSP@ 3 CELLS + ! ;

( ?ERROR-FIXING AUTOLOAD NO-AUTOLOAD ) \ A2oct28 AvdH
REQUIRE OLD: REQUIRE FIX-NMB REQUIRE FIX-DEA
\ Replacement for ?ERROR. Fix up errors, see FIX-NMB FIX-DEA.
: ?ERROR-FIXING OVER IF
DUP 10 = IF FIX-NMB ELSE DUP 11 = IF FIX-DEA ELSE
DUP 12 = IF FIX-DEA ELSE DUP 15 = IF FIX-DEA ELSE
DUP 16 = IF FIX-DEA ELSE THEN THEN THEN THEN THEN
THEN OLD: ?ERROR ;
\ Try to automatically load missing words.
: AUTOLOAD '?ERROR-FIXING >DFA @ '?ERROR >DFA ! ;
: NO-AUTOLOAD '?ERROR RESTORED ;  \ And off again.





( PD PE PC PS get_selectors/descriptors ) \ AvdH A1nov02
REQUIRE ASSEMBLERi86 HEX
CODE PC PUSHS, CS| NEXT END-CODE
CODE PD PUSHS, DS| NEXT END-CODE
CODE PE PUSHS, ES| NEXT END-CODE
CODE PS PUSHS, SS| NEXT END-CODE

DECIMAL








( GET-SEL PUT-SEL NEW-SEL handle_DPMI_selectors) ?WI \ A1nov02
HEX : 4DROP   2DROP 2DROP ;  : BIOS31+ BIOS31 1 AND 0D ?ERROR ;
\ Get the content of the DESCRIPTOR into the BUFFER
: GET-DES >R >R 0B R> 0 0 R> BIOS31+ 4DROP ;
\ Make the selector DESCRIPTION 32 bits from 16 bits or vv.
: TOGGLE-32   6 + 40 TOGGLE ;
\ Make the selector DESCRIPTION code from data or vv.
: TOGGLE-CODE   5 + 8 TOGGLE ;  : TOGGLE-PRES   5 + 80 TOGGLE ;
\ For an existing SELECTOR, return an ALIAS
: GET-ALIAS  0A SWAP 0 0 0 BIOS31+ 2DROP DROP ;
\ Get a SELECTOR to BUFFER
: GET-SEL   >R 0B SWAP 0 0 R> BIOS31+ 4DROP ;
\ Install a SELECTOR from BUFFER
: PUT-SEL   >R 0C SWAP 0 0 R> BIOS31+ 4DROP ;
\ Return a freshly allocated SELECTOR
: NEW-SEL   0 0 1 0 0 BIOS31+ 2DROP DROP ; DECIMAL
( **************ciforth tools*********************************)















( SET-TRAPS ) CF: \ AvdH A3jun12

\ Default (MSDOS) no worky-worky
: SET-TRAPS  DROP ;

?LI
'SET-TRAPS HIDDEN
\ Make sure any traps restart Forth at ADDRESS .
: SET-TRAPS  32 0 DO I OVER _ 48 LINOS DROP LOOP DROP ;







( DO-DEBUG NO-DEBUG ^ ) \ AvdH A1nov24
REQUIRE OLD:    REQUIRE SET-TRAPS
\ An alternative ``OK'' message with a stack dump.
: NEW-OK   .S ."  OK " ;
\ Print index line of SCREEN .
: .INDEX-LINE  0 SWAP (LINE) -TRAILING TYPE ;
\ An alternative ``THRU'' that displays first and last index.
: NEW-THRU  OVER .INDEX-LINE " -- " TYPE  DUP .INDEX-LINE CR
  OLD: THRU ;

\ Install and de-install the alternative ``OK''
: DO-DEBUG   'WARM 16 - SET-TRAPS
   'NEW-OK >DFA @   'OK >DFA !
   'NEW-THRU >DFA @   'THRU >DFA ! ;
: NO-DEBUG   0 SET-TRAPS  'OK RESTORED   'THRU RESTORED ;
: ^ .S ;
( CASE-INSENSITIVE CASE-SENSITIVE CORA-IGNORE ) \ AvdH A2oct24
REQUIRE RESTORED HEX
\ Characters ONE and TWO are equal, ignoring case.
: C=-IGNORE DUP >R   XOR DUP 0= IF 0= ELSE
     20 <> IF 0 ELSE
     R@ 20 OR &a &z 1+ WITHIN THEN THEN  RDROP ;
\ ( sc1 sc2 -- f) f means equal (0) or not. No lexicography.
: CORA-IGNORE 0 DO   OVER I + C@   OVER I + C@   C=-IGNORE 0=
    IF   2DROP -1 UNLOOP EXIT   THEN LOOP   2DROP 0 ;
\ Caseinsensitive version of (MATCH)
: (MATCH)-IGNORE   >R R@ >FFA @ 3 AND 0= DUP IF   DROP R@ >NFA
@ @ OVER -   DUP 0< R@ >FFA @ 8 AND AND 0=   AND 0= DUP IF
DROP OVER R@ >NFA @ $@ CORA-IGNORE 0=   THEN THEN   R> SWAP ;
\ Install matchers
: CASE-INSENSITIVE   '(MATCH)-IGNORE >DFA @ '(MATCH) >DFA ! ;
: CASE-SENSITIVE '(MATCH) RESTORED ; DECIMAL
( DUMP ) REQUIRE B.  <HEX \ AvdH A1oct02
 : TO-PRINT DUP DUP BL < SWAP 7F > OR IF DROP [CHAR] . THEN ;
 : .CHARS  [CHAR] | EMIT 0 DO DUP I + C@ TO-PRINT EMIT LOOP
       [CHAR] | EMIT ;
 : BYTES 0 DO
          DUP I + C@ B.
          I 2 MOD IF SPACE THEN
      LOOP ;
:  DUMP   ( 2/0  DUMPS FROM ADDRESS-2 AMOUNT-1 BYTES)
    OVER + SWAP
    DO
      CR I H. ." : "
      I 0F AND DUP 5 2 */ SPACES 10 SWAP -
      I   OVER BYTES   OVER .CHARS   DROP DROP
    10 I 0F AND - +LOOP         CR
;    HEX>
( OOPS EDIT: ) \ AvdH A1oct12

"EDIT" PRESENT? 0= ?LEAVE-BLOCK
">SFA" PRESENT? 0= ?LEAVE-BLOCK

\ edit the following word
: EDIT: (WORD) FOUND >SFA @  1 MAX 255 MIN EDIT ;


\ edit the latest word, the one with the bug
: OOPS LATEST >SFA @ EDIT ;





( SUPER-QUAD SQ CONDENSED ) CF: ?PC
REQUIRE VIDEO-MODE
VARIABLE L
 : CONDENSED 34 VIDEO-MODE ;
 :  HEADER  CR DUP 2 + SWAP
    DO 3  SPACES ." SCR #" I 4 .R 54  SPACES LOOP ;
 : 1LINE  L @ OVER (LINE)  TYPE
   L @ 2 .R SPACE  L @ 16 + SWAP (LINE)  TYPE CR ;
  : SUPER-DUPE
    2 /MOD SWAP DROP 2 *
    DUP HEADER CR
    16 0 DO  I L ! DUP 1LINE
    LOOP  ;
 : SUPER-QUAD CONDENSED SUPER-DUPE 2 + SUPER-DUPE DROP ;
 : SQ SUPER-QUAD ;

( FOR-BLOCKS SHOW-BLOCK .BL Testing_of_block ) \ AvdH A1oct09
REQUIRE H.
: FOR-BLOCKS >R PREV @
    BEGIN DUP R@ EXECUTE +BUF WHILE REPEAT R> DROP DROP ;
: SHOW-BLOCK
    DUP STALEST @ = IF CR ." STALEST:" THEN
    DUP CR H.
    DUP @ IF
      ." #"  DUP ?
      CELL+ DUP @ IF ."     LOCKED" ELSE ." NOT LOCKED" THEN
      CELL+ &| EMIT 50 TYPE &| EMIT
    ELSE
      ." FREE " DROP
    THEN ;
: .BL 'SHOW-BLOCK >CFA FOR-BLOCKS ;

( DB-INSTALL DB-UNINSTALL Show_block_properties) \ AvdH A1oc08
REQUIRE ALIAS
: .CON &| EMIT   48 TYPE   &| EMIT ;
: .HEAD $@ " BLOCK NR IS " TYPE   .   $@
"LOCK IS " TYPE   . ;
: .SPECIAL
DUP PREV @ = IF &> ELSE
DUP STALEST @ = IF &< ELSE
BL THEN THEN EMIT ;
: DB 0 LIMIT FIRST DO CR DUP . 1+
I .SPECIAL .HEAD .CON B/BUF CELL+ CELL+ +LOOP
DROP KEY DROP .S ;
'BLOCK ALIAS BLOCK2
: NEW-BLOCK BLOCK2 DB ;
: DB-INSTALL 'NEW-BLOCK 'BLOCK 3 CELLS MOVE ;
: DB-UNINSTALL 'BLOCK2 'BLOCK 3 CELLS MOVE ;
( ?IM ?DN ?CD NEXTDEA DEA?   cracker_aux) \ AvdH A3apr25
( For the DEA : it IS immediate / it IS a denotation )
 : ?IM >FFA @ 4 AND ;     : ?DN >FFA @ 8 AND ;
\ For the DEA: it IS a plain code word.
: ?CD   DUP >CFA @ SWAP >PHA = ;
\ For DEA : leave next earlier DEA in current.
: NEXTDEA CURRENT @ BEGIN ( CR DUP ID.) 2DUP >LFA @ DUP 0= IF
2DROP 2DROP 0 EXIT THEN <> WHILE >LFA @ REPEAT SWAP DROP ;
\ For ADDRESS : it IS a proper `dea'
: DEA? NEXTDEA 0= 0= ;






( SEE KRAAK CRACK CRACK-CHAIN ) \ AvdH A2mar21
REQUIRE +THRU   REQUIRE ALIAS   REQUIRE @+
REQUIRE NEXTDEA
: BLOB DROP ; \ For DEPTH: print blob in correct color.
: H.. BASE @ >R HEX 0 .R R> BASE ! ;
: B.. H.. ;

1 7 +THRU
: SEE   CRACK ;
: KRAAK CRACK ;

\ Crack, starting with "name"
 : CRACK-FROM HEAD'
    BEGIN NEXTDEA DUP LATEST < WHILE DUP CRACKED REPEAT DROP ;


( cracker1 ) \ AvdH A3apr25
'' ALIAS HEAD'         \ If dea is intended rather than xt.
 : ?Q KEY? ABORT" Aborted by user" ; \ Emergency

 CREATE SELTAB 60 CELLS ALLOT   CREATE SELTOP SELTAB ,
 \ Put N in select table
 : T, SELTOP @ !  0 CELL+ SELTOP +!  ;


\ Look up N in ``SELTAB'', return DEA/N and "n IS present"
 : SEL@
       0 ( initialise flag)
       SELTOP @ SELTAB DO
         OVER I @ = IF 2DROP I CELL+ @ -1 THEN
       0 CELL+ CELL+  +LOOP ;

( cracker2  ) \ AvdH A1MAY17

\  Return CONTENT of the code field of "name".
 : CFOF HEAD' >CFA @ ;

: ID.. CFA> ID. ; ( cfa--. Print a words name )
: ID.+ @+ ID.. ; ( dip -- dip' Print a words name )
\ Print name of DEA, plainly.
: ID.-PLAIN  >NFA @ @+ TYPE ;

\ For DEA: it IS a header (heuristically).
 : HEAD? DUP >DFA @ SWAP  >PHA = ;


 : BY ( XT --. the CFA word is decompiled using : )
   T, ' T, ; ( a word from the input stream )
( cracker3 ) \ AvdH A0MAR30
 : CRACKED ( DEA--. Decompile a word from its DEA)
   \ Is it known as a mini cracker?
   DUP >CFA @ SEL@ IF EXECUTE ELSE
      DROP DUP ?CD IF CR ." Code definition : " ELSE
      CR ." Can't handle : " THEN ID.-PLAIN CR
   THEN ;
: CRACK  ( Use CRACK "ITEM" to decompile the word ITEM)
    (WORD) FOUND DUP 0= 11 ?ERROR CRACKED ;
( Crack item at POINTER. Leave incremented POINTER , GOON flag)
 : ITEM DUP @ SEL@ ( Something special ?)
     IF EXECUTE ( The special) ALIGNED ELSE
      DUP ?IM IF ." POSTPONE " THEN ID. CELL+
     THEN ;
: CRACK-CHAIN CR BEGIN ?Q DUP @ LIT (;) <>
    WHILE ITEM REPEAT DROP ;
( cracker4_types_by_low_level_code ) \ AvdH A3apr23
\ For all -words: 1/1 pointer before and after execution.


 ( It is done by examples of the defined words )
 : -co DUP CFA> >DFA @ CR H.. ."  CONSTANT " ID.. CR ;
      CFOF BL BY -co
 : -va DUP CFA> >DFA @ @ CR &( EMIT SPACE H.. ."  ) VARIABLE "
    ID.. CR ;              CFOF STALEST BY -va
 : -us DUP CFA> >DFA C@ CR B.. ."  USER " ID.. CR ;
      CFOF FENCE BY -us





( cracker5_types_by_high_level_code ) \ AvdH A3apr23
: -pc CR ." ;CODE plus code (suppressed)"
( DIRTY TRICK FOLLOWING :
make decompile pointer point to exit!)
    DROP 'TASK >DFA @ ;             ' (;CODE) BY -pc
: -hi CR ." : " DUP DUP ID.. CELL+ @ CRACK-CHAIN
    CR ." ;"  DUP
    ?IM IF ."  IMMEDIATE " THEN ?DN IF ."  ( DENOTATION)" THEN
    CR ;         CFOF TASK BY -hi

\ For ADDRESS: find DEA to which this address belongs.
 : FIND-HEAD ALIGNED ( Important for CREATE )
     BEGIN DUP HEAD? 0= WHILE 0 CELL+ - REPEAT ;
: TO-DOES   >DFA @ @ ; \ For DEA: hl POINTER in ``DOES>'' code.
 : -dd   DUP TO-DOES FIND-HEAD ." ( data ) " ID.. ID.. CR ;
      CFOF FORTH BY -dd
( cracker6_inline )     \ AvdH A3apr23

    : -con @+ H.. 1 BLOB ;
    : -dea @+ &' EMIT ID.-PLAIN 1 BLOB ;
: -lit CELL+ DUP @ DEA? IF -dea ELSE -con THEN SPACE ;
' LIT BY -lit
: -sk CELL+ CR ." [ " &" EMIT DUP @+ TYPE &" EMIT
    ."  ] DLITERAL" 2 BLOB SPACE @+ + 4 CELLS + ;
' SKIP BY -sk







( cracker7_branches_loops ) \ AvdH A1MAY17
: TARGET DUP 0 CELL+ - @ + ; ( IP -- TARGET OF CURRENT JUMP)
\ Print the name of ADDRESS , if it is a dea.
: .DEA? DUP DEA? IF ID.-PLAIN ELSE DROP ." ? " THEN ;
( IP -- IP ,print comment about current jump)
: -target DUP ." ( between " TARGET DUP 0 CELL+ - @ .DEA? SPACE
    @ .DEA? ."  ) " ;
: -br  CR @+ ID. ." [ " @+ H.. ."  , ] " -target ;
' BRANCH BY -br         ' 0BRANCH BY -br

: -do CR 2 BLOB ." DO " CELL+ CELL+ ;      ' (DO) BY -do
: -qdo CR 2 BLOB ." ?DO " CELL+ CELL+ ;    ' (?DO) BY -qdo
: -lo CR ." LOOP " CELL+ CELL+ ;           ' (LOOP) BY -lo
: -pl CR 1 BLOB ." +LOOP " CELL+ CELL+ ;   ' (+LOOP) BY -pl


( ASSEMBLER CODE END-CODE C; )  \ AvdH A0oct03
VOCABULARY ASSEMBLER IMMEDIATE
\ ISO standard words.
: CODE ?EXEC (WORD) (CREATE) [COMPILE] ASSEMBLER DSP@  ;
: ;CODE
?CSP   POSTPONE   (;CODE)   [COMPILE] [   [COMPILE] ASSEMBLER
; IMMEDIATE
: END-CODE ?EXEC ?CSP PREVIOUS ;
\ Non standard. A traditional alias for END-CODE .
: C; END-CODE ;






( ASSEMBLERi86-HIGH ) CF: ?32 \ AvdH A0oct17

REQUIRE ASSEMBLER  REQUIRE IVAR   REQUIRE +THRU
REQUIRE SWAP-DP

"ASSEMBLERi86" PRESENT? ?LEAVE-BLOCK
: ASSEMBLERi86 ;

ASSEMBLER DEFINITIONS
SWAP-DP
2 DUP +THRU
SWAP-DP
PREVIOUS DEFINITIONS



( ASSEMBLERi86 )  CF: \ AvdH A0oct17

REQUIRE ASSEMBLER  REQUIRE IVAR   REQUIRE +THRU

"ASSEMBLERi86" PRESENT? ?LEAVE-BLOCK
: ASSEMBLERi86 ;

ASSEMBLER DEFINITIONS
1 DUP +THRU
PREVIOUS DEFINITIONS






( --assembleri86_body )  \ AvdH A0oct03





 1 4 HEX +THRU  DECIMAL ( Common code , prelude)
 DECIMAL 12 DUP +THRU ( protected mode 16/32)

 5 9 HEX +THRU  DECIMAL ( Macro's, postlude)

\ 10 11 DUP +THRU  ( test )




( --assembler_postit_fixup_1 ) \ A2oct21 AvdH
 0 IVAR ISS ( Instruction start )
: X, , ;    ( Cell size du jour)

: DAT, X, ;
: REL, ISS @ - X, ;
: B, C, ;
( To be used when overruling, e.g. prefix)
: lsbyte, 0 100 UM/MOD SWAP C, ;
: W, lsbyte, lsbyte, DROP ;
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;
: RELL, ISS @ - L, ;  : RELW, ISS @ - W, ;
: RELB, ISS @ - B, ;
: SEG, W, ;


( --assembler_postit_fixup_2 ) \ A2oct21 AvdH
: <POST HERE ISS ! ;
 0 IVAR IDP
: <FIX HERE IDP ! ; : IHERE IDP @ ;
: C|, -1 IDP +! IHERE C@ OR IHERE C! ;  ( c.f. C, )
: C@+ COUNT ;  : C@- 1 - DUP C@ ; ( : C!+ >R R@ ! R> 1+ ;)
: POST, C@+ C, ;       : FIX| C@- C|, ;

: 1PI CREATE C, DOES> <POST POST, DROP ;
: 2PI CREATE C, C, DOES> <POST POST, POST, DROP ;
: 3PI CREATE C, C, C, DOES> <POST POST, POST, POST, DROP ;

: 1FI CREATE C, DOES> 1+ <FIX FIX| DROP ;
: 2FI CREATE C, C, DOES> 2 + <FIX FIX| FIX| DROP ;
: 3FI CREATE C, C, C, DOES> 3 + <FIX FIX| FIX| FIX| DROP ;

( --assembler_postit_fixup_3 ) \ A2oct21 AvdH
: SPLIT 0 100 UM/MOD ; ( To handle two bytes at once )
: SPLIT2 SPLIT SPLIT ; ( To handle three bytes at once )
( INCREMENT, OPCODE , COUNT -- )
: 1FAMILY, 0 DO DUP 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP SPLIT SWAP 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP SPLIT2 SWAP ROT 3PI OVER + LOOP DROP DROP ;

: 1FAMILY| 0 DO DUP 1FI OVER + LOOP DROP DROP ;
: 2FAMILY| 0 DO DUP SPLIT SWAP 2FI OVER + LOOP DROP DROP ;
: 3FAMILY| 0 DO DUP SPLIT2 SWAP ROT 3FI OVER + LOOP DROP DROP ;





( spare_1 )















( --assembler_macros_1 TO-PROT, TO-REAL, ) \ A2oct21 AvdH

: GET-CR0   MOV|CD, F| CR0| R| AX| ;
: PUT-CR0   MOV|CD, T| CR0| R| AX| ;
: TO-PROT,  GET-CR0  INC|X, AX|  PUT-CR0 ;
: TO-REAL,  GET-CR0  DEC|X, AX|  PUT-CR0 ;
: COPY-SEG  MOVXI, AX| ( DAT -- ) W,   MOVSW, T| DS| R| AX|
          MOVSW, T| ES| R| AX|   MOVSW, T| SS| R| AX|  ;








( --assembler_macros_2 NOP, CP, ) \ A2oct21 AvdH
: NOP, XCHG|AX, AX| ;
: CP, MOVTA, B| SWAP DUP , 1 + MOVFA, SWAP DUP , 1 + ;













( --assembler_macros_3 NEXT PUSH PUSH1 ) \ A2oct21 AvdH

: NEXT
     LODS, W1|
     MOV, W| F| AX'| R| BX|
     JMPO, D0| [BX]
 ;
: PUSH PUSH, AX| NEXT ;
: PUSH2 PUSH, DX| PUSH ;







( --assembler_macros_4 TO-PROT, TO-REAL, ) \ A2oct21 AvdH
\ These macro's are useful for protected mode under MSDOS
\ or for stand alone booting systems.
 7C0 CONSTANT SWITCH_DS 17C0 CONSTANT GDT_DS
 10 CONSTANT GDT_CS
: JMP-PROT, JMPFAR, HERE 4 + , GDT_CS W, ;
: JMP-REAL, JMPFAR, HERE 4 + SWITCH_DS 10 * - , SWITCH_DS W, ;
: REAL, JMP-REAL,  TO-REAL, ;
: PROT,  TO-PROT, JMP-PROT, ;







( spare_2 )














\
( --assembler_test_1 TEST-NEXT ) \ A2oct21 AvdH
( Tests applicable always )
  CODE TEST-NEXT NEXT  END-CODE
  " Testing next " TYPE
  TEST-NEXT
  " next Tested " TYPE









\
( --assembler_test_2 TEST-JUMP ) \ A2oct21 AvdH
?32  ( Test applicable to 32 bit mode)

CODE TEST-JUMP JMP-REAL, JMP-PROT, NEXT END-CODE

( CODE TEST-MORE TO-REAL,   TO-PROT, NEXT END-CODE           )
( CODE TEST-SWITCH   TO-REAL,   SWITCH_DS COPY-SEG   TO-PROT,)
( GDT_DS COPY-SEG   NEXT END-CODE                            )
DECIMAL







( --assembler_i86_electives ) \ A2oct21 AvdH

1 2 HEX +THRU DECIMAL ( Load either 16 or 32 bit stuff)
3 6 HEX +THRU  DECIMAL ( 8086 level instructions )
7 8 HEX +THRU DECIMAL ( 80386 level instructions )











( --assembler_i86_fixups_1 ) ?16 \ A2oct21 AvdH
( C7) 6 1FI MEM|  ( OVERRULES D0| BP| )
( 07) 1 0 8 1FAMILY| [BX+SI] [BX+DI] [BP+SI] [BP+DI]
[SI] [DI] [BP] [BX]

( Use remainder only with AS: prefix )
( For explanation see next screen, but unprimed.)
 ( 07) 1 0 8 1FAMILY| [AX]' [CX]' [DX]' [BX]'
[SIB]' [BP]' [SI]' [DI]'
( FF) C4 1FI MEM|'
( 0) C0 1PI MEM,'
( 3F) 04 1FI SIB|'
( 0) 0 1PI SIB,'
( F8) 04 1FI +0|'
( C0) 40 0 4 1FAMILY| +1*| +2*| +4*| +8*|

( --assembler_i86_fixups_1 ) ?32 \ A2oct21 AvdH
( FF) C4 1FI MEM| ( MEM| MEM, OVERRULES D0| SIB| SIB, BP| )
( 07) 05 1PI MEM, ( REQUIRED AFTER MEM|)
( COMBINES WITH D0| DB| DX| )
 ( 07) 1 0 8 1FAMILY| [AX] [CX] [DX] [BX] [SIB] [BP] [SI] [DI]

( 0) 0 1PI SIB, ( REQUIRED AFTER SIB|)
( F8) 04 1FI +0| ( OVERRULES +1*| SP'| )
( C0) 40 0 4 1FAMILY| +1*| +2*| +4*| +8*|
( Example : MOVI, X| D0| SIB|   SIB, AX| +1*| SI'|   117 X, )
( Example : MOVI, X| MEM|   MEM, +0| VAR X, 117 X, )

( Use remainder only with AS: prefix . See previous screen )
( C7) 6 1FI MEM|'
( 07) 1 0 8 1FAMILY| [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]'
[SI]' [DI]' [BP]' [BX]'
( --assembler_i86_fixups_2 opcodes ) \ A2oct21 AvdH
 8 0 4 1FAMILY| ES| CS| SS| DS|    1 6 2 1FAMILY, PUSHS, POPS,
 8 26 4 1FAMILY, ES:, CS:, SS:, DS:,
 8 27 4 1FAMILY, DAA, DAS, AAA, AAS,
 1 0 2 1FAMILY| B1| W1|    08 04 8 1FAMILY, ADDI|A, ORI|A,
 ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
 2 A0 2 1FAMILY, MOVTA, MOVFA,
1 0 2 1FAMILY| Y| N|   2 0 8 1FAMILY| O| C| Z| CZ| S| P| L| LE|
 70 1PI J,  ( As in J, L| Y| <CALC> S, )
 1 0 8 1FAMILY| AX| CX| DX| BX| SP| BP| SI| DI|
08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,   90 1PI XCHG|AX,
 ( C0) 40 00 4 1FAMILY| D0| DB| DW| R|
 ( 38) 08 00 8 1FAMILY| AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
 1 0 8 1FAMILY| AL| CL| DL| BL| AH| CH| DH| BH|


( --assembler_i86_opcodes_1 fixups ) \ A2oct21 AvdH
1 0 2 2FAMILY| B| W|   2 0 2 2FAMILY| F| T|
8 0 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
2 84 2 2FAMILY, TEST, XCHG,   0 88 2PI MOV,
( 00FD) 0 8C 2PI MOVSW,   ( 00FE) 0 8D 2PI LEA,
( IRR,egular) ( FF) 9A 1PI CALLFAR,  ( FE) A8 1PI TESTI|A,
 1 98 8 1FAMILY, CBW, CWD, IR2, WAIT, PUSHF, POPF, SAHF, LAHF,
( FE) 2 A4 6 1FAMILY, MOVS, CMPS, IR3, STOS, LODS, SCAS,
08 B0 2 1FAMILY, MOVRI, MOVXI,
8 C2 2 1FAMILY, RET+, RETFAR+,  8 C3 2 1FAMILY, RET,  RETFAR,
1 C4 2 1FAMILY, LES, LDS,  0 C6 2PI MOVI,   CD 1PI INT,
1 CC 4 1FAMILY, INT3, IRR, INTO, IRET,
1 D4 4 1FAMILY, AAM, AAD, IL1, XLAT,
1 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
2 E4 2 1FAMILY, INAP, OUTAP,  2 EC 2 1FAMILY, INAD, OUTAD,
1 E8 2 1FAMILY, CALL, JMP,  EA 1PI JMPFAR,  EB 1PI JMPS,
( --assembler_i86_opcodes_2 ) \ A2oct21 AvdH
1 F0 6 1FAMILY, LOCK, ILL, REP, REPZ, HLT, CMC,
1 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD, ( 38FE)
800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI,
 CMPI, 800 83 8 2FAMILY, ADDSI, IL3, ADCSI, SBBSI, IL4, SUBSI,
 IL5, CMPSI,   2 0 2 2FAMILY| 1| V|
800 D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, IL6, RAR,
800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,
00 F6 2PI TESTI, 800 FE 2 2FAMILY, INC, DEC,
( 38FF) 00 8F 2PI POP,  30 FE 2PI PUSH,
800 10FF 4 2FAMILY, CALLO, CALLFAROI, JMPO, JMPFAROI,





( spare_3 )















( --assembler_i386_opcodes_1 ) \ A2oct21
1 60 2 1FAMILY, PUSHA, POPA,
1 62 2 2FAMILY, BOUND, ARPL,
1 64 4 1FAMILY, FS:, GS:, OS:, AS:,
( OPERAND AND ADDRESS SIZE OVERWRITE)
66 1PI OS,   67 1PI AS,  ( Keep a short while)
2 68 2 2FAMILY, PUSHI|X, PUSHI|B,
2 69 2 2FAMILY, IMULI|X, IMULI|B,
2 6C 2 1FAMILY, INS, OUTS,   C8 1PI ENTER,   C9 1PI LEAVE,
1 F02 2 2FAMILY, LAR, LSL,
06 0F 2PI CLTS,    C0 20 0F 3PI MOV|CD,
1 0 4 2FAMILY| CR0| ~~~1 CR1| CR2|
1 0 8 2FAMILY| DR0| DR1| DR2| DR3| ~~~2 ~~~3 DR6| DR7|
80 0F 2PI J|X,         ( FFF08C) 00 90 0F 3PI SET,
100 0 2 1FAMILY| Y'| N'|
200 0 8 1FAMILY| O'| C'| Z'| CZ'| S'| P'| L'| LE'|
( --assembler_i386_opcodes_2 ) \ A2oct21 AvdH
1 A00F 3 2FAMILY, PUSH|FS, POP|FS, CPUID,
800 A30F 4 3FAMILY, BT, BTS, BTR, BTC,
800 A40F 2 3FAMILY, SHLDI, SHRDI,
800 A50F 2 3FAMILY, SHLD|C, SHRD|C,
1   B20F 4 3FAMILY, LSS, ~~~4 LFS, LGS,
800 B60F 2 3FAMILY, MOVZX|B, MOVSX|B,
800 B70F 2 3FAMILY, MOVZX|W, MOVSX|W,

1 A80F 2 2FAMILY, PUSH|GS, POP|GS,
800 00000F 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,
800 20BA0F 4 3FAMILY, BTI, BTSI, BTRI, BTCI,
800 000F0F 7 3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, ~~~5 LMSW,



( LOCATED LOCATE .SOURCEFIELD ) CF: \ AvdH A1sep26
">SFA" PRESENT? 0= ?LEAVE-BLOCK
\ Interpret a SOURCEFIELD heuristically.
: .SOURCEFIELD
    DUP 0 = IF "Belongs to the kernel" TYPE CR ELSE
    DUP 1000 U< IF LIST ELSE
    DUP TIB @ 40000 WITHIN IF "Typed in" TYPE CR ELSE
    50 - 200 TYPE THEN THEN THEN ;
\ Show the screen or text how SC is defined
: LOCATED FOUND DUP 0= 11 ?ERROR >SFA @ .SOURCEFIELD ;
\ Idem but string from input.
: LOCATE (WORD) LOCATED ;




( OS-IMPORT cdED ) CF: \ AvdH A2feb05
"SYSTEM" PRESENT? 0= ?LEAVE-BLOCK   REQUIRE +THRU
CREATE cmdbuf 1000 ALLOT
: OS-IMPORT ( sc "name-forth"  -- )
     CREATE , ,
     DOES>
     2@ cmdbuf $! BL cmdbuf $C+ \ Command
     ^J (PARSE) cmdbuf $+!      \ Append
     cmdbuf $@ SYSTEM          \  Execute
;
?LI
\ Change directory to SC .
: cdED   ZEN HERE HERE 12 LINOS ?ERRUR ;



( cat echo diff grep list ls make man rm cd cp ee l )
\ Unix like commands
CF:   REQUIRE +THRU
1 2 +THRU












( cat cp echo diff grep list ls make man rm   ee l unix) ?LI
REQUIRE OS-IMPORT ( and cdED )          \ AvdH A30325
"cat    "   OS-IMPORT cat
: cd (WORD) cdED ;      \ Change directory to "SC"
"cp     "   OS-IMPORT cp
"echo   "   OS-IMPORT echo
"diff   "   OS-IMPORT diff
"grep   "   OS-IMPORT grep
"list   "   OS-IMPORT list
"ls     "   OS-IMPORT ls
"make   "   OS-IMPORT make
"man    "   OS-IMPORT man
"rm  -i "   OS-IMPORT rm
"ee     "   OS-IMPORT ee  \ My favorite editor ee
"l      "   OS-IMPORT l   \ Very short personal command
""          OS-IMPORT !!
( cat cd cp echo edit list ls rm   ee l unix) ?PC \ AvdH A30325
REQUIRE OS-IMPORT
"TYPE   "   OS-IMPORT cat
"ECHO   "   OS-IMPORT echo
"MORE<  "   OS-IMPORT list
\ "LIST  "   OS-IMPORT list
"DIR    "   OS-IMPORT ls
"COPY   "   OS-IMPORT cp
"DEL    "   OS-IMPORT rm
"CD     "   OS-IMPORT cd
"EDIT   "   OS-IMPORT edit
"ee     "   OS-IMPORT ee  \ My favorite editor
"l      "   OS-IMPORT l   \ Very short personal command

""          OS-IMPORT !!
"A:" OS-IMPORT A:   "C:" OS-IMPORT C:   "D:" OS-IMPORT D:
( type ECHO MORE list DIR COPY DEL CD edit ) ?PC  \ AvdH A30325
REQUIRE OS-IMPORT
"TYPE   "   OS-IMPORT type
"ECHO   "   OS-IMPORT ECHO
"MORE<  "   OS-IMPORT MORE
"LIST   "   OS-IMPORT list  \ Not to conflict with: #BL LIST
"DIR    "   OS-IMPORT DIR
"COPY   "   OS-IMPORT COPY
"DEL    "   OS-IMPORT DEL
"CD     "   OS-IMPORT CD
"EDIT   "   OS-IMPORT edit  \ Not to conflict with: #BL EDIT
"RENAME "   OS-IMPORT RENAME
"A:" OS-IMPORT A:   "C:" OS-IMPORT C:   "D:" OS-IMPORT D:



( EDITOR ) CF:   ?PC    \ AvdH A1oct05
REQUIRE IVAR   REQUIRE +THRU
REQUIRE VIDEO-MODE   REQUIRE $
  1 12 +THRU


"BLOCK-INIT" PRESENT? 0= ?LEAVE-BLOCK

: DEVELOP   BLOCK-EXIT   1 WARNING !   2 BLOCK-INIT ;

DEVELOP





( protected_editor_stuff ) ?WI \ AvdH A1nov01
REQUIRE NEW-SEL  HEX
NEW-SEL CONSTANT VID
VID PAD GET-SEL
00 PAD     C!   20 PAD 1 + C!   00 PAD 2 + C!   80 PAD 3 + C!
0B PAD 4 + C!
VID PAD PUT-SEL
: LC@ L@ 0FF AND ;
: LC! OVER OVER L@ 0FF INVERT AND >R ROT R> OR ROT ROT L! ;

DECIMAL





( 16_bit_editor_stuff ) ?16 \ A1oct05
"VID" PRESENT? ?LEAVE-BLOCK  \ Already protected mode
HEX
B800 CONSTANT VID
: LC@ L@ FF AND ;
: LC! OVER OVER L@ FF00 AND >R ROT R> OR ROT ROT L! ;




DECIMAL





( 32_bit_editor_stuff ) ?32 \ A1oct05
"VID" PRESENT? ?LEAVE-BLOCK  \ Already protected mode
HEX
B800 CONSTANT VID
  HEX 0 CONSTANT CS_START
 : LC@ SWAP 10 * + CS_START - C@ ;
 : LC! SWAP 10 * + CS_START - C! ;




 DECIMAL




( BLUE FRAME NO-FRAME --editor_stuff ) \ AvdH A1nov01
"FRAME" PRESENT? ?LEAVE-BLOCK
HEX
17 CONSTANT BLUE           7 CONSTANT BLACK
VARIABLE COLOR

: VERTICAL-FRAME
   20 0 DO COLOR @ VID 0 I 50 * + 7F + LC! 2 +LOOP ;

: HORIZONTAL-FRAME
   80 1 DO COLOR @ VID 0  A00 + I + LC! 2 +LOOP ;

: FRAME   BLUE COLOR !   VERTICAL-FRAME   HORIZONTAL-FRAME ;

: NO-FRAME   BLACK COLOR !   VERTICAL-FRAME  HORIZONTAL-FRAME ;
DECIMAL
( FIG-GREEN FRAME NO-FRAME --editor_stuff ) \ AvdH A1nov01
"FRAME" PRESENT? ?LEAVE-BLOCK
HEX
2F CONSTANT FIG-GREEN    7 CONSTANT BLACK
VARIABLE COLOR

: ONE-LINE
  DUP 1+ SWAP 78 + SWAP DO COLOR @ VID I LC! 2 +LOOP ;

: SIXTEEN-LINES
   10 0 DO I A0 * ONE-LINE LOOP ;

: FRAME      FIG-GREEN COLOR !   SIXTEEN-LINES ;
: NO-FRAME   BLACK COLOR !       SIXTEEN-LINES ;

DECIMAL
( Screen_access ) HEX
050 CONSTANT VW   19 CONSTANT VH
VH VW * CONSTANT VL
: A-L SCR @ (LINE) ;
\ For INDEX leave the far ADDRES of that char on the screen
: VA DUP + VID SWAP ;
: >V SWAP 0 DO  ( sc offset - )
      OVER I + C@ 700 OR   OVER I + VA LC!
     LOOP DROP DROP ;
: V> ( BUF -LEN OFFSET- )  2DUP + >R
SWAP 0 DO OVER I +   OVER I + VA
 LC@ SWAP C!
LOOP DROP R> + ^J SWAP 2DROP ;
: PAGE PAD VL 2DUP BLANK 0 >V ;
: PG PAD 10 VW * 2DUP BLANK 0 >V ;
 DECIMAL
( editor ) HEX
: GET-S 10 0 DO I A-L I VW * >V LOOP ;
: PUT-S 10 0 DO I A-L I VW * V> LOOP UPDATE ;
: GET-L PAD VW ROT VW * >V ;
: PUT-L PAD VW ROT VW * V> ;
( 19 CONSTANT PB )   13 CONSTANT HW
: PUSH-D 10 SWAP DO I 1+ PUT-L I GET-L LOOP ;
: PUSH-DOWN VH SWAP DO I 1+ PUT-L I GET-L LOOP ;
: DEL-L DUP PUT-L VH  GET-L PUSH-DOWN ;
: PUSH-UP VH DO I PUT-L I 1+ GET-L -1 +LOOP ;
: UDL-L DUP PUSH-UP VH PUT-L GET-L ;
: PUSH-U HW DO I PUT-L I 1+ GET-L -1 +LOOP ;
: UFL-L DUP PUSH-U VH 1 - PUT-L GET-L ;
: DUP-L DUP DEL-L DUP UFL-L UDL-L ;

 DECIMAL
( editor CURSOR ) HEX
0 IVAR CURSOR
 : CURL CURSOR @ VW / ; : CP CURSOR @ VW MOD  ;
 : BIOS-CURSOR CURSOR @ VW /MOD 100 * + ;
: SET BIOS-CURSOR _ 0 200 10 BIOSN 2DROP ;
: MOVE-CURSOR   ( WORD STAR)
DUP ^D = IF  1 ELSE   DUP ^E = IF 0 VW - ELSE
DUP ^I = IF  8 ELSE   DUP ^M = IF VW CP - ELSE
DUP ^S = IF -1 ELSE   DUP ^X = IF VW ELSE
0    THEN THEN THEN THEN THEN THEN CURSOR +! ;
: DELSTORING DUP ^Y  = IF CURL DEL-L ELSE
      DUP ^P = IF CURL UDL-L ELSE
      DUP ^U = IF CURL UFL-L
      THEN THEN THEN ;
: DUP-L CURL DUP-L ;
  DECIMAL
( editor ) HEX
: PAD-B PAD VW BLANK  ;
: GET-R PAD VW CP - CURL VW * CP + >V ;
: GET-P PAD    CP   CURL VW *      >V ;
: PUT-R PAD VW CP - CURL VW * CP + V> ;
: RUBOUT-M PUT-R NEGATE CURSOR +! GET-R ;
: INSERT-M PUT-R DUP CURSOR +! GET-R NEGATE CURSOR +! ;
: RUBOUT 1 RUBOUT-M ; : INSERT 1 INSERT-M ;
: RUB-C PAD-B RUBOUT SET ;
: DEL-C 1 CURSOR +! RUB-C ;
: INS-C INSERT ;
: EOL PAD-B GET-R ;  : FOL PAD-B GET-P ;
: SPL DUP-L PAD-B PUT-R CURL 1+ GET-L EOL ;
: JOL PAD-B CURL 1+ PUT-L GET-R CURL 1+ PUSH-D ;
DECIMAL

( INSELETING JOINITTING )
HEX 0 IVAR I-MODE
: INSELETING
      DUP ^H = IF RUB-C ELSE
      DUP ^G = IF DEL-C ELSE
      DUP ^V = IF I-MODE 1 TOGGLE ELSE
      THEN THEN THEN ;
: JOINITTING
      DUP ^J = IF JOL   ELSE
      DUP ^O = IF SPL   ELSE THEN THEN ;
: EM-C EMIT 1 CURSOR +! ;
: PRINT ( C --C . Print it if printable)
  DUP 1F > IF DUP 7F < IF
  I-MODE @   IF INS-C THEN  DUP EM-C
  THEN THEN ;
DECIMAL
( Finding the next word A0MAY25 )
 : BL? VA LC@ $FF AND BL = ;
: BOUNDARY? BL? 0= OVER 1 - BL? AND ;
: >SEARCH BEGIN 1 + DUP BOUNDARY? UNTIL  ;
: <SEARCH BEGIN 1 - DUP BOUNDARY? UNTIL  ;
: NEXT-W CURSOR @ >SEARCH CURSOR ! ;
: BACK-W CURSOR @ <SEARCH CURSOR ! ;
: DEL-W  CURSOR @ NEXT-W SPL
   CURSOR ! SET SPL CURL 1+ DEL-L JOL ;
: VTYPE CURSOR @ >V ;
: GET-W VH 1 - PUT-L PAD VW -TRAILING 1+ ;
: UFW GET-W INSERT-M DROP GET-W VTYPE ;
: WORDING
  DUP ^F = IF NEXT-W ELSE   DUP ^A = IF BACK-W ELSE
  DUP ^T = IF DEL-W ELSE    DUP ^Z = IF UFW ELSE
THEN THEN THEN THEN ;
( DISPATCHER )    HEX
: AT-END VH 1 - VW * CURSOR ! SET ;
: DEBUG CURSOR @ AT-END .S CURSOR ! ;
: EXITING KEY 20 OR &q <> IF PUT-S THEN ;
: ROUTE BEGIN KEY
PRINT DELSTORING
INSELETING JOINITTING
WORDING MOVE-CURSOR SET
( DEBUG)
ESC = UNTIL ;
: E-S  ( EDIT CURRENT SCREEN )
1 I-MODE ! FRAME 0 CURSOR ! SET   PG
GET-S ROUTE EXITING  AT-END NO-FRAME ;
:  EDIT SCR ! E-S ;
: E-R 3 VIDEO-MODE EDIT ;
DECIMAL  ( Attempts at comamnd line editor)
( P ME Mini_editors ) \ AvdH A1oct05
( FORTH DIM iii/2 @ SHAPIN)
( Usage : to change line 1 of screen 3 )
( 3 SCR ! 1 P <CONTENT> )
HEX : TEXT HERE C/L 1+ BLANK WORD PAD C/L 1+ CMOVE ;
: LINE DUP FFF0 AND 17 ?ERROR SCR @ (LINE) DROP ;
: -MOVE LINE C/L 1- ( leave \n) CMOVE UPDATE ;
: P 1 TEXT PAD 1+ SWAP -MOVE ; DECIMAL
( Mini editor by retyping, Usage ME) <HEX
: EL LINE C/L 1 - BLANK ;
: GL PAD C/L  ACCEPT C/L 1- MIN >R LINE PAD SWAP R> MOVE ;
: OEPS SCR @ LIST "PROCEED?" TYPE KEY 20 OR
  &y <> 2000 ?ERROR  "GO" TYPE CR ;
: ME SCR ! OEPS 10 0 DO I EL I GL LOOP ;

HEX>
( CLEAN LO-S L-S C-S Handy_screen_tools ) \ AvdH A1oct05






: CLEAN BLOCK B/BUF OVER + SWAP DO
  I C@ 0= IF BL I C! THEN  LOOP ;

: L-S SCR @ LIST ;
: LO-S SCR @ LOAD ;
: C-S SWAP BLOCK SWAP BLOCK B/BUF CMOVE UPDATE FLUSH ;
: LIST' BASE @ 10 - 25 ?ERROR LIST ;
: LIST LIST' ;

( BIOSI VIDEO-MODE DISK-INIT ) ?PC \ AvdH A1oct05
HEX



: BIOSI SWAP 2SWAP SWAP BIOSN 2DROP ;  ( Ignore result)
: VIDEO-MODE  >R _ _ _ R> 10 BIOSN 2DROP ;
\ Reset DISK (C: 80 D: 81 A: 0 B: 1 etc.)
: DISK-INIT   _ _ 0 13 BIOSN 2DROP  ;






DECIMAL
( FARDUMP ) CF: ?PC \ AvdH A1oct
<HEX
:  FARDUMP   ( SEG ADDRESS AMOUNT - ..)
    OVER + SWAP FFF0 AND
    DO
      CR DUP H. I H. ." : "
      I
      10 0 DO
          2DUP I + L@ B.
          I 2 MOD IF SPACE THEN
      LOOP  [CHAR] | EMIT
      10 0 DO 2DUP I + L@ FF AND TO-PRINT EMIT LOOP
      [CHAR] | EMIT DROP
    10 +LOOP CR DROP
;    HEX>

( FREE-BLOCK REFRESH ) CF:   ?LI   \ AvdH A1oct03
\ Return the first BLOCK not written yet.
: FREE-BLOCK
    1 BEGIN DUP 'BLOCK CATCH 0= WHILE DROP 1+ REPEAT DROP ;
: REFRESH
    BLOCK-EXIT
    "ee BLOCKS.BLK;cp BLOCKS.BLK blocks.frt" SYSTEM
    2 BLOCK-INIT
    1 WARNING ! ;







( SET-MEMORY TEST-MEMORY MEM-SIZE ) ?PC ?32 \ AvdH A1oct17
DECIMAL  123456789 CONSTANT MAGIC  HEX
HERE MAGIC , 10,0000 - CONSTANT MM
VARIABLE (MEM-SIZE)   1000 (MEM-SIZE) ! \ Megabytes
: FAIL? DUP >R   @ MAGIC =   MAGIC R@ ! R> @ MAGIC <>  OR ;
: SET-MEMORY (MEM-SIZE) @ 2 DO
   I 14 LSHIFT  MM +
   DUP FAIL?   IF I (MEM-SIZE) ! LEAVE THEN
   \ ^M EMIT ." probing " I . 4 SPACES
   I SWAP !
LOOP ;
: TEST-MEMORY (MEM-SIZE) @ 2 DO
   I 14 LSHIFT  MM +  @ I <>  IF I (MEM-SIZE) ! LEAVE THEN
   LOOP ;
: MEM-SIZE SET-MEMORY TEST-MEMORY (MEM-SIZE) @ ;
DECIMAL
( SEL-DUMP dump_a_selector ) \ AvdH A1nov02
HEX 1 1 +THRU
: .CD 5 + C@  >R  R@  10 AND IF R@ 08 AND IF
." CODE SEGMENT: " R@ .CODE ELSE
." DATA SEGMENT: " R@ .DATA THEN  R@ 1+ .PRES
   ELSE ." SOME SORT OF GATE" THEN CR RDROP ;
: .LIMIT DUP @ 0FFFF AND  OVER 6 + C@ 0F AND  DH.
   ."  PARAGRAPHS OF " 6 + C@
80 AND IF ." A 4K PAGE"  ELSE ." ONE BYTE" THEN  CR  ;
: .BASE >R   R@ 2 + @ 0FFFF AND  R@ 4 + @ 0FF AND
R@ 6 + @ 0FF00 AND OR   RDROP
 ." LINEAR BASE ADDRESS : " DH. CR ;
: .ST 6 + C@ 40 AND IF ." 32-BITS " ELSE ." 16-BITS" THEN
 CR ;
: SEL-DUMP DUP .PRIV   DUP .ST   DUP .CD   DUP .BASE   .LIMIT ;
DECIMAL
( dump_a_selector ) \ AvdH A1nov02

: .PRIV 5 + C@ 5 RSHIFT 3 AND ." PRIVILEGE LEVEL: " . CR ;
: NOT 0= IF ." NOT " THEN ;
: .CODE
  DUP 1 AND NOT ." ACCESSED, "     DUP 2 AND NOT ." READABLE, "
  DUP 4 AND NOT ." CONFORMING, "   DUP 8 AND NOT ." Ppppppp, "
  DROP ;
: .DATA
  DUP 1 AND NOT ." ACCESSED, "     DUP 2 AND NOT ." WRITABLE, "
  DUP 4 AND NOT ." EXPAND DOWN, "  DUP 8 AND NOT ." Ppppppp, "
  DROP ;
: .PRES
  DUP 80 AND NOT ." PRESENT. "
  DROP ;

( **************communication with stand alone hd ************)















( --hd_LBA utils_for_stand_alone_disk ) CF: ?PC ?16
REQUIRE ASSEMBLERi86   REQUIRE DISK-INIT   REQUIRE +THRU
( backup and restore a stand alone hard disk system to floppy )
( run from a booted floppy system )
( this is for a 16 bit system, because the assembly assumes )
( so. If you have a 32 bit system, there are easier ways.   )
: --hd_LBA ;
256 CONSTANT #BLOCKS
1 5 +THRU DECIMAL







( --hd_LBA READ-BLOCK WRITE-BLOCK RW-BUFFER ) ?16 ?PC HEX
HERE DUP 3 + 3 INVERT AND SWAP - ALLOT HERE B/BUF ALLOT
CONSTANT RW-BUFFER
CREATE PARAM-BLOCK 10 C, 0 C, 2 , ( 2 sectors/block)
RW-BUFFER , 0 , HERE 2 CELLS ALLOT 0 , 0 , CONSTANT BL#
 : R\W-BLOCK  ASSEMBLER
  OS:, POP|X, AX|   OS:, ADD, W| R| AX'| AX|
  OS:, MOVFA, W1| BL# W,   PUSH|X, SI|
  MOVXI, BX| ( FUNCTION CODE ) W,   MOVXI, DX| 0080 W,
  MOVXI, SI| PARAM-BLOCK SWITCH_DS 10 * -  W,
  TO-REAL, SWITCH_DS COPY-SEG
 XCHG|AX, BX| INT, 13 B, PUSHF, POP|X, BX|
 TO-PROT, GDT_DS COPY-SEG
  POP|X, SI|   PUSH|X, BX|  NEXT ; PREVIOUS
CODE READ-BLOCK 4200 R\W-BLOCK  END-CODE
CODE WRITE-BLOCK 4300 R\W-BLOCK  END-CODE     DECIMAL
\ --hd_LBA (HWD) (HRD) (FRD) (FWD) \ ?16 ?PC HEX
?16 ?PC

( Write the default buffer to hard disk at 32-bit POSITION)
: (HWD) SWAP WRITE-BLOCK 1 AND . ;
( Read the default buffer from hard disk at 32-bit POSITION)
: (HRD) SWAP READ-BLOCK 1 AND . ;
DECIMAL
( As R\W but relative, counting from ``OFFSET''. )
: RELR\W SWAP OFFSET @ + SWAP R\W ;
( Read absolute BLOCK from floppy into default buffer.)
: (FRD) RW-BUFFER SWAP 1 R\W ;
( Write absolute BLOCK to floppy from default buffer.)
: (FWD) RW-BUFFER SWAP 0 R\W ;


( --hd_LBA SWAP-FLOPPY ) ?PC ?16 \ AvdH A1oct07
?PC ?16
DECIMAL
( Prompt for floppy change, plus whatever needed.)
: SWAP-FLOPPY   0 WARNING !
  "Swap floppy and press a key" TYPE
  KEY 32 OR &q = IF ABORT THEN   0 '(FRD) CATCH DROP
  EMPTY-BUFFERS   0 DISK-INIT   80 DISK-INIT ;

\ copy a hd system, was written to a floppy to the
\  hard disk. Done by a Forth booted from another floppy.)
\ DBS : default boot system, first 1400 K of hd.
\ Prompt for empty floppy. Save from hard disk BLOCK
\ (a 32-bit number) and 1400K following to the floppy.
: BACKUP>FLOPPY  SWAP-FLOPPY
  1400 0 DO 2DUP I S>D D+ (HRD) I (FWD) LOOP 2DROP ;
( --hd_LBA BACKUP-KERNEL BACKUP-BLOCKS ) CF: ?PC ?16
REQUIRE --hd_LBA
\ Prompt for floppy created with ``BACKUP>FLOPPY''
\ Restore to hard disk ``DBS'' 1400 K from floppy.
: RESTORE<FLOPPY SWAP-FLOPPY
1400 0 DO I (FRD) 2DUP I S>D D+ (HWD) LOOP 2DROP ;

\ Copy the kernel (first 64K of ``DBS'' to raw floppy.
: BACKUP-KERNEL  SWAP-FLOPPY 64 0 DO I S>D (HRD) I (FWD) LOOP ;

\ Copy the BLOCKS (#BLOCKS at 64K of ``DBS'') to BLOCKS.BLK.
: BACKUP-BLOCKS
#BLOCKS 0 DO I 64 + S>D (HRD) RW-BUFFER I 0 RELR\W LOOP ;



( INSTALL-KERNEL RESTORE-BLOCKS ) CF: ?PC ?16
REQUIRE --hd_LBA

\ Copy the kernel (first 64K of ``DBS'') from raw floppy.
: INSTALL-KERNEL SWAP-FLOPPY 64 0 DO I (FRD) I S>D (HWD) LOOP ;

\ Copy the BLOCKS (#BLOCKS at 64K of ``DBS'') from BLOCKS.BLK.
: RESTORE-BLOCKS
#BLOCKS 0 DO RW-BUFFER I 1 RELR\W I 64 + S>D (HWD) LOOP ;







( SECTORS/TRACK #HEADS ) CF: \ AvdH A1oct10
1 2 +THRU
EXIT
Get information directly after booting aboot the hard disk
The BIOS puts this in the real memory area, and it is
no longer available if e.g. from DOS sufficiently large
programs have been run.
Directly after a Forth has booted, it is safe,
of course.







( SECTORS/TRACK #HEADS ) ?16 ?PC \ AvdH A1oct10
HEX
\ See Ralph Brown's table 03196
\ Far address of interrupt 41
: (int41) 0 041 4 * ;
\ Far address of HD 1 table.
: (hd1) (int41) CELL+ L@ (int41) L@ ;
\ Number of heads on hard disk one.
(hd1) 2 + L@ 0FF AND CONSTANT #HEADS
\ Sectors per track for hard disk one.
(hd1) 0E + L@ 0FF AND CONSTANT SECTORS/TRACK
DECIMAL




( SECTORS/TRACK #HEADS ) ?32 ?PC \ AvdH A1oct10
HEX
\ See Ralph Brown's table 03196
\ Address of interrupt 41
: (int41) 041 4 * ;
\ Address of HD 1 table.
: (hd1) (int41) @ FFFF AND (int41) @ 10 RSHIFT 4 LSHIFT + ;
\ Number of heads on hard disk one.
(hd1) 2 + C@ CONSTANT #HEADS
\ Sectors per track for hard disk one.
(hd1) 0E + C@ CONSTANT SECTORS/TRACK
DECIMAL




( INSTALL-FORTH-ON-HD ) CF: ?32 \ AvdH A1oct11
REQUIRE +THRU   REQUIRE NEW-IF
\ Elective and configuration screen
\ Define and overrule this for manual installation
CREATE #BLOCKS 256 ,
HEX F8 CONSTANT MEDIA-ID \ For hard disk.
\ ?? CONSTANT #HEADS   ?? CONSTANT SECTORS/TRACK
\ ?? CONSTANT MEM-SIZE
1 6 +THRU
INSTALL-FORTH-ON-HD
EXIT
Re-installs a sector-and-track ciforth to a hard disk (or
floppy). This is a user utility, so it can be run for other
type ciforth's. But then it only explains to the user what is
going wrong.

( --disclaimer_INSTALL_FORTH_ON_HD ) ?HD \ AvdH A1oct11

\ This utility is intended for sectors & track installations
\ so for floppy compatible hard disks.

." This is the wrong utility for your Forth ." CR
." Try INSTALL-KERNEL and RESTORE-BLOCKS instead." CR

QUIT







( --disclaimer_INSTALL_FORTH_ON_HD ) ?FD \ AvdH A1oct11
: stop? KEY &Y <>  IF
   ." ABONDANNED! " CR QUIT
THEN ;
CR
." You are about to install Forth on your hard disk" CR
." making it unusable for any other purpose." CR
." No dual boot system, no partition or even a master" CR
." boot record will remain, and on some computers e.g. " CR
." Compaq some data will be permanently lost." CR
." Go on at your own risk. No guarantees." CR CR
." GO ON ??   Y/N" CR

stop?
CR ." Analysing..." CR

( --disclaimer_INSTALL_FORTH_ON_HD ) ?FD \ AvdH A1oct11
REQUIRE #HEADS    REQUIRE B. DECIMAL
CR ." The number of heads on your hard disk is reported: "
#HEADS DUP . B. &H EMIT
CR ." The number of sectors/track is reported: "
SECTORS/TRACK DUP . B. &H EMIT
CR ." (If this is incorrect, you have to configure manually)"
CR CR ." Do you believe this? Y/N" CR

stop?


CR ." Analysing..." CR



( --disclaimer_INSTALL_FORTH_ON_HD ) ?FD ?32 \ AvdH A1oct17
REQUIRE MEM-SIZE   REQUIRE B.   REQUIRE NEW-IF
DECIMAL
CR ." The amount of Megabytes on your system is probed as: "
MEM-SIZE DUP . HEX 0 .R &H EMIT DECIMAL
CR ." (If this is incorrect, you have to configure manually)"
CR CR ." Do you believe this? Y/N" CR

stop?
HEX
\ Now patch the systems memory size.
\ This takes effect after booting only.

: PATCH-MEM BM -  \ Addres to which start of buffer corresponds
 5 0 DO MEM-SIZE 14 LSHIFT EM - OVER I CELLS + +ORIGIN +!
LOOP  MEM-SIZE 14 LSHIFT SWAP 'EM >DFA + ! ;
( PATCH-NEW-FORTH PATCH-THIS-FORTH ) ?FD ?32 \ AvdH A1oct12
REQUIRE #HEADS HEX
\ The SIZE of Forth (kernel +blocks) in blocks.
: SIZE-FORTH   OFFSET @   #BLOCKS @  + ;
\ What we need then
CREATE buffer SIZE-FORTH B/BUF * ALLOT
\ Now patch into the boot record the hard disk dimensions
\ And into the access definition of this Forth
: PATCH-NEW-FORTH   \ Overlapping 32 bits stores, big endian!
   MEDIA-ID          buffer 15 + C!
   SECTORS/TRACK     buffer 18 + !     \ Actually 16 bit
   #HEADS            buffer 1A + ! ;   \ Actually 16 bit
: PATCH-THIS-FORTH   \ This Forth now accesses hard disk
   80                DRIVE C!
   SECTORS/TRACK     DRIVE 1+ C!
   #HEADS            DRIVE 2 + C! ;
( INSTALL-FORTH-ON-HD ) ?FD \ AvdH A1oct11
: show ^M EMIT ." BLOCK" 4 .R 5 SPACES ;
\ Read into BUFFER absolute block NUMBER , leave NEXT buffer.
: read+ OFFSET @ -   BLOCK     OVER B/BUF MOVE   B/BUF + ;
\ Cannot read directly into the buffer because it is above 1 Mb
: READ-FORTH  buffer SIZE-FORTH 0 DO I show I read+ LOOP DROP ;
\ Cannot write directly into the buffer because it is above 1 M
: write+   OFFSET @ - (BUFFER) CELL+ CELL+   OVER SWAP   B/BUF
   MOVE     UPDATE B/BUF + ;
: WRITE-FORTH
   buffer SIZE-FORTH 0 DO I show I write+ LOOP DROP ;
: ready ." Press the reset button, to boot your new FORTH"
    CR ;
: INSTALL-FORTH-ON-HD  DRIVE @ 0 WARNING ! READ-FORTH
buffer PATCH-MEM PATCH-NEW-FORTH PATCH-THIS-FORTH WRITE-FORTH
EMPTY-BUFFERS 1 WARNING ! DRIVE ! ready ;
( --hd_driver_standalone_) CF: ?PC ?32 ?HD HEX

  1 8 +THRU













( 250 Redefine R\W to accomdate larger addresses. A1may05AH)
VOCABULARY SYS ONLY FORTH
 DP @ LOW-DP @  DP ! LOW-DP ! SYS DEFINITIONS
( 247 248 ) THRU HEX
: NEW-COLD
EMPTY-BUFFERS   FIRST STALEST !   FIRST PREV !
0 CELLS +ORIGIN DUP CELL+ @  40 CELLS CMOVE
1<>64 LOAD-ALL (ABORT) ;
'NEW-COLD 'COLD 3 CELLS MOVE
DP @ LOW-DP @  DP ! LOW-DP ! PREVIOUS DEFINITIONS DECIMAL
  SYS
: SAVE-SYSTEM
   U0 @   0 +ORIGIN   40 CELLS CMOVE ( Save user variables)
   STORE-ALL ;
PREVIOUS

( 247: Redefine R\W to accomdate larger addresses. A1aug05AH)
HEX
( The screen BUFFER is apparently virgin)
: (FREE?) B/BUF 0 DO DUP I + C@ &v - IF UNLOOP DROP 0 EXIT THEN
   LOOP DROP -1 ;
8,F000 CONSTANT RW-BUFFER    ( A 64 kBYTE BUFFER)
( Switch between reading 64K and 1 K)
: 1<>64 LBAPAR 2 + 82 TOGGLE ;

( All: address block -- addres' block' And : SIZE 64K)
: READ++   1<>64 DUP RW-BUFFER SWAP 1 R\W
OVER RW-BUFFER SWAP 1,0000 MOVE   40,0001,0000. D+ 1<>64 ;
: WRITE++  1<>64 OVER RW-BUFFER 1,0000 MOVE
  DUP RW-BUFFER SWAP 0 R\W 40,0001,0000. D+ 1<>64 ;

DECIMAL
( 248: Words to load and store a system A1aug31 AH)
HEX
: MUD? B/BUF - (FREE?) ;  ( limit -- loaded/stored empty block)
( All: address block -- addres' block' )
: LOAD-MID BEGIN OVER A,0000 < WHILE READ++ REPEAT ;
: LOAD-HIGH BEGIN OVER B/BUF - MUD? 0= WHILE READ++ REPEAT ;
( Load and store all of the system, 9,0000 .. A,000 is scratch)
: LOAD-ALL
2,7C00 OFFSET @ 40 +  LOAD-MID  ( Skip kernel, stack)
SWAP DROP 10,0000 SWAP LOAD-HIGH   2DROP ;

: STORE-MID BEGIN OVER A,0000 < WHILE WRITE++ REPEAT ;
: STORE-HIGH BEGIN OVER HERE < WHILE WRITE++ REPEAT ;
: STORE-ALL  ( Store to next chunk, inc. kernel, stack )
0,7C00 OFFSET @ 40 - STORE-MID
SWAP DROP 10,0000 SWAP STORE-HIGH 2DROP ;  DECIMAL
( hd_driver1 PATCH-CHUNK ) ?PC ?32 HEX \ AH&CH A1sep01
SYS  0800,0000 B/BUF / CONSTANT CHUNK-SIZE  ( BLOCKS PER CHUNK)
: CHUNK-START  OFFSET @ 40 - ;
: CURRENT-CHUNK
CHUNK-START  CHUNK-SIZE / -6 +ORIGIN OVER SWAP C! ;
( -- offset and bl# where to patch OFFSET )
: OFFSET-/MOD 'OFFSET >DFA @ +ORIGIN 7C00 - B/BUF /MOD ;
: CHECK CURRENT-CHUNK = 0D ?ERROR ;
: PATCH-CHUNK
     OFFSET-/MOD >R DROP
     RW-BUFFER  CURRENT-CHUNK CHUNK-SIZE * R@ + 1 R\W
     CHUNK-SIZE * DUP 40 + OFFSET-/MOD DROP RW-BUFFER + !
     RW-BUFFER  SWAP R> + 0 R\W
; DECIMAL


( hd_driver2 WIPE-HD )  ?PC ?32 \ AH A1may3
: WIPE-BUFFER RW-BUFFER B/BUF &v FILL ;
: WRITE-BUFFER RW-BUFFER SWAP OFFSET @ + 64 - 0 R\W ;
: CHECK-RANGE 589 64 + CHUNK-SIZE WITHIN 0= 13 ?ERROR ;
: SHOW DUP 100 MOD 0= IF . ^M EMIT ELSE DROP THEN ;
: WIPE-RANGE  DUP CHECK-RANGE SWAP DUP 1- CHECK-RANGE SWAP
    WIPE-BUFFER
    DO I SHOW I WRITE-BUFFER LOOP ;
HEX 130,3BBF CONSTANT LAST-BLOCK DECIMAL
CHUNK-SIZE 8 * CONSTANT FIRST-BLOCK
: doit  CHUNK-SIZE 589 64 + CR WIPE-RANGE ;
: WIPE-HD WIPE-BUFFER FIRST-BLOCK
     BEGIN RW-BUFFER OVER 0 R\W DISK-ERROR 1 AND 0= WHILE
      DUP SHOW 1+ REPEAT DROP ;


( hd_driver3 FIRST-FREE ) ?PC ?32 \ AH A1may3
REQUIRE BIN-SEARCH
: FREE? RW-BUFFER SWAP 1 R\W
DISK-ERROR 1 AND   RW-BUFFER (FREE?)   OR ;
: NON-FREE? FREE? INVERT ;
: FIRST-FREE  ( -- FIRST FREE BLOCK IN BACKUP AREA)
FIRST-BLOCK LAST-BLOCK 'NON-FREE? BIN-SEARCH 1 + ;
: FNTB ( First free in current chunk)
  CHUNK-START CHUNK-SIZE OVER + 'NON-FREE? BIN-SEARCH 1+ ;
: SAVE-COMMENT 200 BLOCK FIRST-FREE 0 R\W ;






( hd_driver4 BLMOVE BLMOVE-FAST BACKUP ) ?PC ?32 \ AH A1sep01
HEX
: BLMOVE 0 DO  ( as MOVE for blocks.)
  SWAP RW-BUFFER OVER 1 R\W 1+   SWAP RW-BUFFER OVER 0 R\W 1+
  DUP SHOW KEY? IF UNLOOP EXIT THEN  LOOP . . ;
: BLMOVE-FAST  ( as MOVE for blocks, ONLY MULTIPLES OF 64K.)
 1<>64 0 DO
       SWAP RW-BUFFER OVER 1 R\W 40 +
       SWAP RW-BUFFER OVER 0 R\W 40 +
       DUP SHOW KEY? IF UNLOOP EXIT THEN
40 +LOOP . . 1<>64 ;
: BACKUP ( BACKUP THE CURRENT CHUNK TO PRISTINE DISK )
   CHUNK-START FNTB OVER - FIRST-FREE SWAP BLMOVE-FAST ;
: SAVE-CHUNK DUP CHECK
 CURRENT-CHUNK CHUNK-SIZE * OVER CHUNK-SIZE * CHUNK-SIZE
BLMOVE-FAST PATCH-CHUNK ; DECIMAL
( hd_driver5 I-INSPECT AH) ?PC ?32 HEX \ AH A1sep01
: ASCII? DUP BL 7F WITHIN SWAP ^J = OR ;
( SC contains all ``ASCII'' )
: ALL-ASCII? OVER + SWAP DO I C@ DUP ASCII? 0=
IF DROP UNLOOP 0 EXIT THEN 0= IF UNLOOP -1 EXIT THEN LOOP -1 ;
: INSPECT RW-BUFFER SWAP 1 R\W
  RW-BUFFER B/BUF 2DUP
 ALL-ASCII? IF TYPE ELSE DROP 100 DUMP THEN ;
: I-INSPECT
BEGIN KEY >R
R@ ^E = IF 1 - THEN R@ ^X = IF 1 + THEN
R@ ^R = IF 8 - THEN R@ ^C = IF 8 + THEN
DUP . DUP INSPECT R> 20 OR &q = UNTIL ;
DECIMAL  PREVIOUS


( **************Working ciforth examples *********************)
      EXIT

This contains examples and benchmarks.












( AA : Nesting_benchmark )
 : A0 ;
 : A1 A0 A0 ;   : A2 A1 A1 ;    : A3 A2 A2 ;
 : A4 A3 A3 ;   : A5 A4 A4 ;    : A6 A5 A5 ;
 : A7 A6 A6 ;   : A8 A7 A7 ;    : A9 A8 A8 ;
 : AA A9 A9 ;

: TEST 0 DO AA LOOP ;
: Q 0 DO 10000 TEST LOOP ;







( DO-PRIME-ISO GILBREATH's_benchmark_MMI_mar17__VERSIE_#2)
 8190 CONSTANT SIZE
 CREATE FLAGS      SIZE ALLOT

 : DO-PRIME-ISO
     FLAGS SIZE 1 FILL
     0 ( 0 COUNT ) SIZE 0
     DO FLAGS I + C@
      IF I DUP + 3 +  ( DUP . )
         DUP I +
         BEGIN DUP SIZE <
         WHILE 0 OVER FLAGS +  C!  OVER + REPEAT
         DROP DROP 1+
      THEN
     LOOP ;

( ERATOSTHENES SIEVE by_multiple_batches ) \ AvdH A1oct04
( Adaptations from CP/M : VARIABLE )
REQUIRE +THRU
1 5 +THRU












(       ERATOSTHENES >1< Variables - A. van der Horst         )
 ( User specified variables:)
VARIABLE CH/L  80 CH/L  !  ( Characters per line)
VARIABLE LN/P  24 LN/P  ! ( Lines per page)
VARIABLE PAUSE  1 PAUSE ! ( Boolean: pause between pages)

 ( Other:)
 6250 CONSTANT SIZE ( 16 numbers pro byte)
 CREATE FLAGS      SIZE ALLOT
 FLAGS SIZE + CONSTANT END-FLAGS
 VARIABLE LIM     ( part of FLAGS considered)
 VARIABLE C#      VARIABLE L#  ( char and line counter)
 VARIABLE THOUSANDS ( #  thousand to be sieved)
 VARIABLE MILS      ( Contains current thousand)
 VARIABLE MANTISSA  ( The current thousands is to be printed)

(       ERATOSTHENES >2< Pretty printing - A. van der Horst   )
 : FFEED  PAUSE @ IF CR ." KEY FOR NEXT SCREEN" KEY DROP THEN
     12 EMIT CR ." ERATOSTHENES SIEVE -- PRIMES LESS THAN"
     THOUSANDS @ 5 .R ."  000" CR 2 L# ! 1 MANTISSA ! ;
 : ?P ( LENGTH -- . , give FF if LENGTH lines don't fat)
      DUP L# +! L# @ LN/P @ > IF FFEED L# +! ELSE DROP THEN ;
 : NEWLINE  ( Start at a new line, maybe with a mantissa)
     1 ?P CR ( Checks first)
     MANTISSA @ IF MILS @ 6 .R ELSE 6 SPACES THEN
     6 C# !   0 MANTISSA ! ;
 : ?L ( LENGTH -- . , give LF if LENGTH char's don't fit)
      DUP C# +! C# @ CH/L @ >
      IF NEWLINE C# +! ELSE DROP THEN ;
 : .P   4 ?L SPACE 0 <# # # # #> TYPE ;
 : INIT-P  FFEED NEWLINE  ;

(       ERATOSTHENES >3< Bit manipulation - A. van der Horst  )
   HEX
 : NOT   0FF XOR ( N -- N  FLIP ALL BITS OF N) ;
CREATE S-MASK
    01 C, 02 C, 04 C, 08 C, 10 C, 20 C, 40 C, 80 C,
CREATE C-MASK 01 NOT C, 02 NOT C, 04 NOT C, 08 NOT C,
           10 NOT C, 20 NOT C, 40 NOT C, 80 NOT C,
 : INIT-T   FLAGS SIZE 0FF FILL ; ( Preset to 'prime')
 DECIMAL
 : 8/MOD   0 8 UM/MOD ; ( May be redefined in assembler )
 : CLEAR-B ( BIT# --  clears the specified bit)
         8/MOD FLAGS + SWAP  ( Address in flags table)
         C-MASK + C@         ( Get mask)
         OVER C@ AND SWAP C! ( Clear the bit)  ;


(       ERATOSTHENES >4< Bit manipulation - A. van der Horst  )
 : SET-B ( BIT# --  sets the specified bit)
         8/MOD FLAGS + SWAP  ( Address in flags table)
         S-MASK + C@         ( Get mask)
         OVER C@ OR SWAP C!  ( Store with bit set)  ;
 : TEST-B ( BIT# -- FLAG  Gets a FLAG testable by IF)
         8/MOD FLAGS + C@ SWAP  ( Get flag)
         S-MASK + C@ AND        ( Result: =0 or #0)     ;
 : FLIP ( PRIME,START -- .  , marks multiples of PRIME as
      (  non prime starting with START)
         BEGIN  DUP LIM @ U<  WHILE
                DUP CLEAR-B  OVER +
         REPEAT   DROP DROP ;
 : CHECK SIZE 16 UM* 1000 UM/MOD  THOUSANDS @ U< IF
       ." INCREASE SIZE " ABORT ELSE DROP DROP THEN ;

(       ERATOSTHENES >5< Main program - A. van der Horst     )
 : BATCH1 ( First batch of 500 numbers)
      500 1 ( Only odd numbers)
     DO I TEST-B
      IF I DUP + 1 + DUP .P ( get prime number)
         I FLIP THEN ( Mark multiple as non-prime)
     LOOP ;
 : BATCH ( OFFSET --  every  following batch )
      500 0
      DO DUP I + TEST-B IF I DUP + 1 + .P THEN
      LOOP DROP ;
 : SIEVE  ( N --  Makes list of primes <N*1000 )
     DUP THOUSANDS !   DUP CHECK   500 * LIM !  0 MILS !
     INIT-T INIT-P 2 .P BATCH1
     THOUSANDS @ 1
     ?DO I MILS !  1 MANTISSA !  NEWLINE I 500 * BATCH LOOP ;
( BIN-SEARCH binary_search_variables ) \ AvdH
VARIABLE IMIN  \ IMIN 'COMP EXECUTE is always TRUE
VARIABLE IMAX  \ IX 'COMP EXECUTE is always FALSE for IX>IMAX
VARIABLE COMP \ Execution token of comparison word.
: BIN-SEARCH    COMP !  IMAX ! IMIN !
    BEGIN       \ Loop variant IMAX - IMIN
      IMIN @ IMAX @ ( .S) <> WHILE
      IMAX @ IMIN @ + 1+ 2 /   ( -- ihalf )
      DUP COMP @ EXECUTE IF
         ( ihalf) IMIN !
      ELSE
         ( ihalf) 1- IMAX !
      THEN
    REPEAT
IMIN @ 1+ ; ( This example with variables works the same as )
( the stack version in utilities.)
( binary_search_test )
REQUIRE BIN-SEARCH
: <100 100 < ;
"Expect 100, a few times plus 99 101 :" TYPE
-1000 +1000 '<100 BIN-SEARCH .
+90 +110 '<100 BIN-SEARCH .
+90 +111 '<100 BIN-SEARCH .
+91 +110 '<100 BIN-SEARCH .
+99 +101 '<100 BIN-SEARCH .
+99 +100 '<100 BIN-SEARCH .
+100 +101 '<100 BIN-SEARCH .
+100 +100 '<100 BIN-SEARCH .
+99 +99 '<100 BIN-SEARCH .
+101 +101 '<100 BIN-SEARCH .


( tak1 )
VARIABLE X      VARIABLE Y      VARIABLE Z

: tak
X @ Y @ Z @ >R >R >R
Z ! Y ! X !
    X @ Y @ > 0= IF
      Z @
    ELSE
      X @ 1 - Y @ Z @ RECURSE
      Y @ 1 - Z @ X @ RECURSE
      Z @ 1 - X @ Y @ RECURSE
      RECURSE
    THEN
R> R> R> Z ! Y ! X !
;
( tak2 , Using poor man's locals on the data stack)
: Z "R@ @" EVALUATE ; IMMEDIATE
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE
: tak
     DSP@  >R  \ X . Y . Z . CR
     X Y > 0= IF
       Z
     ELSE
       X 1 - Y Z RECURSE
       Y 1 - Z X RECURSE
       Z 1 - X Y RECURSE
       RECURSE
     THEN
     RDROP \ Drop frame pointer.
     >R DROP DROP DROP R> ; \ Discard input, leave result
( tak3 )
: Z "R@ @" EVALUATE ; IMMEDIATE
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE
: tak SWAP ROT
     DSP@  >R  \ X . Y . Z . CR
     X Y > 0= IF
       Z
     ELSE
       X 1 - Y Z SWAP ROT  RECURSE
       Y 1 - Z X SWAP ROT  RECURSE
       Z 1 - X Y SWAP ROT  RECURSE
       SWAP ROT  RECURSE
     THEN   RDROP \ Drop frame pointer.
     >R DROP DROP DROP R> ; \ Discard input, leave result
: tak SWAP ROT tak ;
( tak4 )
: Z "R@ @" EVALUATE ; IMMEDIATE
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE
: tak SWAP ROT
     DSP@  >R  \ X . Y . Z . CR
     X Y > 0= IF
       Z
     ELSE
       Y X Z 1 - RECURSE
       X Z Y 1 - RECURSE
       Z Y X 1 - RECURSE
       RECURSE
     THEN   RDROP \ Drop frame pointer.
     >R DROP DROP DROP R> ; \ Discard input, leave result
: tak SWAP ROT tak ;
( tak5 )
: Z "R@ @" EVALUATE ; IMMEDIATE
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE
: tak SWAP ROT
     DSP@  >R  \ X . Y . Z . CR
     X Y > 0= IF
       Z
     ELSE
       Y X Z 1 - RECURSE  >R DROP DROP DROP R>
       X Z Y 1 - RECURSE  >R DROP DROP DROP R>
       Z Y X 1 - RECURSE  >R DROP DROP DROP R>
       RECURSE >R DROP DROP DROP R>
     THEN   RDROP \ Drop frame pointer.
; \ Discard input, leave result
: tak SWAP ROT tak >R DROP DROP DROP R> ;
( tak6 )
: X "R@ @" EVALUATE ; IMMEDIATE
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE
: Z "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE
: tak
     2DUP < 0= IF
       >R >R DUP R> SWAP R> SWAP
     ELSE
       DSP@  >R  \ X . Y . Z . CR
       Y X Z 1 - RECURSE  >R DROP DROP DROP R>
       X Z Y 1 - RECURSE  >R DROP DROP DROP R>
       Z Y X 1 - RECURSE  >R DROP DROP DROP R>
       RECURSE >R DROP DROP DROP R>
       RDROP \ Drop frame pointer.
     THEN ;
: tak SWAP ROT tak >R DROP DROP DROP R> ;
( tak7 )
: kat
    2DUP < 0= IF
       >R >R DUP R> SWAP R> SWAP
    ELSE
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       R> R> R> SWAP ROT
       RECURSE
      >R DROP DROP DROP R>
    THEN ;
: tak SWAP ROT kat >R DROP DROP DROP R> ;



( tak8 , Only stacks and still comprehensible)
: 2PICK  ">R >R DUP R> SWAP R> SWAP" EVALUATE ; IMMEDIATE
: tak ; ( Forward)
: kat
    2DUP < 0= IF 2PICK
    ELSE
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       R> R> R> tak
    THEN ;
: tak' SWAP ROT kat >R DROP DROP DROP R> ;
'tak' >DFA @ 'tak >DFA ! ( solve forward reference)

: q MARK-TIME 18 12 6 tak . ELAPSED ;

( tak9 , Only stacks and still comprehensible)
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)
: tak SWAP ROT kat' >R DROP DROP DROP R> ( 3 NIP's ) ;
\ Like ``tak'' but arguments in reverse order and not consumed
: kat
    2DUP < IF
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       ROT 1 - RECURSE  >R 1+
       R> R> R> tak
    ELSE
      >R >R DUP R> SWAP R> SWAP \ 2 PICK
    THEN ;
'kat >DFA @ 'kat' >DFA ! ( solve forward reference)
: q MARK-TIME 18 12 6 tak . ELAPSED ;
( tak10 Only stacks, already less comprehensible)
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)
: tak SWAP ROT 1+ kat' >R DROP DROP DROP R> ( 3 NIP's ) ;
\ For Z Y X   --- return Z Y X tak(X-1,Y,Z)
: kat
   1-
   2DUP < IF
       ROT RECURSE >R   ROT RECURSE >R   ROT RECURSE >R
       1+
       R> R> R> tak
   ELSE  1+
       >R >R DUP R> SWAP R> SWAP \ 2 PICK
   THEN ;
'kat >DFA @ 'kat' >DFA ! ( solve forward reference)
: q MARK-TIME 18 12 6 tak . ELAPSED ;
( tak11 Only stacks, less and less comprehensible)
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)
: tak SWAP ROT 1+ kat' >R DROP DROP DROP R> ( 3 NIP's ) ;
\ For Z Y X   --- return Z Y X tak(X-1,Y,Z)
: kat
   2DUP 1- < IF
       1-
       ROT RECURSE >R   ROT RECURSE >R   ROT RECURSE >R
       1+
       R> R> R> tak
   ELSE
       >R >R DUP R> SWAP R> SWAP \ 2 PICK
   THEN ;
'kat >DFA @ 'kat' >DFA ! ( solve forward reference)
: q MARK-TIME 18 12 6 tak . ELAPSED ;
( tak12 using 3SWAP NIP and PICK )
: PICK 1+ CELLS DSP@ + @ ;
: NIP SWAP DROP ;
: 3SWAP SWAP ROT ; \ Reverse top 3 elements.
: tak' ;  ( Forward reference)
\ Auxiliary: For Z Y X   --- return Z Y X tak(X-1,Y,Z)
: kat
   2DUP 1- < IF
       1-  ROT RECURSE >R  ROT RECURSE >R  ROT RECURSE >R  1+
       R> R> R> tak'
   ELSE
       2 PICK
   THEN ;
: tak 3SWAP 1+ kat NIP NIP NIP ;
'tak >DFA @ 'tak' >DFA ! ( Solve forward reference)

( Binair zoeken, commentaar ) EXIT
( BIN-SEARCH : n IMIN, n IMAX, xt COMP -- n IRES )
Gebruikt een orakel met het execution token `COMP'.
`COMP' heeft het stack effect ( IT -- flag) , waar
vlag typisch betekent dat IT kleiner of gelijk een
bepaalde waarde is. `COMP' moet WAAR teruggeven
voor `IMIN' en afnemen tussen `IMIN' en `IMAX', en
het moet voor alle waarden tot en met IMAX
aangeroepen mogen worden. BIN-SEARCH vindt de
laatste index `IT' tussen `IMIN' en `IMAX'
(inclusief) waarvoor `COMP' WAAR teruggeeft.
Voorbeeld: -100 100 ' 0< BIN-SEARCH .
-1 OK



( BIN-SEARCH : n IMIN, n IMAX, xt COMP -- n IRES              )
VARIABLE COMP \ Execution token van het "Orakel"
VARIABLE IMIN \ IMIN 'COMP EXECUTE is altijd waar.
VARIABLE IMAX \ Als IX 'COMP EXECUTE waar is,
            \ dan ook voor IY mits IY > IX
: BIN-SEARCH   COMP !  IMAX ! IMIN !
    BEGIN     \ Loop variant IMAX - IMIN
      IMIN @ IMAX @ <> WHILE
      IMAX @ IMIN @ + 1+ 2 /   ( -- ihalf )
      DUP COMP @ EXECUTE IF
         ( ihalf) IMIN !
      ELSE
         ( ihalf) 1- IMAX !
      THEN
    REPEAT
IMIN @ ;
 ( *************stand alone unshaven***************  )
( tools for stand alone system, to be reimplemented )














( CHUNK 5.0 20001sep01 AH  `SCREEN 200'                       )
\   Second conception
\   Start of this chunk DECIMAL : 1084434













( CRB compare_blocks_content ) ?32 ?PC HEX
B/BUF 10 / CONSTANT SZ
VARIABLE B
: ?TERMINATE KEY 20 OR &q = IF QUIT THEN ;
: RWBUF0 0 BLOCK 0 LOCK ;   : RWBUF1 1 BLOCK 1 LOCK ;
: CB B/BUF 0 DO
   OVER I +   OVER I +   SZ CORA
IF  CR B . I . OVER I + SZ DUMP   DUP I + SZ DUMP  ?TERMINATE
THEN   SZ +LOOP 2DROP ;
( Compare block FROM to FROM1 over RANGE )
: read SWAP 1 R\W ;   : write SWAP 0 R\W ;
: CRB     0 DO I 10 .R ^M EMIT
I B ! OVER I + RWBUF0 read DUP I + RWBUF1 read
RWBUF0 RWBUF1 CB LOOP 2DROP ;
( Restore FROM length RANGE to booting )
: restore 0 DO DUP I + RWBUF0 read I RWBUF0 write LOOP ;
( R\W-floppy ) ?PC ?32 HEX
( must only be used for low buffers.) 2 CONSTANT SEC/BLK
: SEC-RW  ( function address bl# -- )
24 /MOD   SWAP   12 /MOD   >R   SWAP   100 *   +   1+
R>   100 *   0 +   13 ^ BIOSO   1
AND   DISK-ERROR   +!   DROP DROP DROP DROP ;
\ : SEC-RW CR . . . ;
: R\W-floppy       0 DISK-ERROR ! ^
0BRANCH [ 10 , ] 201  BRANCH  [ 8 , ] 301  ^
SWAP SEC/BLK * SEC/BLK OVER + SWAP
DO SWAP   ^ 2DUP   I   SEC-RW   200 +   ^ SWAP
LOOP DROP   DROP   DISK-ERROR   @   ?DUP
0BRANCH [ 38 , ] 0< 0BRANCH [ 10 , ] 9
BRANCH  [ 8 , ] 8 0   PREV   @   !   ?ERROR
;  : R-floppy 1 R\W-floppy ;  : W-floppy 0 R\W-floppy ;

( backup restore ) ?PC ?32 \ AvdH A1sep01 208
\ Copy the currently booted chunk to free space on the hd,
\ flanked by comment blocks (block 200, fill beforehand).
: backup SAVE-COMMENT BACKUP SAVE-COMMENT ;
\ From START restore LEN blocks to ``DBS''.
: restore CHUNK-START SWAP BLMOVE ;










( PROBABLY OBSOLETE Alternative for COLD A1may04 AH)  HEX
DP @ LOW-DP @  DP ! LOW-DP ! \ Compile to low memory.
VOCABULARY SYS    SYS DEFINITIONS
: NEW-COLD
EMPTY-BUFFERS   FIRST STALEST !   FIRST PREV !
'SYS 'FORTH >WID >LFA !
0 CELLS +ORIGIN DUP CELL+ @  40 CELLS CMOVE   ABORT ;
DP @ LOW-DP @  DP ! LOW-DP ! \ Compile to high memory.
'NEW-COLD 'COLD 3 CELLS CMOVE
PREVIOUS DEFINITIONS
DECIMAL
( This has been loaded since chunk 0.5 and must not be
reloaded.)
I guess that only screen 248 should be used from now on.


( Redefine R\W to accomdate larger addresses. A1aug05AH)
HEX
( The screen BUFFER is apparently virgin)
: (FREE?) B/BUF 0 DO DUP I + C@ &v - IF UNLOOP DROP 0 EXIT THEN
   LOOP DROP -1 ;
8,F000 CONSTANT RW-BUFFER    ( A 64 kBYTE BUFFER)
( Switch between reading 64K and 1 K)
: 1<>64 LBAPAR 2 + 82 TOGGLE ;

( All: address block -- addres' block' And : SIZE 64K)
: READ++   1<>64 DUP RW-BUFFER SWAP 1 R\W
OVER RW-BUFFER SWAP 1,0000 MOVE   40,0001,0000. D+ 1<>64 ;
: WRITE++  1<>64 OVER RW-BUFFER 1,0000 MOVE
  DUP RW-BUFFER SWAP 0 R\W 40,0001,0000. D+ 1<>64 ;

DECIMAL
( Words to load and store a system A1aug31 AH)
HEX
: MUD? B/BUF - (FREE?) ;  ( limit -- loaded/stored empty block)
( All: address block -- addres' block' )
: LOAD-MID BEGIN OVER A,0000 < WHILE READ++ REPEAT ;
: LOAD-HIGH BEGIN OVER B/BUF - MUD? 0= WHILE READ++ REPEAT ;
( Load and store all of the system, 9,0000 .. A,000 is scratch)
: LOAD-ALL
2,7C00 OFFSET @ 40 +  LOAD-MID  ( Skip kernel, stack)
SWAP DROP 10,0000 SWAP LOAD-HIGH   2DROP ;

: STORE-MID BEGIN OVER A,0000 < WHILE WRITE++ REPEAT ;
: STORE-HIGH BEGIN OVER HERE < WHILE WRITE++ REPEAT ;
: STORE-ALL  ( Store to next chunk, inc. kernel, stack )
0,7C00 OFFSET @ 40 - STORE-MID
SWAP DROP 10,0000 SWAP STORE-HIGH 2DROP ;  DECIMAL
( Get source in core, edit to core 2001MAY11 AH) DECIMAL
SYS
CREATE CIFORTH HERE 0 ,
HERE OFFSET @ LOAD-HIGH 2DROP
589 B/BUF * DUP ALLOT SWAP !  \ Patch length
: WBLK' B/BUF * CIFORTH CELL+ +  B/BUF MOVE ;
: RBLK' B/BUF * CIFORTH CELL+ +  SWAP B/BUF MOVE ;
HEX \ Redefine R\W to accomodate blocks in SOURCE
: BLOCK' >R   PREV   @   DUP   @   R@   -
0BRANCH [ 5C , ] +BUF   0=
0BRANCH [ 24 , ] DROP   R@   BUFFER   DUP   CELL+   CELL+
R@   NOOP RBLK' DUP  @   R@   -   0=
0BRANCH [ -50 , ] DUP   PREV   !   RDROP   CELL+   CELL+   ;
: UPDATE' PREV @ DUP CELL+ CELL+ SWAP @ WBLK' ;
' UPDATE' 'UPDATE 3 CELLS MOVE
' BLOCK' 'BLOCK 3 CELLS MOVE
( Redefine R\W to accomodate larger addresses. A1may05AH)
VOCABULARY SYS ONLY FORTH
 DP @ LOW-DP @  DP ! LOW-DP ! SYS DEFINITIONS
247 248 THRU HEX
: NEW-COLD
EMPTY-BUFFERS   FIRST STALEST !   FIRST PREV !
0 CELLS +ORIGIN DUP CELL+ @  40 CELLS CMOVE
1<>64 LOAD-ALL (ABORT) ;
'NEW-COLD 'COLD 3 CELLS MOVE
DP @ LOW-DP @  DP ! LOW-DP ! PREVIOUS DEFINITIONS DECIMAL
  SYS
: SAVE-SYSTEM
   U0 @   0 +ORIGIN   40 CELLS CMOVE ( Save user variables)
   STORE-ALL ;
PREVIOUS

( Redefine R\W to accomdate larger addresses. A1may05AH)
DP @ LOW-DP @  DP ! LOW-DP ! SYS DEFINITIONS HEX
: ABORT-NEW
S0   @   DSP!   DECIMAL   ?STACK
CR .CPU "ciforth stand alone " TYPE
-8 +ORIGIN
COUNT 0. D.R &c EMIT COUNT 0. D.R &. EMIT COUNT 0. D.R BL EMIT
DROP CR
ONLY POSTPONE FORTH DEFINITIONS QUIT ;

'ABORT-NEW 'ABORT  3 CELLS MOVE
DP @ LOW-DP @  DP ! LOW-DP ! PREVIOUS DEFINITIONS DECIMAL




 ( ************** ciforth unshaven ***************************
      EXIT
Attempts include :
 tryout of high level code before building into kernel
 code that has later been improved
 code that doesn't work
 dutch versions that do work

Do not use any of this without an assesment.
They have not been properly REQUIRE 'd too.






( Test_screen_for_reverse-engineering_BLK )
BLK ?
"BLK ?" EVALUATE













( $I $S Reference_implementation ) \ AvdH A0APR04
( cs, del - Index Index is the first place del is found in the
string else 0. It is assumed del cannot be a valid addr )
: $I   OVER 0= IF DROP DROP DROP 0 ELSE  DUP >R
     ROT ROT OVER + SWAP DO
     DUP I C@ = IF DROP I LEAVE THEN
   LOOP R> OVER = IF DROP 0 THEN
THEN ;
( cs, del -- cs2 , cs1 )  ( Splits the text at the del )
   ( in two, if not present, cs2 is a null string )
: $S
  >R OVER OVER R> $I  DUP IF
    >R OVER R@ SWAP - ( Length before delimiter )
    SWAP OVER - 1 - ( Length after delimiter)
    R> 1+ SWAP
 ELSE ( DROP 0) 0 THEN  2SWAP ;
( This has the effect as ?ERROR ) ?LI
( But counting back from 100 )
: LINUX-ERROR 100 OVER - ?ERROR ;
: IOCTL 54 LINOS LINUX-ERROR ;
0 IVAR TERMIO 60 ALLOT
HEX 5401 CONSTANT TCGETS
HEX 5402 CONSTANT TCSETS
8 2 OR CONSTANT RAWIO
: getit 0 TCGETS TERMIO IOCTL ;
: setit 0 TCSETS TERMIO IOCTL ;
( Set the terminal length to len and toggle the )
( raw byte with b )  ( len b -- )
: tc TERMIO 3 CELLS + SWAP TOGGLE
    TERMIO 4 CELLS + 1 + 6 + C!
     setit ;
DECIMAL  getit
(_Testing_of_TERMIO_raw_character_input_)
?LI
3 CONSTANT read
( expect one key and retain it.)
: KEY2 1 RAWIO tc
     0 DSP@
    0 SWAP 1 read LINOS DROP
     1 RAWIO tc
;
( expect zero keys and retain the count.)
: KEY?2
    0 RAWIO tc
    0 DSP@
    0 SWAP 1 read LINOS SWAP DROP
    1 RAWIO tc
;
( A reverse engineering BLK and SOURCE-ID )
: BLK'
    IN @ FIRST LIMIT WITHIN
    SRC 2@ - 1024 = AND
    IF SRC @ 2 CELLS - @ ELSE 0 THEN
    BLK !
    BLK
;   BLK' ?   " BLK' ?" EVALUATE
: SOURCE-ID
   SRC @
   DUP TIB @ = IF DROP 0 ELSE
   DUP 7 - "FiLeBuF" CORA IF ( Leave it) ELSE
   DROP -1 THEN THEN ;
SOURCE-ID ? "SOURCE-ID ?" EVALUATE


( Print registers as passed from DPMI to INT 31H fc 0300 )
: REG-SET? REG-SET + @ H. SPACE ;
: .REG CREATE DUP , CELL+ DOES> DUP BODY> ID. @ REG-SET? ;
 0
.REG .DIL      .REG .DIH      .REG .SIL       .REG .SIH
.REG .BPL      .REG .BPH      .REG .RSL       .REG .RSH
.REG .BXL      .REG .BXH      .REG .CXL       .REG .CXH
.REG .DXL      .REG .DXH      .REG .AXL       .REG .AXH
.REG .PSW
.REG .ES       .REG .DS       .REG .FS        .REG .GS
.REG .IP       .REG .CS       .REG .SP        .REG .SS
: .REGS  .DIL .DIH .SIL .SIH  CR  .BPL .BPH .RSL .RSH CR
       .AXL .AXH .BXL .BXH  CR  .CXL .CXH .DXL .DXH CR ;
: .SYSS   .ES .DS .FS .GS CR   .IP .CS .SP .SS .PSW CR   ;
: .ALL .REGS .SYSS ;

( Experiment with DPMI testing jumps to 32 bit code. ) ?WI
REQUIRE ASSEMBLERi86 REQUIRE GET-SEL REQUIRE PC
PC GET-ALIAS CONSTANT NEW       \ Create a new segment that
NEW PAD GET-SEL                 \ differs from current code
PAD TOGGLE-32                   \ segment in being 32 bit.
PAD TOGGLE-CODE  ( Alias always return data segments )
NEW PAD PUT-SEL

\ To prove that we can actually use the 32 bits code segment.
CODE CRASH    \ It doesn't crash. But pushes a 32 bit EAX !
JMPFAR, HERE 4 + , NEW ,
PUSH|X, AX|
JMPFAR, HERE 6 + , 0 , PC ,
NEXT END-CODE
\ This pushes return information correct for a 16 bit segment
CODE CRASH2   AS:, CALLFAR, HERE 4 + , 0 , PC ,  NEXT END-CODE
( Experiment with GDT etc.) HEX
( 32 K GDT AT 0001.8000 ) 2800 CONSTANT GDT-SEGMENT
7FFF IVAR GDT 2.8000 SWAP , ,
7C0 CONSTANT CODE-SEGMENT ( The same for real and prot)
CODE-SEGMENT  10 * CONSTANT CODE-START
: GDT! GDT-SEGMENT SWAP L! ;   1800 CONSTANT DATA-SEGMENT
: CODE! CODE-SEGMENT + GDT! ;   : DATA! DATA-SEGMENT + GDT! ;
: PREPARE-CS
  FFFF 0 CODE!   CODE-START 2 CODE!
  9A00 4 CODE!   008F 6 CODE! ;
: PREPARE-DS
  FFFF 0 DATA! CODE-START 2 DATA!
  9200 4 DATA! 008F 6 DATA! ;
 CODE LOAD-GDT CLI, 0F C, 01 C, 10 C, MEM| GDT MEM,
NEXT END-CODE DECIMAL

( Experiment with GDT etc.) HEX
7C8 CONSTANT CS-32 ( 32 BITS CODE SEGMENT)
10  CONSTANT DS-32 ( 32 BITS DATA SEGMENT)
: CS32! CS-32 + GDT! ;   : DS32! DS-32 + GDT! ;
: PREPARE-CS32
  FFFF 0 CS32!   CODE-START 2 CS32!
  9A00 4 CS32!   00CF 6 CS32! ;
: PREPARE-DS32
  FFFF 0 DS32!   CODE-START 2 DS32!
  9200 4 DS32!   00CF 6 DS32! ;
PREPARE-DS PREPARE-CS PREPARE-CS32 PREPARE-DS32

DECIMAL



( Experimenting with drive parameters ) HEX
B/BUF SEC/BLK / CONSTANT SEC-LEN
0 IVAR RW-BUFFER B/BUF ALLOT
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,
HERE 1 - SEC-LEN / , SEC-LEN , 7C0 ,
( We use the two l.s. bytes of 64 bit number)
            1 , 0 , 0 , 0 ,
 CODE WRITE-SYSTEM
  PUSH|X, SI|
  MOVXI, AX| 4300 W,
  MOVXI, DX| 0080 W,
  MOVXI, SI| PARAM-BLOCK W,
  INT, 13  B,
  POP|X, SI|
  PUSHF,
  NEXT END-CODE            DECIMAL
( Experiment: switch to protected mode and back )
  90 LOAD 41 42 THRU HEX     LOAD-GDT
CODE TO-PROT1
  CLI, PUSHS, DS|  TO-PROT,
    JMPFAR, HERE 4 + MEM, CS-32 SEG,
    MOVI, W| R| AX| DS-32 , 0 ,
    MOVSW, T| DS| R| AX|
    MOVI, W| R| AX| 61 , 0 ,
    XCHG|AX, AX| XCHG|AX, AX| XCHG|AX, AX| XCHG|AX, AX|
    MOVFA, B| B.0400 SWAP , ,
    JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG,
 TO-REAL, STI, POPS, DS|  OS, PUSH|X, AX|
 NEXT END-CODE   DECIMAL



( Switch to protected mode and back timing test )
CODE TO-PROT2
  CLI, TO-PROT,
    JMPFAR, HERE 4 + MEM, CS-32 SEG,
    JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG,
 TO-REAL, STI,
 NEXT END-CODE   DECIMAL
CODE TO-PROT3
  CLI, TO-PROT,   TO-REAL, STI,
 NEXT END-CODE   DECIMAL

: TEST2 0 DO TO-PROT2 LOOP ;
: TEST3 0 DO TO-PROT3 LOOP ;
: Q2 0 DO 10000 TEST2 LOOP ;
: Q3 0 DO 10000 TEST3 LOOP ;

( Switch to protected mode and back replacement for DOCOL )
  90 LOAD 41 42 THRU HEX     LOAD-GDT
CODE NEW-DOCOL
 (  JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG, )
 (  TO-REAL,) STI,   CLI,   ( TO-PROT,)
(  JMPFAR, HERE 4 + MEM, CS-32 SEG, )
  LEA, BP'| DB| [BP] -2 B,
  MOV, W| F| SI'| DB| [BP] 0 B,
  LEA, SI'| DB| [DI] 2 B,
 NEXT END-CODE   DECIMAL
 : A0 ; ' A0 >CFA @ CONSTANT 'DOCOL
CODE X JMP,  ' NEW-DOCOL >DFA 'DOCOL 3 + - , END-CODE
 CODE SWITCH  ' X >DFA 'DOCOL  CP, CP, CP, DROP DROP
CLI,  ( TO-PROT, MOVXI, AX| DATA-SEGMENT MEM,
 MOVSW, T| DS| R| AX|  MOVSW, T| ES| R| AX|  MOVSW, T|
SS| R| AX| ) NEXT END-CODE  DECIMAL
( Switch to protected mode and back replacement for DOCOL )
CODE NEW-BIOS
  POP|X, AX|   MOVFA, B| HERE 0 ,    ( PATCH THE INTERRUPT #)
  POP|X, DX|  POP|X, CX|  POP|X, BX|  POP|X, DI|
PUSH|X, SI|   PUSH|X, BP| ( TO-REAL,) STI, XCHG|AX, DI|
  INT, HERE SWAP ! 0 C, ( PATCH THE ADDRESS WHERE TO PATCH )
  PUSHF, POP|X, DI|   ( SAVE FLAGS BEFORE THEY ARE DESTROYED)
  XCHG|AX, SI| ( FREE AX)  CLI,  ( TO-PROT,)
  ( NOW ALL REGISTERS ARE TIED UP EXCEPT ax| [!])
POP|X, BP|  POP|X, AX|  XCHG|AX, SI| ( RESTORE FORTH REGISTERS)
PUSH|X, AX|    PUSH|X, BX|    PUSH|X, CX|    PUSH|X, DX|
PUSH|X, DI|    NEXT END-CODE
CODE HLT HLT, END-CODE
: PATCH-BIOS 'NEW-BIOS >DFA 'BIOSO >CFA ! ;
: PATCH PATCH-BIOS SWITCH ;

( Dutch ) CR ." TAARTEN AUTOMATISERING DOOR DRS HENK" CR
 ." EEN VOORBEELD UIT BRODIE"     CR
 ." TYPE HELP VOOR DE GLOSSARY"  CR
 0 IVAR TAARTEN     0 IVAR DIEP-VRIES
 : HELP CR ." GLOSSARY:" CR ." BAK-TAART"
      CR ." EET-TAART" CR ." VRIES-IN" CR ." ONTDOOI"
      CR ." START" CR ." STATUS" CR ;
 : START 0 TAARTEN ! 0 DIEP-VRIES ! ;
 : BAK-TAART 1 TAARTEN +! ;
 : EET-TAART TAARTEN @ DUP
       IF -1 TAARTEN +! CR ." DANKJEWEL !" CR ELSE
       CR ." WELKE TAART ?" CR DROP THEN ;
 : VRIES-IN TAARTEN @ DIEP-VRIES +! 0 TAARTEN ! ;
 : ONTDOOI DIEP-VRIES @ TAARTEN +! 0 DIEP-VRIES ! ;
 : STATUS CR ." AANTAL AANWEZIGE TAARTEN: " TAARTEN ?
   CR ." EN NOG " DIEP-VRIES ? ." IN DE DIEP VRIES " ;
( Experimenting with drive parameters ) HEX
B/BUF SEC/BLK / CONSTANT SEC-LEN
CREATE RW-BUFFER B/BUF ALLOT
CREATE PARAM-BLOCK -2 ALLOT 10 C, 0 C,
HERE 1 - SEC-LEN / , SEC-LEN , 7C0 ,
( We use the two l.s. bytes of 64 bit number)
            1 , 0 , 0 , 0 ,
 CODE WRITE-SYSTEM
  PUSH|X, SI|
  MOVXI, AX| 4300 W,
  MOVXI, DX| 0080 W,
  MOVXI, SI| PARAM-BLOCK W,
  INT, 13  B,
  POP|X, SI|
  PUSHF,
  NEXT END-CODE            DECIMAL
( TEST OF HARD DISK ) ?16 HEX
CODE READ-BLOCK2 4200 R\W-BLOCK  END-CODE  ( D - . )
 CODE WRITE-BLOCK2 4300 R\W-BLOCK  END-CODE ( D - . )
DECIMAL : TEST  0.
  BEGIN  CR 2DUP D.
       2000. D+ ( SKIP 1 MEG)
       2DUP READ-BLOCK2 1 AND UNTIL
DROP DROP ;
0 IVAR SYSTEM-OFFSET
HEX : SAVE 140 * SYSTEM-OFFSET !
  140 0 DO I 0 READ-BLOCK2 .
       SYSTEM-OFFSET @ I + 0 WRITE-BLOCK2 .
  LOOP ;
: .ELECTIVE 140 UM* 48. D+ READ-BLOCK2 . RW-BUFFER C/L TYPE ;
DECIMAL

( 16 BITS: Experimenting with drive parameters ) HEX
ALIGN 0 IVAR RW-BUFFER B/BUF ALLOT
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,
2 , ( 2 sectors/block) RW-BUFFER , 7C0 ,
HERE 2 ALLOT  0 , 0 , 0 , CONSTANT BL#
 : R\W-BLOCK  ASSEMBLER  ( MACRO: OPCODE -- . )
  POP|X, BX|    POP|X, AX|
  ADD, W| AX'| R| AX|  MOVFA, W1| BL# W, XCHG|AX, BX|
  ADC, W| AX'| R| AX|  MOVFA, W1| BL# 2 + W,
  PUSH|X, SI|  MOVXI, BX| W,  MOVXI, DX| 0080 W,
  MOVXI, SI| PARAM-BLOCK W,  TO-REAL,
  MOVI, W| AX| 7C0 MEM,  MOVSW, T| DS| AX|
  XCHG|AX, BX|
  INT, 13 B, PUSHF, POP|X, BX| TO-PROT,
  POP|X, SI|   PUSH|X, BX|  NEXT ;
DECIMAL
( Experimenting ALLOC-MEM Get_sludges_of_memory) ?WI
REQUIRE ASSEMBLERi86 HEX
CODE BIOS31SI
  LEA, BP'| DB| [BP] -2 B,     MOV, W| F| SI'| DB| [BP] 0 B,
  POP|X, DI|   POP|X, SI|   POP|X, DX|
  POP|X, CX|   POP|X, BX|   POP|X, AX|
  INT, 31 C,
  PUSH|X, AX|  PUSH|X, BX|  PUSH|X, CX|  PUSH|X, DX|
  PUSH|X, SI|  PUSH|X, DI|  PUSHF,
  MOV, W| T| SI'| DB| [BP] 0 B,   LEA, BP'| DB| [BP] 2 B,
NEXT END-CODE
: BIOS31SI+ BIOS31SI 1 AND 0D ?ERROR ;
\ Get an amount DOUBLE of memory, return linear ADDRESS and HDL
: ALLOC-MEM   0501 SWAP ROT 0 0 0   BIOS31SI+
  >R >R DROP >R >R DROP R> R> R> R> ;
DECIMAL
( Experiment with DPMI testing jumps to 32 bit code. ) ?WI
REQUIRE ASSEMBLERi86 REQUIRE GET-SEL REQUIRE ALLOC-MEM
REQUIRE PC   REQUIRE SEL-DUMP HEX
10.0000 ALLOC-MEM CREATE HANDLE , ,
NEW-SEL CONSTANT NEW32          \ Create a new segment that
NEW32 PAD GET-SEL                 \ differs from current code
PAD TOGGLE-32     \ segment in being 32 bit.
10.0000 -1. D+ PAD 6 + C@ 0F INVERT AND OR PAD 6 + C!   PAD !
PAD 2 + !   DUP PAD 4 + C!   8 RSHIFT PAD 7 + C!
PAD SEL-DUMP NEW32 PAD PUT-SEL
\ To prove that we can actually use the 32 bits code segment.
CODE CRASH    \ It doesn't crash. But pushes a 32 bit EAX !
JMPFAR, HERE 4 + , NEW32 ,
PUSH|X, AX|
JMPFAR, HERE 6 + , 0 , PC ,
NEXT END-CODE                                DECIMAL
( Experimenting Get_a_32_bit_code_segment) ?WI HEX
: MOVEIT   NEW32 LES   0 0 FFF0 MOVE   LES DROP ;
: GETIT   NEW32 PAD GET-SEL    PAD TOGGLE-CODE
   NEW32 PAD PUT-SEL ;
NEW32 GET-ALIAS CONSTANT NEW32D
CODE CRASH
  POP|X, DI|   POP|X, DX|  POP|X, CX|  POP|X, BX|   POP|X, AX|
  JMPFAR, HERE 4 + , NEW32 ,
  INT, 31 C,
  JMPFAR, HERE 6 + , 0 , PC ,
  PUSH|X, AX|  PUSH|X, BX|  PUSH|X, CX| PUSH|X, DX| PUSH|X, DI|
  PUSHF,
NEXT END-CODE
DECIMAL


( Experimenting Use_a_32_bit_code_segment) ?WI HEX
CODE CRASH2  POPS, ES|
  POP|X, DI|   POP|X, DX|  POP|X, CX|  POP|X, BX|   POP|X, AX|
  JMPFAR, HERE 4 + , NEW32 ,
  INT, 31 C,
  JMPFAR, HERE 6 + , 0 , PC ,
  PUSH|X, AX|  PUSH|X, BX|  PUSH|X, CX| PUSH|X, DX| PUSH|X, DI|
  PUSHF, PUSHS, DS| POPS, ES|
NEXT END-CODE
: IDLE-OKAY   1680 REG-SET 1C + !   0 REG-SET 1E + !
0300 002F 0 0 REG-SET CRASH ;
: OKAY 200 REG-SET 1C + ! 0 REG-SET 1E + !   &x REG-SET 14 + !
  0 REG-SET 16 + ! 0300 0021 0 0 REG-SET CRASH ; \ Works!
: CRSH2 200 REG-SET 1C + ! 0 REG-SET 1E + !   &x REG-SET 14 + !
  0 REG-SET 16 + ! 0300 0021 0 0 REG-SET NEW32D CRASH2 ;
DECIMAL \ The last one crashes under windows 3.11: 32 bit ES
( **************ciforth FIG model examples **************)
      EXIT

These are examples that have worked on ciforth versions
2.### i.e. those that were still FIG compatible

They have not been adapted to the ISO standard (yet)
Some of them did work on FIG though.








( The FIG width trick )
 <HEX
  : TOH 30 - DUP 9 > IF 7 - THEN ;
     1 WIDTH !
  : ". ( 0/1 Leaves ASCII character at .  f.i. 'A leaves 41H)
    HERE 2 + C@ [COMPILE] LITERAL ; IMMEDIATE
  : ^. ( 0/1 leaves control character at . f.i. ^A leaves 01H)
    HERE 2 + C@ 1F AND [COMPILE] LITERAL ; IMMEDIATE
  : $.. ( 0/1 leaves hex number f.i. $0A leaves 0AH)
    HERE 2 + C@ TOH 10 * HERE 3 + C@ TOH + [COMPILE] LITERAL ;
  IMMEDIATE
  : $.... ( 0/1 leaves hex number 16 bits)
     0 HERE 6 + HERE 2 + DO 10 * I C@ TOH + LOOP
     [COMPILE] LITERAL ; IMMEDIATE
     1F WIDTH ! HEX>
  $1B CONSTANT ESC    $0F CONSTANT SI   $0E CONSTANT SO
( --BNF_parser ) \ AH&CH A0oct03
REQUIRE +THRU












1 9 +THRU

( FORWARD ) \ AH&CH A0oct03
( Create a forward definition, one that patches its own
  call with a cfa that is in its data field. Then goes on
  with that definition. )
: FORWARD CREATE 0 , DOES> @ DUP 0= 9 ?ERROR
   R> 1 CELLS - DUP >R  ! ;
( : DOIT HERE IN @ POSTPONE ' POSTPONE >DFA ! IN !
POSTPONE : ; )
: :R  IN @ >R [COMPILE] : R> IN !
HERE >CFA (WORD) FOUND IF CELL+ ! THEN ; IMMEDIATE
FORWARD FAC
:R FAC   DUP 0= IF DROP 1 ELSE DUP 1 - FAC * THEN ;
.S 4 FAC .S ." 4! IS " .



( BNF PARSER NON-IM COMPILING WORDS CALLED BY IM WORDS AH&CH)
0 IVAR SUCCESS
: POP POSTPONE R> ;    : PUSH POSTPONE >R ;
: SUC@ POSTPONE  SUCCESS POSTPONE @ ;
: SUC! POSTPONE 1 POSTPONE SUCCESS POSTPONE ! ;
: % DSP@ H. TIB @ IN @ TYPE
SPACE SUCCESS ? CR ; : % POSTPONE  % ;
 CODE  POPSP MOV, W| T| SP'| DB| [BP] 0 B,
 LEA, BP'| DB| [BP] 0 CELL+ B, NEXT END-CODE
: <PTS POSTPONE DSP@ PUSH
       POSTPONE DP POSTPONE @ PUSH
       POSTPONE IN POSTPONE @ PUSH ;
: BT> POP POSTPONE IN POSTPONE !
      POP POSTPONE DP POSTPONE !
      POSTPONE POPSP ;
: PTS> POP POSTPONE DROP POP POSTPONE DROP  POP POSTPONE DROP ;
( compile-only words called by immediate words   )
( Fake an embedded colon definition, i.e. an `EXIT' between
  `<FAKE' and `FAKE>' must return after `FAKE>' )
: <FAKE POSTPONE LIT HERE 0 , POSTPONE >R ;
: FAKE>  POSTPONE R> POSTPONE DROP  HERE SWAP ! ;

( Bracket an optional part, i.e. its success depends on what is
  before it. The part must balance the return stack. )
: <OPT  SUC@ PUSH ;
: OPT>  POP POSTPONE SUCCESS POSTPONE ! ;






( HANDLING OF SINGLE CHARACTERS )

: @TOKEN  IN @ TIB @ + C@ ;
: +TOKEN  ( f ) IF 1 IN +! THEN ;
: =TOKEN ( n) SUCCESS @ IF @TOKEN = DUP SUCCESS ! +TOKEN
ELSE       DROP THEN ;
: TOKEN CREATE ( c) C, DOES> ( a) C@ =TOKEN ;

1 WIDTH !
: '__  HERE 2 + C@ [COMPILE] LITERAL POSTPONE =TOKEN ;
IMMEDIATE
31 WIDTH !
0 TOKEN <EOL>    $0A TOKEN 'CR'    BL TOKEN 'BL'



( bnf PARSER TOKENS                                     )
( Skip blanks and handle comment )
: SKIP-BLANK TIB @ IN @ + BEGIN DUP C@ BL = WHILE
  1+ 1 IN +! REPEAT DROP ;
FORWARD skip  ( SKIP-BLANK is possible)
: +KEYWORD ( len f -) IF IN +! ELSE DROP THEN ;
: =KEYWORD  ( VS -) SUCCESS @ IF $@ >R R
IN @ TIB @ + R@ COMPARE DUP SUCCESS ! R> SWAP +KEYWORD
ELSE DROP THEN ;
: KEYWORD CREATE BL WORD HERE C@ 1+ ALLOT
DOES> skip =KEYWORD ;





( BNF PARSER                                   ch&ch )
: `IF [COMPILE] IF ;            : `BEGIN [COMPILE] BEGIN ;
: `ELSE [COMPILE] ELSE ;        : `WHILE [COMPILE] WHILE ;
: `THEN [COMPILE] THEN ;        : `REPEAT [COMPILE] REPEAT ;
: `EXIT POSTPONE EXIT ;

: <BNF  SUC@ `IF <PTS `ELSE `EXIT `THEN ;
: BNF>  SUC@ `IF PTS> `ELSE BT> `THEN ;

( Embed a BNF definition in the current one, i.e.
<<BNF ... BNF>>  is equivalent to xxx with xxx defined
by BNF: xxx ... ;BNF )
: <<BNF <FAKE <BNF ;
: BNF>> BNF> FAKE> ;


( THE { } round_bracket_pair and [ ] definitions )
( Start a BNF definition, must have been declared by FORWARD )
: BNF:   [COMPILE] :R   <BNF   SUC!   ;
: ;BNF   BNF>   [COMPILE] ;   ; IMMEDIATE

: | SUC@ `IF PTS> `EXIT `ELSE BT> <PTS SUC! `THEN ;
IMMEDIATE
: (( <<BNF ;  IMMEDIATE
: )) BNF>> ;  IMMEDIATE
: [ <OPT <<BNF ;  IMMEDIATE
: ] BNF>> OPT> ;  IMMEDIATE
: {  <OPT `BEGIN SUC@ `WHILE <<BNF ; IMMEDIATE
: } BNF>> `REPEAT OPT> ; IMMEDIATE



( Examples and tests )
 : COMMENT  SKIP-BLANK  ;
FORWARD AUX1 BNF: AUX1 'A' [ 'B' | 'C' ] ;BNF
: RUN[ 1 SUCCESS ! AUX1 SUCCESS ? ;
: RUN{ 1 SUCCESS ! 'A' { 'B' 'C' } SUCCESS ? ;
FORWARD <CHAR>
BNF: <CHAR> @TOKEN DUP [CHAR] ) = >R DUP [CHAR] ( = >R 0=
   R> R> OR OR   0= DUP SUCCESS !      +TOKEN ;BNF
( This requires an enormous return stack ! )
FORWARD <S>   BNF: <S>  '(' <S> ')' <S> | <CHAR> <S> | ;BNF
FORWARD AUX3 BNF: AUX3 <S>  <EOL>  ;BNF
: RUN() 1 SUCCESS ! AUX3 SUCCESS @
IF -1 IN +! ." ok" ELSE ." NOK"  THEN ;

FORWARD "KEY" BNF: "KEY" 'K' 'E' 'Y' ;BNF
: RUNKEY 1 SUCCESS ! "KEY" SUCCESS ? ;
( Push the string matched by the current terminal symbol on
 the data stack  bnf: iets  app nooot mies PUSH" ;bnf
 pushes what is matched by iets )
: PUSH" R>   R@ TIB @ +   IN @ R@ -  ROT >R ;
( Add the string on the stack to the output )
: POP" >R HERE R@ CMOVE R> ALLOT BL C, ;
( Add the symbol matched to the output )
: .SYMBOL POSTPONE PUSH" POSTPONE POP" ; IMMEDIATE
FORWARD STATEMENT    FORWARD IDENTIFIER
BNF: IDENTIFIER { 'A' | 'B' | 'C' } .SYMBOL ;BNF
BNF: STATEMENT IDENTIFIER COMMENT '+' IDENTIFIER  " F+" POP"
| '(' STATEMENT COMMENT ')'  ;BNF
: PS HERE 1 SUCCESS ! STATEMENT SUCCESS ?  HERE OVER - TYPE ;



( test screen for BNF parser. )
PS ABA + BABAA
." NOW COME THE FREAKS"
( PS (A+B)     )












( **************Non working FIG model examples  **************)
      EXIT

These are examples from old FIG screens.
They have not been adapted to the ISO standard (yet)
Some of them did work on FIG though.










( ." QUADRUPLE ARITHMETIC 08-02-84 "                     )
 : ADC ( n1,n2-n,c  add, leave sum and carry)
    0 SWAP 0 D+ ;
 : 2M+ ( d1,d2-d,c  add double )
   >R SWAP >R    ADC R> ADC   R> SWAP >R
   ADC R> + ;
 : 3M+ ROT >R 2M+ R> ADC ;
 : 4M+ ROT >R 3M+ R> ADC ;
 : 2U*  ( d1,d2-q unsigned product)
 ROT ( l1,l2,h2,h1)    OVER OVER UM* >R >R .S
 ROT ( l1,h2,h1,l2)    OVER OVER UM* >R >R .S
 SWAP DROP ROT ROT ( l2,l1,h2) OVER OVER UM* >R >R .S
 DROP ( l1,l2)    UM* .S R> ADC .S R> ADC .S
  IF ( carry) R> R> 2M+ 1+ ." C" ELSE
            R> R> 2M+    ." NC" THEN  .S
  R> R> 2M+ DROP .S ;
( ." CASSADY'S 8080 ASSEMBLER 81AUG17  >1<"                  )
HEX VOCABULARY ASSEMBLER IMMEDIATE : 8* DUP + DUP + DUP + ;
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER DSP@ ; IMMEDIATE
ASSEMBLER DEFINITIONS ( ;CODE see screen3 )
: C; PREVIOUS ?EXEC ?CSP ; IMMEDIATE
: LABEL ?EXEC 0 IVAR SMUDGE -2 ALLOT [COMPILE] ASSEMBLER
    DSP@ ; IMMEDIATE     ASSEMBLER DEFINITIONS
4 CONSTANT H    5 CONSTANT L     7 CONSTANT A    6 CONSTANT PSW
2 CONSTANT D    3 CONSTANT E     0 CONSTANT B    1 CONSTANT C
6 CONSTANT M 6 CONSTANT SP 'EXIT >CFA 0B + @ CONSTANT (NEXT)
: 1MI CREATE C, DOES> C@ C, ;  : 2MI CREATE C, DOES> C@ + C, ;
 : 3MI CREATE C, DOES> C@ SWAP 8* +  C, ;
: 4MI CREATE C, DOES> C@ C, C, ;
: 5MI CREATE C, DOES> C@ C, , ;  : PSH1 C3 C, (NEXT) 1 - , ;
: PSH2 C3 C, (NEXT) 2 - , ;       : NEXT C3 C, (NEXT) , ;
1 2 +THRU
(   ." CASSADY'S 8080 ASSEMBLER 81AUG17  >2<"              )
00 1MI NOP     76 1MI HLT     F3 1MI DI     FB 1MI EI
07 1MI RLC     0F 1MI RRC     17 1MI RAL    1F 1MI RAR
E9 1MI PCHL    F9 1MI SPHL    E3 1MI XTHL   EB 1MI XCHG
27 1MI DAA     2F 1MI CMA     37 1MI STC    3F 1MI CMC
80 2MI ADD     88 2MI ADC     90 2MI SUB    98 2MI SBB
A0 2MI ANA     A8 2MI XRA     B0 2MI ORA    B8 2MI CMP
09 3MI DAD     C1 3MI POP     C5 3MI PUSH   02 3MI STAX
0A 3MI LDAX    04 3MI INR     05 3MI DCR    03 3MI INX
0B 3MI DCX     C7 3MI RST     D3 4MI OUT    DB 4MI IN
C6 4MI ADI     CE 4MI ACI     D6 4MI SUI    DE 4MI SBI
E6 4MI ANI     EE 4MI XRI     F6 4MI ORI    FE 4MI CPI
22 5MI SHLD    2A 5MI LHLD    32 5MI STA    3A 5MI LDA
CD 5MI CALL    C3 5MI JMP
             ( CZ,CNZ,CCY,CNC)

(   ." CASSADY'S 8080 ASSEMBLER 81AUG17  >3<"              )
C9 1MI RET                   C2 CONSTANT 0=  D2 CONSTANT CS
E2 CONSTANT PE  F2 CONSTANT 0<   : NOT 8 + ;
: MOV 8* 40 + + C, ;   : MVI 8* 6 + C, C, ;  : LXI 8* 1+ C, , ;
: THEN HERE SWAP ! ;               : IF C, HERE 0 , ;
: ELSE C3 IF SWAP THEN ;           : BEGIN HERE ;
: UNTIL C, , ;                     : WHILE IF ;
: REPEAT SWAP C3 C, , THEN ;
EXIT

: ;CODE
?CSP   POSTPONE   (;CODE)   [COMPILE] [   [COMPILE] ASSEMBLER
; IMMEDIATE



(   ." CRC CHECK FOR FIG  85JAN06 ALBERT VAN DER HORST"       )
( Adapted from FORTH DIMENSIONS IV-3 ) HEX
 : ACCUMULATE ( oldcrc/char -- newcrc )
   0100 * XOR
   8 0 DO
      DUP 0< IF 4002 XOR DUP + 1+ ELSE DUP + THEN
   LOOP ;
 : DISPOSE ( crcvalue/adres/len -- newcrcvalue)
    OVER DUP C@ "( = SWAP 1+ C@ BL = AND OVER 1 = AND IF
       ( comment; skip it) DROP DROP ") WORD DROP
    ELSE
       1+ OVER + SWAP DO I C@ ACCUMULATE LOOP
    THEN ;
 : MORE ( -- adr f  Leaves flag if there is more in the block)
    BL WORD DUP C@ 2 < OVER 1+ C@ "! < AND 0=
 ;
(    ." CRC 2 "                                               )
 : VERIFY-BLOCK ( oldcrc/blnr -- newcrc)
   BLK @ >R IN @ >R   BLK !  0 IN !
   BEGIN MORE WHILE
       BL OVER COUNT + C! COUNT DISPOSE
   REPEAT DROP ( drop the address left by MORE)
   R> IN ! R> BLK ! ;
 : VERIFY ( scrnr/crc)
   0 SWAP B/SCR * DUP B/SCR + SWAP DO
      I VERIFY-BLOCK
   LOOP
 ;
 : VER   SCR @ VERIFY U. ;



( CRC_test_screen)
     For program exchange, the medium of hard copy is cheap,
convenient, and machine-independent. Its primary disadvantages
are the time required for hand-typing the source code and the
possibility of human error in the process. Even if the screens
LOAD without error messages, some errors may pass undetected
until run-time, when the system crashes mysteriously.









( GILBREATH 's benchmark - BYTE jan 83 )  ( VERSIE #1) ( FIG)
 8190 CONSTANT SIZE
 0 VARIABLE FLAGS      SIZE ALLOT

 : DO-PRIME
     FLAGS SIZE 1 FILL
     0 ( 0 COUNT ) SIZE 0
     DO FLAGS I + C@
      IF I DUP + 3 +  ( DUP . )
         DUP I +
         BEGIN DUP SIZE <
         WHILE 0 OVER FLAGS +  C!  OVER + REPEAT
         DROP DROP 1+
      THEN
     LOOP
     . ." PRIMES" ;
(    ." ERATOSTHENES >1< Variables - A. van der Horst"  CR   )
 ( User specified variables:)
 52 IVAR CH/L  ( Characters per line)
 22 IVAR LN/P  ( Lines per page)
  1 IVAR PAUSE ( Boolean: pause between pages)

 ( Other:)
 6250 CONSTANT SIZE ( 16 numbers pro byte)
 0 IVAR FLAGS      SIZE ALLOT
 FLAGS SIZE + CONSTANT END-FLAGS
 0 IVAR LIM     ( part of FLAGS considered)
 0 IVAR C#      0 IVAR L#  ( char and line counter)
 0 IVAR THOUSANDS ( #  thousand to be sieved)
 0 IVAR MILS      ( Contains current thousand)
 0 IVAR MANTISSA  ( The current thousands is to be printed)

(    ." ERATOSTHENES >2< Pretty printing - A. van der Horst" )
 : FFEED  PAUSE @ IF CR ." KEY FOR NEXT SCREEN" KEY DROP THEN
     12 EMIT CR ." ERATOSTHENES SIEVE -- PRIMES LESS THAN"
     THOUSANDS @ 5 .R ."  000" CR 2 L# ! 1 MANTISSA ! ;
 : ?P ( LENGTH -- . , give FF if LENGTH lines don't fat)
      DUP L# +! L# @ LN/P @ > IF FFEED L# +! ELSE DROP THEN ;
 : NEWLINE  ( Start at a new line, maybe with a mantissa)
     1 ?P CR ( Checks first)
     MANTISSA @ IF MILS @ 6 .R ELSE 6 SPACES THEN
     6 C# !   0 MANTISSA ! ;
 : ?L ( LENGTH -- . , give LF if LENGTH char's don't fit)
      DUP C# +! C# @ CH/L @ >
      IF NEWLINE C# +! ELSE DROP THEN ;
 : .P   4 ?L SPACE 0 <# # # # #> TYPE ;
 : INIT-P  FFEED NEWLINE  ;

(    ." ERATOSTHENES >3< Bit manipulation - A. van der Horst ")
   HEX
 : NOT   0FF XOR ( N -- N  FLIP ALL BITS OF N) ;
 0 IVAR S-MASK -2 ALLOT 01 C, 02 C, 04 C, 08 C,
                          10 C, 20 C, 40 C, 80 C,
 0 IVAR C-MASK -2 ALLOT
           01 NOT C, 02 NOT C, 04 NOT C, 08 NOT C,
           10 NOT C, 20 NOT C, 40 NOT C, 80 NOT C,
 : INIT-T   FLAGS SIZE 0FF FILL ; ( Preset to 'prime')
 DECIMAL
 : 8/MOD   0 8 UM/MOD ; ( May be redefined in assembler )
 : CLEAR-B ( BIT# --  clears the specified bit)
         8/MOD FLAGS + SWAP  ( Address in flags table)
         C-MASK + C@         ( Get mask)
         OVER C@ AND SWAP C! ( Clear the bit)  ;

(    ." ERATOSTHENES >4< Bit manipulation - A. van der Horst ")
 : SET-B ( BIT# --  sets the specified bit)
         8/MOD FLAGS + SWAP  ( Address in flags table)
         S-MASK + C@         ( Get mask)
         OVER C@ OR SWAP C!  ( Store with bit set)  ;
 : TEST-B ( BIT# -- FLAG  Gets a FLAG testable by IF)
         8/MOD FLAGS + C@ SWAP  ( Get flag)
         S-MASK + C@ AND        ( Result: =0 or #0)     ;
 : FLIP ( PRIME,START -- .  , marks multiples of PRIME as
      (  non prime starting with START)
         BEGIN  DUP LIM @ U<  WHILE
                DUP CLEAR-B  OVER +
         REPEAT   DROP DROP ;



(    ." ERATOSTHENES >5< Main program - A. van der Horst " CR)
 : BATCH1 ( First batch of 500 numbers)
      500 1 ( Only odd numbers)
     DO I TEST-B
      IF I DUP + 1 + DUP .P ( get prime number)
         I FLIP THEN ( Mark multiple as non-prime)
     LOOP ;
 : BATCH ( OFFSET --  every  following batch )
      500 0
      DO DUP I + TEST-B IF I DUP + 1 + .P THEN
      LOOP DROP ;
 : SIEVE  ( N --  Makes list of primes <N*1000 )
     DUP THOUSANDS !   500 * LIM !  0 MILS !
     INIT-T INIT-P 2 .P BATCH1
     THOUSANDS @ 1
     DO I MILS !  1 MANTISSA !  NEWLINE I 500 * BATCH LOOP ;
( **************Non working FIG CP/M  examples  **************)
      EXIT


These FIG screens contains, in addition to FIG portability
problems, CP/M dependant tricks and knowledge.










( EXTENDING THE FORTH SYSTEM #1 84/4/12 A.H.)
<HEX
 : NEW-SYSTEM   ( Generates a new FORTH system, )
              ( using the CP/M SAVE command)
      LATEST NFA 10C ! ( Define new topmost word)
      ( Initial value for VOC-LINK and FENCE:)
      HERE DUP 11C ! 11E !
      HERE 100 / DECIMAL CR
      CR ." TYPE: SAVE" . ." NEWFORTH.COM"
      BYE
 ;     HEX>





( DISC IO SCREEN 16 WRITE    >1<   85/12/08 AH )
 0 IVAR DISK-BUFFER-W 100 ALLOT
 DISK-BUFFER-W IVAR POINTER-W
0 IVAR FCB2   21 ALLOT  ( BUG: 2nd goes wrong)
 : ?PRES ( .-. Aborts whether the entry is already present )
    FCB2 0F BDOS 0FF - IF ." ALREADY PRESENT" QUIT THEN
    FCB2 10 BDOS DROP ;
 : .OPENW FCB2 CLEAN-FCB FCB2 FILL-FCB ?PRES
   FCB2 16 BDOS 0FF = IF ." DISK FULL " QUIT THEN
   DISK-BUFFER-W POINTER-W ! ;
 : .CLOSEW
      DISK-BUFFER-W SET-DMA FCB2 15 BDOS . ." LAST RECORD" CR
          FCB2 10 BDOS . ." CLOSE STATUS" CR ;
 : MOVE-DOWN   -80 POINTER-W +!
             DISK-BUFFER-W 80 OVER + SWAP 80 CMOVE ;

( DISC IO SCREEN 17 WRITE    >2<   85/12/08 AH )3<)
 : TO-DISK DUP >R POINTER-W @ SWAP CMOVE
         R> POINTER-W +!
         POINTER-W @ DISK-BUFFER-W -
         80 >  IF   -->
            DISK-BUFFER-W SET-DMA FCB2 15 BDOS .
            MOVE-DOWN
        THEN ;

 : .WRITE  ( 2/0 WRITE SCREEN-1 .. SCREEN-2 TO DISK)
      1+ B/SCR * SWAP B/SCR * ( GET START BUFFER #'S)
      DO I BLOCK DUP
      40 -TRAILING TO-DISK  CRLF 2 TO-DISK
      40 + 40 -TRAILING TO-DISK CRLF 2 TO-DISK
      LOOP CTRLZ 1 TO-DISK
 ;   HEX>
( DISC IO SCREEN 18 READ     >1<   85/12/08 AH )
 <HEX  ( BUG: 64 char lines go wrong)
 0 IVAR DISK-BUFFER-R  80 ALLOT 0 IVAR POINTER-R
  0A CONSTANT "LF"  0D CONSTANT "CR"
  1A CONSTANT ^Z    DISK-BUFFER-R 80 + CONSTANT END-BUF
  0 IVAR EOF
 : .OPENR   FCB2 DUP CLEAN-FCB FILL-FCB
       END-BUF POINTER-R !
       FCB2 0F BDOS 0FF = IF ." NOT PRESENT" QUIT THEN
       0 EOF ! ;
 : .CLOSER   FCB2 10 BDOS . ." CLOSE STATUS" CR ;





( DISC IO SCREEN 19 READ     >2<   85/12/08 AH )
 : ?EMPTY ( POINTER -- CORRECTED PNR, READ SECTOR IF AT END)
     DUP END-BUF = IF DISK-BUFFER-R SET-DMA  FCB2 14 BDOS .
                  DROP DISK-BUFFER-R THEN  ;
 : GET-CHAR
    POINTER-R @
      ?EMPTY                   ( GET NEW BUFFER IF NEEDED)
      DUP C@ "LF" = IF 1+ ?EMPTY THEN ( SKIP OVER LF)
      DUP C@ SWAP              ( GET CHAR, POINTER ON TOP)
      OVER ^Z =
      IF 1 EOF ! ELSE 1+ THEN ( INCREMENT POINTER UNLESS AT ^Z)
    POINTER-R !  ;




( DISC IO SCREEN 20 READ     >3<   85/12/08 AH )
 : GET-LINE ( ADR -- . reads a line to ADR )
      DUP 40 20 FILL ( preset spaces )
      41 OVER + SWAP ( max $41 char to a line, CR!)
      DO  GET-CHAR
        DUP "CR" = IF DROP 20 LEAVE THEN
        DUP ^Z   = IF DROP 20 LEAVE THEN
        I C! ( may leave spurious 81th space)
      LOOP  ;
 : .READ ( 2/0 READ SCREEN-2 TO SCREEN -1)
      1+ B/SCR * SWAP B/SCR * ( get start buffer #'s)
      DO  I BLOCK DUP GET-LINE
        DUP 40 + GET-LINE  81 + 0 SWAP C! UPDATE
        I #BUFF MOD 0= IF ( full load of buffers) FLUSH THEN
      LOOP
; HEX>
( DISC IO SCREEN 21  LOAD    >1<   85/12/08 AH )
 ( EXAMPLE: .OPENR TEMP" 25 26 .LOAD .CLOSER )
 <HEX  0 IVAR LBUF 3E ALLOT 0 C,
 : I-F-A ( ADRES -- . ,INTERPRET FROM ADDRESS )
     TIB @ >R  IN @ >R  ( SAVE CURRENT INTERPRET POSITION)
     TIB !     0 IN !   ( NEW POSITION)
     0 INTERPRET
     >R IN !   >R TIB ! ( RESTORE)  ;

 : .LOAD ( LOAD THE CPM FILE SPECIFIED IN FCB2 )
       BEGIN   LBUF DUP GET-LINE I-F-A
       EOF @ UNTIL ;


    HEX>

( STREAM READ ROUTINES CP/M 85/012/08  AH )
 : F_READ ( B,N-N2 Tries to read N char's to buffer B)
        ( N2 is number actually read, 0 for EOF)
      ( NOT  YET: NOW IT IS FILLED WITH ^Z, NOTHING RETURNED )
  BEGIN
     SWAP GET-CHAR
     OVER C! 1+ SWAP 1 -
     DUP 0=
  UNTIL
 ;
 : F_WRITE ( B,N-N2 Tries to write N char's from buffer B)
      ( N2 is the number actually written to disk )
      ( NOT  YET: NOW IT IS UNCLEAR, NOTHING RETURNED )
   TO-DISK
 ;

(   ." 84NOV25 Initialize STAR-printer AH "  <HEX             )
 : PEMIT 7F AND 5 BDOS DROP ;
 : PCR   0D PEMIT   0A PEMIT ;
 : INIT-STAR ( N--. N is lines per pages)
    ESC PEMIT "@ PEMIT ESC PEMIT "C PEMIT ( TOS) PEMIT ;
 : CONDENSED  ESC PEMIT "P PEMIT "3 PEMIT ;
 : EMPHASIZED ESC PEMIT  "E PEMIT ;
 : DOUBLE ESC PEMIT "G PEMIT ;
 : BOLD EMPHASIZED DOUBLE ;
 ( 137 CH/L !    60 LN/P !     0 PAUSE ! )
  : PSPACES  ( 1/0 print N-1 spaces)
    0 DO 20 PEMIT LOOP ;
  : PTYPE  ( ADDRESS,LENGTH -- . PRINT LENGTH CHAR AT ADDRESS)
        ?DUP IF
        OVER + SWAP DO I C@ PEMIT LOOP THEN ;
  : P."  "" WORD COUNT PTYPE ;       34 LOAD
( READ-FILE ) ( AvdH A1nov3 )
HEX  : RPD 4891 @ 0FFFF AND ;
CREATE RW-BUFFER 400 ALLOT
( ISO  ( buf len fd -- len ior)
: READ-FILE   RW-BUFFER ROT ROT 3F00 BDOSN IF
   >R DROP 0 R> NEGATE ELSE
   >R >R   RPD RW-BUFFER   0 R>   R@ FARMOVE   R> 0
THEN ;
( ISO  ( buf len fd -- len ior)
: WRITE-FILE   >R >R  0 SWAP  RPD RW-BUFFER R@ FARMOVE
   RW-BUFFER R> R> 4000 BDOSN IF NEGATE ELSE DROP 0 THEN ;
( Honest to god Unix read ( fd buf len -- len/err)
: READ ROT READ-FILE OR ;



( WRITE-FILE READ-FILE --large_buffers ) ?WI \ AvdH
REQUIRE OLD:
\ ISO WRITE-FILE : Chop into 32 K chunks for using WRITE
: WRITE-FILE >R
    BEGIN 2DUP 8000 MAX R@ OLD: WRITE-FILE
    DUP IF RDROP >R 2DROP R> EXIT THEN DROP
    8000 - DUP 0> WHILE
    SWAP 8000 + SWAP REPEAT
RDROP 2DROP 0 ;
VARIABLE RC






( WRITE-FILE READ-FILE --large_buffers ) ?WI \ AvdH
: READ-FILE 0 RC ! >R
    BEGIN 2DUP 8000 MAX R@ OLD: READ-FILE
    DUP IF RDROP >R 2DROP R> EXIT THEN DROP
    RC +! 8000 - DUP 0> WHILE
    SWAP 8000 + SWAP REPEAT
RDROP 2DROP RC @ 0 ;   'RC HIDDEN








( 4096 last line)
