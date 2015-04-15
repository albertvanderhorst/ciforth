 ciforth lab  $Revision$ (c) Albert van der Horst
 : EMPTY STACK
 : DICTIONARY FULL
 : FIRST ARGUMENT MUST BE OPTION
 : ISN'T UNIQUE
 : EMPTY NAME FOR NEW DEFINITION
 : DISK RANGE ?
 : FULL STACK
 : ERROR ACCESSING BLOCKS FROM MASS STORAGE
 : UNRESOLVED FORWARD REFERENCE
 : NOT A WORD, NOR A NUMBER OR OTHER DENOTATION
 : WORD IS NOT FOUND
 : NOT RECOGNIZED
 : ERROR, NO FURTHER INFORMATION
 : SAVE/RESTORE MUST RUN FROM FLOPPY
 : CANNOT FIND WORD TO BE POSTPONED
 : CANNOT FIND WORD TO BE COMPILED
 : COMPILATION ONLY, USE IN DEFINITION
 : EXECUTION ONLY
 : CONDITIONALS NOT PAIRED
 : STACK UNBALANCE, STRUCTURE UNFINISHED?
 : IN PROTECTED DICTIONARY
 : USE ONLY WHEN LOADING
 : OFF CURRENT EDITING SCREEN
 : (WARNING) NOT PRESENT, THOUGH WANTED
 : LIST EXPECTS DECIMAL
 : AS: PREVIOUS INSTRUCTION INCOMPLETE
 : AS: INSTRUCTION PROHIBITED IRREGULARLY
 : AS: UNEXPECTED FIXUP/COMMAER
 : AS: DUPLICATE FIXUP/UNEXPECTED COMMAER
 : AS: COMMAERS IN WRONG ORDER
 : AS: DESIGN ERROR, INCOMPATIBLE MASK
 : AS: PREVIOUS OPCODE PLUS FIXUPS INCONSISTENT
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : NO BUFFER COULD BE FREED, ALL LOCKED
 : EXECUTION OF EXTERNAL PROGRAM FAILED
 : NOT ENOUGH MEMORY FOR ALLOCATE
 : UNKNOWN FORMAT IDENTIFIER
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
 : ( NO TEXT MESSAGE AVAILABLE FOR THIS ERROR )
( ************** configuration *******************************)














\
( CONFIG ?LEAVE-BLOCK ?16 ?64 ?LI ?PC ?MS ?FD ?HD ) \ B0jan12
: ?LEAVE-BLOCK   IF SRC CELL+ @ PP ! THEN ;
: CONFIG   CREATE 0= , DOES> @ ?LEAVE-BLOCK ;
  0 CELL+   DUP 2 = CONFIG ?16   DUP 2 > CONFIG ?32+
    DUP 4 = CONFIG ?32   DUP 8 = CONFIG ?64 DROP
 "CALL" PRESENT DUP   CONFIG ?WI   \ (MS-windows)
 "BIOS31" PRESENT DUP   CONFIG ?DP   \ DPMI (legacy windows)
 DUP 0= "BIOSN" PRESENT AND DUP   CONFIG ?MS   \ MS-DOS
            OR OR DUP   CONFIG ?WIMS \ One of 3 above
 "XOSV"   PRESENT DUP   CONFIG ?OSX  \ OSX.
 DUP 0= "XOS" PRESENT AND DUP     CONFIG ?LI   \ Linux, BSD.,
    OR           OR     CONFIG ?HS \ Any host
 "BIOSN"  PRESENT CONFIG ?PC   \ MS-DOS OR dpmi
 "LBAPAR" PRESENT DUP   CONFIG ?HD   \ Hard disk, modern
 "SEC-RW" PRESENT DUP   CONFIG ?FD   \ Floppy or hard disk old
    OR CONFIG ?SA
( HELP ) CF: ?HS \ A5dec07
: HELP-WANTED? ." Press space to skip " TYPE
 ." , other key to confirm" CR KEY BL <> ;
:  HELP    1 20 INDEX   CR
  "I will try to start a help window" TYPE CR
  "Press a key" TYPE CR KEY DROP
  "XOS"  PRESENT IF
  "PDF" HELP-WANTED? IF
      "acroread ci86.lina.pdf&" SYSTEM EXIT THEN
  "PostScript" HELP-WANTED? IF
      "gv ci86.lina.ps&" SYSTEM THEN
  "info" HELP-WANTED? IF
      "info -f ci86.lina.info" SYSTEM THEN
   ELSE   "BDOSN"  PRESENT IF
      "wina.pdf" SYSTEM THEN THEN ;

( -syscalls- ) CF: ?LI ?32                      \ AvdH B4feb10

 13 CONSTANT __NR_time
 43 CONSTANT __NR_times
  1 CONSTANT __NR_exit

120 CONSTANT __NR_clone
 37 CONSTANT __NR_kill
 48 CONSTANT __NR_signal
 12 CONSTANT __NR_chdir

HEX
 36 CONSTANT __NR_ioctl
 8E CONSTANT __NR_select

CREATE -syscalls- DECIMAL
( -syscalls- ) CF: ?LI ?64                      \ AvdH B4feb10

201 CONSTANT __NR_time
100 CONSTANT __NR_times
 60 CONSTANT __NR_exit

 56 CONSTANT __NR_clone
 62 CONSTANT __NR_kill
 48 CONSTANT __NR_signal
 80 CONSTANT __NR_chdir

HEX
 10 CONSTANT __NR_ioctl
 17 CONSTANT __NR_select

CREATE -syscalls- DECIMAL
( -syscalls- ) CF: ?OSX                         \ AvdH B4feb10

\ 13 CONSTANT __NR_time
\ 43 CONSTANT __NR_times
  1 CONSTANT __NR_exit

\ 120 CONSTANT __NR_clone
\  37 CONSTANT __NR_kill
\ 48 CONSTANT __NR_signal
\ 12 CONSTANT __NR_chdir





CREATE -syscalls- DECIMAL
( -legacy- ) CF: ?LI \ AvdH A8jun24









: LINOS XOS ;





( -legacy- $!-BD $S $I PRESENT? REQUIRE )       \ AvdH B4may28
\ This will make most old programs run.
WANT ALIAS

'$/ ALIAS $S   '$^ ALIAS $I
: $!-BD   2DUP C! 1+ SWAP CMOVE ;



: REQUIRE WANT ;   : REQUIRED WANTED ;

: PRESENT? PRESENT 0= 0= ;  \ For WORD sc: it IS found as such




( -legacy- IN >IN REFILL  )                   \ AvdH B4Oct25
WANT ALIAS
\ Use L_>IN instead of >IN , don't store into it!
: L_>IN PP   @   SRC   @   -   (>IN)   !   (>IN) ;
'L_>IN ALIAS >IN
: REFILL 0 ;
: IN PP ;









( -legacy- VOCABULARY )                        \ AvdH B2aug12
\ Use replacing vocabularies instead of pushing namespaces.

: FORTH   CONTEXT @ 'ONLY >WID <> IF PREVIOUS THEN FORTH ;

: VOCABULARY NAMESPACE  DOES> CELL+ CONTEXT ! ;










\ -legacy- WORD FIND (WORD) (PARSE)             \ AvdH B2oct02
WANT ALIAS      WANT $!-BD
\ ISO
: FIND   DUP COUNT PRESENT DUP IF   SWAP DROP DUP SWAP
    >FFA @ 4 AND  -1 SWAP IF NEGATE THEN THEN ;
\ ISO
: WORD   DUP BL = IF DROP NAME ELSE >R
    BEGIN PP@@ R@ = WHILE DROP REPEAT DROP -1 PP +!
    R> PARSE THEN   HERE 34 BLANK   HERE $!-BD HERE ;

'NAME ALIAS (WORD)     'PARSE ALIAS (PARSE)





\ -legacy- ?LOADING ?EXEC                       \ AvdH B2oct02










\ Exceptions on compilation modes.
: ?EXEC   STATE @   12 ?ERROR ;
: ?LOADING   BLK   @   0=   16 ?ERROR ;

CREATE -legacy-     \ Last legacy block!
( **************ISO language extension ***********************)
                    EXIT

An ISO language extension is either an ISO word, implemented in
a possibly non-portable way, or a word that is not defined in
the standard but that is implemented by using only standard
words.
Sometimes it is an ``in''tension, a loadable extension required
for ISO, that in my opinion should never be loaded.
Note that ISO words are only documented by the comment ISO.





\
( MEMORY _AH            )                      \ AvdH B0aug02
2 CELLS CONSTANT overhead
VARIABLE _ADP    : _AH ( -- adr ) _ADP @ ;
: _bump ( -- ) _AH @ _ADP ! ;
: _alloc_f   ( adr -- ) CELL+ DUP >R CELL+ + R> ! ;
: _free   ( adr -- ) 0 SWAP CELL+ ! ;
: _split ( adr -- ) DUP $@ SWAP @ ALIGNED >R 0 R@ CELL+ !
    R@ ! R> SWAP ! ;
: _merge ( adr -- ) DUP @ OVER < IF DROP ELSE DUP >R
    BEGIN @ DUP CELL+ @ UNTIL R> !  THEN ;
: _free? ( adr -- f ) CELL+ @ 0= ;
: _avail ( adr -- n )   DUP @ SWAP - overhead - ;
: _remain ( adr -- n ) DUP $@ SWAP @ DUP IF - NIP ELSE DROP
    SWAP - THEN overhead - ;
: _search  ( n -- adr ) _AH BEGIN DUP _merge 2DUP _remain >
    WHILE @ DUP _AH = 50 ?ERROR REPEAT _ADP ! DROP ;
( MEMORY ALLOCATE FREE RESIZE  )                \ AvdH B0aug02
WANT _AH
: _allocate ( n -- adu ) _AH _merge   DUP _AH _remain > IF
   _bump DUP _search THEN   _AH _free? 0= IF
   _AH _split _bump THEN   _AH _alloc_f _AH overhead + ;
: _alloc&move ( n -- adu) DUP _allocate DUP >R SWAP CMOVE  R> ;
: _resize ( adu n -- adu) OVER overhead - >R R@ _merge  DUP R@
    _avail > IF _alloc&move R> _free ELSE R> _alloc_f THEN  ;
: INTEGRITY? ( -- f ) DUP BEGIN DUP @ 2DUP < WHILE NIP
    REPEAT NIP = ;
DATA _alloc-buf _ , 0 , DSP@ HERE - 4 / ALLOT ALIGN
    _alloc-buf HERE OVER ! DUP , DUP ,  _ADP !
( ISO )
: ALLOCATE   ['] _allocate CATCH ;
: FREE   overhead - _free 0 ;
: RESIZE  ['] _resize CATCH DUP IF NIP THEN ;
( REALLOT REALLOC AT-HERE )                     \ AvdHB5Mar30
WANT ALIAS
\ For DEA of data fill the data pointer with here.
: AT-HERE   HERE SWAP >DFA ! ;

\ For DEA of data reserve LENGTH bytes at here.
: REALLOT   SWAP AT-HERE   ALLOT ;
'REALLOT ALIAS REALLOC








( SEARCH-WORDLIST GET-CURRENT SET-CURRENT )    \ AvdH B2aug12
\ ISO
: GET-CURRENT    CURRENT @ ;
: SET-CURRENT    CURRENT ! ;
: SEARCH-WORDLIST    (FIND) NIP NIP
    DUP IF DUP >FFA @ 4 AND   IF 1 ELSE -1 THEN THEN ;










( NOT 0<> >= <= UMIN U>                    )     \ AvdH B5Mar9
WANT ALIAS
'0= ALIAS NOT           : 0<> 0= NOT ;
: >=   < NOT ;          : <=   > NOT ;
: UMIN   2DUP U< IF SWAP THEN NIP ;
: UMAX   2DUP U< IF SWAP THEN DROP ;
: U>     SWAP U< ;









( D0= D0<> D0< D< D- M+ DRSHIFT DLSHIFT DU< )    \ AvdH B5Apr8
WANT NOT        WANT 0<>

: D0=  OR 0= ;          : D0<>  OR 0<> ;
: D-   DNEGATE D+ ;     : D0<   NIP 0< ;
: D<   ROT 2DUP <> IF > NIP NIP ELSE 2DROP U< THEN ;
: DU<  ROT 2DUP <> IF U> NIP NIP ELSE 2DROP U< THEN ;
: D>   2SWAP D< ;       : D>=  D< NOT ;         : D<=  D> NOT ;

: DLSHIFT >R SWAP DUP R@ LSHIFT SWAP 8 CELLS R@ - RSHIFT ROT R>
    LSHIFT OR ;
: DRSHIFT >R DUP R@ RSHIFT SWAP 8 CELLS R@ - LSHIFT ROT R>
    RSHIFT OR SWAP ;

: M+   S>D D+ ;

( PARSE-NAME SAVE-INPUT EXECUTE-PARSING --> )   \ B4oct16
WANT ALIAS
: SAVE-INPUT    SRC 2@ PP @ 3 ;                \ ISO
: RESTORE-INPUT   DROP PP ! SRC 2! -1 ;        \ ISO
: -->   BLK @ DUP UNLOCK   1+ DUP LOCK
    BLOCK B/BUF SET-SRC ; IMMEDIATE             \ ISO
'NAME ALIAS PARSE-NAME                          \ ISO
: EXECUTE-PARSING SAVE SET-SRC CATCH RESTORE THROW ; \ ISO







\
( TIME&DATE ) CF: ?LI                   \ AH B30423
WANT -syscalls-  : SSE   0 0 0 __NR_time XOS ; ( 1970/1/1)
: |   OVER , + ;   : 5m   31 | 30 | 31 | 30 | 31 | ;
DATA TABLE ( start of month within leap period) -1
    31 | 28 | 5m 5m   31 | 28 | 5m 5m   31 | 29 | 5m 5m
    31 | 28 | 5m 5m   ,   : T[] CELLS TABLE + @ ;
\ For DAYS within leap return MONTHS
: MONTHS   >R 0 BEGIN R@ OVER T[] > WHILE 1+ REPEAT 1- RDROP ;
\ For DAYS within leap period return DAY MONTH YEARS
: SPLIT-LEAP  DUP MONTHS DUP >R T[] - R> 12 /MOD >R 1+ R> ;
\ For TIME return SEC MIN HOUR DAYS
: SPLIT-OFF-TIME   0 60 UM/MOD   60 /MOD   24 /MOD ;
\ For DAYS return DAY MONTH YEAR
: SPLIT-OFF-DATE  1461 /MOD >R SPLIT-LEAP   R> 4 * + 1970 + ;
\ Return current  SEC MIN HOUR DAY MONTH YEAR
: TIME&DATE   SSE   SPLIT-OFF-TIME   SPLIT-OFF-DATE ;
( TIME&DATE ) CF: ?PC \ AH A30612
HEX
\ Return current  DAY MONTH YEAR
: DATE    2A00 _ _ _ BDOSO DROP SWAP >R >R 2DROP
    R> 100 /MOD   R> ;

\ Return current  SEC MIN HOUR
: TIME    2C00 _ _ _ BDOSO 2DROP SWAP >R >R DROP
    R> 100 /   R> 100 /MOD ;

\ Return current  SEC MIN HOUR DAY MONTH YEAR
: TIME&DATE TIME DATE ;
\ In fact a check is in order whether the day has changed
: TIME&DATE DATE >R >R >R   TIME DATE 2DROP DUP R> = IF
    R> R> ELSE RDROP RDROP 2DROP 2DROP RECURSE THEN ;
DECIMAL
( DEADBEEF leading_hex_digit) CF: \ AvdH A5jun28
\ Are denotations starting with "Z" already known?
"A" 'ONLY >WID (FIND) SWAP DROP SWAP DROP ?LEAVE-BLOCK
\ Apparently not, so:
WANT ALIAS
CURRENT @   'ONLY >WID CURRENT !  '3
\ Make sure Forth understands DEADBEEF as a number
    DUP ALIAS A   DUP ALIAS B   DUP ALIAS C   DUP ALIAS D
    DUP ALIAS E   DUP ALIAS F
DROP   CURRENT !




\
\ Use  'ONLY >WID CURRENT ! instead of DEFINITIONS
( DEADHORSE leading_hex_digit) CF: \ AvdH A5jun28
\ Are denotations starting with "Z" already known?
"Z" 'ONLY >WID (FIND) SWAP DROP SWAP DROP ?LEAVE-BLOCK
\ Apparently not, so:
WANT ALIAS
CURRENT @   'ONLY >WID CURRENT !  '3
\ Make sure Forth understands DEADHORSE as a number
(   DUP ALIAS A   DUP ALIAS B   DUP ALIAS C   DUP ALIAS D    )
(   DUP ALIAS E   DUP ALIAS F ) DUP ALIAS G   DUP ALIAS H
    DUP ALIAS I   DUP ALIAS J   DUP ALIAS K   DUP ALIAS L
    DUP ALIAS M   DUP ALIAS N   DUP ALIAS O   DUP ALIAS P
    DUP ALIAS Q   DUP ALIAS R   DUP ALIAS S   DUP ALIAS T
    DUP ALIAS U   DUP ALIAS V   DUP ALIAS W   DUP ALIAS X
    DUP ALIAS Y   DUP ALIAS Z
DROP   CURRENT !
\ Use  'ONLY >WID CURRENT ! instead of DEFINITIONS
( TUCK -ROT PICK ROLL )                          \ AvdH B2sep25
\ Obscure stack manipulations.
: PICK 1+ CELLS DSP@ + @ ;
: TUCK SWAP OVER ;
: -ROT ROT ROT ;
: ROLL   >R DSP@ DUP CELL+ R> 2 - CELLS
    2DUP + @ >R CELL+ MOVE DROP R> ;









( 2>R 2R> 2R@ 2CONSTANT 2VARIABLE 2, )          \ AvdH B5Mar9




\ Double return stack manipulations all ISO
: 2>R POSTPONE SWAP POSTPONE >R POSTPONE >R ; IMMEDIATE
: 2R> POSTPONE R>  POSTPONE R> POSTPONE SWAP  ;  IMMEDIATE
: 2R@ POSTPONE 2R> POSTPONE 2DUP POSTPONE 2>R ; IMMEDIATE

: 2,   , , ;                            \ ISO
: 2VARIABLE   DATA 0. 2, ;              \ ISO
: 2CONSTANT   CREATE 2, DOES> 2@ ;      \ ISO


\
( READ-LINE GETCHAR ) CF: ?LI                 \ AvdH A5jun28
\ From HANDLE get : a CHAR. Errors are thrown , 6=eof.
: GETCHAR >R 0 DSP@ 1 R@ READ-FILE THROW 0=  6 AND THROW
    DUP ^M = IF DROP R@ RECURSE THEN RDROP ;

: eol? ^J = ; \ For CHAR : "it IS a line end".

\ To BUFFER read COUNT chars from HANDLE. Leave free ADDRESS.
: (READ-LINE) SWAP BEGIN >R >R   R@ GETCHAR   2DUP SWAP C!
    eol? IF R> R> DROP 0 ELSE 1+ R> R> 1- THEN
    DUP  WHILE REPEAT  2DROP ;
\ To BUFFER read a line of at most COUNT char's from HANDLE.
\ Leave actual COUNT, "chars REMAIN", ERROR.
: READ-LINE   ROT DUP >R ROT ROT   '(READ-LINE) CATCH
   DUP 6 = IF 2DROP 2DROP 0 0 0 ELSE   DUP IF 0 0 ROT ELSE
   DROP   R@ - -1 0 THEN THEN   RDROP ;
( WRITE-LINE PUTCHAR ) CF: ?LI                \ AvdH A5jun28

\ Linux file ending.
DATA CR$ 1 , ^J C,

\ Output CHAR to HANDLE. Errors are thrown.
: PUTCHAR >R  DSP@ 1 R> WRITE-FILE THROW DROP ;

\ Write a line from BUFFER COUNT characters to HANDLE.
\ Errors are thrown.
: (WRITE-LINE)   DUP >R   WRITE-FILE THROW
    CR$ $@ R> WRITE-FILE THROW ;

\ Write a line from BUFFER COUNT characters to HANDLE.
\ Leave actual ERROR.
: WRITE-LINE '(WRITE-LINE) CATCH DUP IF >R 2DROP DROP R> THEN ;
( COMPARE $= BOUNDS ALIGN UNUSED ) \ AvdH A1oct04
\ ISO
 : COMPARE ROT 2DUP SWAP - >R
     MIN CORA DUP IF RDROP ELSE DROP R> THEN ;
\ For STRING1 and STRING2 : "they ARE equal".
: $= ROT OVER = IF CORA 0 = ELSE DROP DROP DROP 0 THEN ;
\ In general use
: BOUNDS   OVER + SWAP ;

: UNUSED DSP@ HERE - ;
CF:
"ALIGNED" PRESENT ?LEAVE-BLOCK
\ ISO
: ALIGNED    1-   0 CELL+ 1- OR   1+ ;
: ALIGN   DP @   ALIGNED   DP ! ;
\
( --manifest TRUE FALSE NULL NULL$ NONE R/O W/O R/W ) \ AvdH
\ Define some manifest constants.
-1 CONSTANT TRUE       \ Flag
0 CONSTANT FALSE       \ Flag
0 CONSTANT NULL        \ Invalid address
: NULL$ 0 0 ;          \ Invalid string
-1 CONSTANT NONE       \ Invalid index, where valid is pos.


0 CONSTANT R/O
1 CONSTANT W/O
2 CONSTANT R/W



\
( 0> U.R ) \ AvdH A2oct23




: 0> 0 > ;
: U.R 0 SWAP D.R ;









( DEFER IS ) \ AvdH B4feb28

\ Default action for not filled in deferred word.
: DEFER-ERROR    -1 9 ?ERROR ;

\ Define a word, with a changable behaviour. "deferred" word".
: DEFER CREATE 'DEFER-ERROR , DOES> @ EXECUTE ;

\ Fill XT in as the behaviour of the named deferred word.
\ This ugly word is state-smart!
: IS   NAME FOUND >BODY   STATE @ IF
    POSTPONE LITERAL POSTPONE ! ELSE ! THEN ; IMMEDIATE



\
( [IF] ] [ELSE] [THEN] [DEFINED] [UNDEFINED] ) \ AvdH A7feb20
WANT COMPARE
\ From ANSI manual.
: SKIPPING
1 BEGIN NAME DUP WHILE
   2DUP "[IF]" COMPARE 0= IF 2DROP 1+ ELSE
   2DUP "[ELSE]" COMPARE 0= IF 2DROP 1- DUP IF 1+ THEN ELSE
        "[THEN]" COMPARE 0= IF 1- THEN THEN THEN
   ?DUP 0= IF EXIT THEN
REPEAT 2DROP DROP ;
: [IF] 0= IF SKIPPING THEN ; IMMEDIATE
: [ELSE] SKIPPING ; IMMEDIATE
: [THEN] ; IMMEDIATE

: [UNDEFINED] NAME 2DUP WANTED PRESENT 0= ; IMMEDIATE
: [DEFINED] POSTPONE [UNDEFINED] 0= ; IMMEDIATE
( VALUE TO FROM ) \ AvdH B2aug07


VARIABLE TO-MESSAGE   \ 0 : FROM ,  1 : TO .
DATA _value_jumps  '@ , '! , '+! ,
: FROM 0 TO-MESSAGE ! ;
\ ISO
: TO  1 TO-MESSAGE ! ;
\ Signal that we want to add to value
: +TO 2 TO-MESSAGE ! ;

\ ISO
: VALUE CREATE , DOES> _value_jumps TO-MESSAGE @ CELLS +
    @ EXECUTE   FROM ;


( LOCAL )                               \ AHCH B5mar03
WANT VALUE      WANT {{    WANT BAG     WANT DO-BAG

16 BAG _locals   _locals !BAG

: LOCAL  POSTPONE {{   _ VALUE   }}
    POSTPONE TO   LATEST >LFA @ DUP _locals BAG+!
    POSTPONE LITERAL POSTPONE EXECUTE
; IMMEDIATE

: ;   _locals DO-BAG I @ HIDDEN LOOP-BAG POSTPONE ;
   _locals !BAG ;
IMMEDIATE



( ORDER .WID .VOCS BUFFER ) \ AvdH B5apr8
\ Print all namespace (voc) names in existence.
: .VOCS 'ID. FOR-VOCS ;
\ Print a voc's name from the WID)
: .WID 0 CELL+ - BODY> ID. ;
\ Print the current search order by namespace names
: ORDER  CONTEXT BEGIN $@ DUP 'ONLY >WID <> WHILE .WID REPEAT
    2DROP &[ EMIT SPACE CURRENT @ .WID &] EMIT ;







\
( PAGE AT-XY INVIS REVERSE )                    \ AvdH B5Feb25
HEX      WANT HEX:       WANT .FORMAT
FORMAT-WID DEFINITIONS
: e 1B CRS$ $C+ ;      \ Print escape
: dd DEC: 0  <# #S #> CRS$ $+! ; \ Print N no space.
PREVIOUS DEFINITIONS
\ Define and execute simple escape commands
: esc-seq   CREATE $, DROP DOES> 1B EMIT $@ TYPE ;
"[2J"   esc-seq CLEAR  \ Clear Page
"[H"    esc-seq HOME   \ Set cursor home
"[?25l" esc-seq INVIS  \ Make cursor invisible
"[?25h" esc-seq CVVIS  \ Make cursor visible
"[0m" esc-seq NORMAL
"[7m" esc-seq REVERSE
: PAGE   HOME CLEAR ;   \ ISO
: AT-XY   1+ SWAP 1+ SWAP "%e [%dd ;%dd H" .FORMAT ; \ ISO
( BAG !BAG BAG? BAG+! BAG@- BAG-REMOVE BAG-HOLE BAG-INSERT )
\ Warning uses $@ as as @+
( Build a bag with X items. )
: BUILD-BAG   HERE CELL+ , CELLS ALLOT ;
( Create a bag "x" with X items. )
: BAG   DATA HERE CELL+ , CELLS ALLOT DOES> ;
: !BAG   DUP CELL+ SWAP ! ;   ( Make the BAG empty )
: BAG?   $@ = 0= ;   ( For the BAG : it IS non-empty )
: BAG+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Push ITEM to the BAG )
: BAG@- 0 CELL+ NEGATE OVER +! @ @ ;   ( From BAG: pop ITEM )
: BAG-REMOVE    ( Remove entry at ADDRESS from BAG. )
>R   DUP CELL+ SWAP  OVER R@ @ SWAP -   MOVE -1   CELLS R> +! ;
: BAG-HOLE      ( Make hole at ADDRESS in BAG. )
>R   DUP CELL+   OVER R@ @ SWAP -   MOVE   0 CELL+ R> +! ;
( Insert VALUE at ADDRESS in BAG. )
: BAG-INSERT   OVER SWAP BAG-HOLE   ! ;
( |BAG| DO-BAG .BAG BAG-WHERE IN-BAG? BAG- SET+ SET- ) \ AvdH
: |BAG|   $@ SWAP - 0 CELL+ / ; ( For BAG : NUMBER of items )
\ Loop over a bag, see ``.BAG'' for an example.
: DO-BAG  POSTPONE $@ POSTPONE SWAP POSTPONE ?DO ; IMMEDIATE
: LOOP-BAG 0 CELL+ POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
: .BAG   DO-BAG I ? LOOP-BAG ; ( Print BAG )
( For VALUE and BAG : ADDRESS of value in bag/nill.)
: BAG-WHERE DO-BAG DUP I @ = IF DROP I UNLOOP EXIT THEN
    LOOP-BAG  DROP 0 ;
( For VALUE and BAG : value IS present in bag.)
: IN-BAG? BAG-WHERE 0= 0= ;
( Remove VALUE from BAG. )
: BAG-   DUP >R   BAG-WHERE   R> BAG-REMOVE ;
( Add/remove VALUE to bag, used as a SET, i.e. no duplicates.)
: SET+   2DUP IN-BAG? IF 2DROP ELSE BAG+! THEN ;
: SET-   2DUP IN-BAG? IF BAG- ELSE 2DROP THEN ;
( F: auxiliary_for_struct ) \ AH A4jun16
4096 CONSTANT LEN
DATA NAME$ 128  ALLOT         \ The name of the struct.
DATA CRS$ LEN ALLOT  : !CRS$ 0 CRS$ ! ; \ Generate struct
DATA DOES>$ LEN ALLOT   \ Generate fields/methods.
\ Add STRING and the name of the current struct to CRS$.
: +NAME$   CRS$ $+!    NAME$ $@ CRS$ $+!   BL CRS$ $C+ ;
VARIABLE LAST-IN         VARIABLE start
: RLI PP @ LAST-IN ! ; \ Remember last value of ``PP''.
: GLI >R LAST-IN @ PP @  R> - OVER - ; \ Input since RLI trim.
: itoa 0 <# #S BL HOLD #> ; \ Transform an INT to a STRING.
\ Add the first part of a definition of a field to DOES>$.
: F:   " : " DOES>$ $+! NAME DOES>$ $+!   RLI
  HERE start @ - itoa DOES>$ $+!   " ^" DOES>$ $+!
  NAME$ $@ DOES>$ $+!    " @ + " DOES>$ $+! ;

( FORMAT FORMAT&EVAL .FORMAT )          \ AH&CH B4Oct16
WANT 2>R   DATA CRS$ 4096 ALLOT \  WANT :2
NAMESPACE FORMAT-WID           FORMAT-WID DEFINITIONS
: c CRS$ $C+ ;  : n ^J c ;   : r ^M c ;  \ Add single char's
: d S>D 0 (D.R) CRS$ $+! ;  \ Add INT as a string.
: s CRS$ $+! ;             \ Add a STRING as such.
PREVIOUS DEFINITIONS
\ Format the first part of STRING, up till %, leave REST.
: _plain    &% $/ CRS$ $+! ;
\ Format X with first word of STRING, up till BL, leave REST.
: _format   BL $/ 2SWAP 2>R 'FORMAT-WID >WID (FIND) NIP NIP
    DUP 0= 51 ?ERROR EXECUTE 2R> ;
\ Format X1 .. Xn using the format STRING.
: FORMAT 0 CRS$ ! BEGIN DUP WHILE _plain DUP IF _format THEN
    REPEAT 2DROP CRS$ $@ ;
: FORMAT&EVAL   FORMAT EVALUATE ;   : .FORMAT FORMAT TYPE ;
( class endclass M: M; ) \ AH B2jul17
WANT SWAP-DP WANT FORMAT  VARIABLE LAST-IN   VARIABLE DP-MARKER
DATA NAME$ 128 ALLOT          DATA BLD$ 4096 ALLOT
: -WORD 1- BEGIN 1- DUP C@ ?BLANK UNTIL 1+ ; ( in -- firstch)
: {BLD   PP @ LAST-IN ! ;
\ Retain input since {BLD excluding word name.
: BLD}   LAST-IN @   PP @ -WORD   OVER -   BLD$ $+! ;
: class   NAME NAME$ $!   NAME$ $@ "VARIABLE ^%s" FORMAT&EVAL
   "" BLD$ $! SWAP-DP   HERE DP-MARKER !   {BLD ;
: M:   BLD}   HERE DP-MARKER @ - >R   SWAP-DP :
    R> NAME$ $@ "^%s  @ %d  +" FORMAT&EVAL ;
: M;   POSTPONE ;   SWAP-DP   {BLD ; IMMEDIATE
: endclass   BLD}  DP-MARKER @ HERE - ALLOT   SWAP-DP
  BLD$ $@ NAME$ $@ ": BUILD-%s  HERE >R %s  R> ;" FORMAT&EVAL
  NAME$ $@ 2DUP 2DUP 2DUP
  ": %s  CREATE BUILD-%s  ^%s  ! DOES> ^%s  ! ;" FORMAT&EVAL ;
( :NONAME CASE MARKER )
WANT POSTFIX
: :NONAME "NONAME" POSTFIX : LATEST DUP HIDDEN !CSP ; \ ISO

\ ISO
: CASE 0 ; IMMEDIATE
: OF   POSTPONE OVER POSTPONE =
    POSTPONE IF POSTPONE DROP ; IMMEDIATE
: ENDOF POSTPONE ELSE ; IMMEDIATE
: ENDCASE POSTPONE DROP BEGIN DUP WHILE POSTPONE THEN REPEAT
  DROP ; IMMEDIATE

\ ISO
: MARKER HERE CREATE , DOES> @ 'FORGET-VOC FOR-VOCS DP ! ;


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
value. It may be  TRUE , FALSE or undefined for `IMAX' , but
it must be monotonic down in the range [IMIN,IMAX), i.e.
if IMIN<=IX<=IY<IMAX then if IX COMP gives false, IY COMP
cannot give true.

BIN-SEARCH finds the first index `IT' between `IMIN'
and `IMAX' (exclusive) for which `COMP' returns false.
or else ``IMAX''.
An empty range is possible, (`IMIN' and `IMAX' are equal.)
See also  binary_search_test in the examples section.
( MEMOIZE                                       ) \ AvdHB4dec02
WANT BAG                WANT |BAG|              WANT BIN-SEARCH
1000 CONSTANT CSIZE
CSIZE BAG keys          CSIZE BAG values
\ Reset and reallocate to N items, for zero, just reset.
: INIT-MEMOIZE   DROP keys !BAG  values !BAG ;
: key[]   1+ CELLS keys + ;    \ For INDEX return ADDRESS
VARIABLE _compand
: _compson key[] @ _compand @ < ;
\ For COMPARAND find: POSITION where it is to be placed.
: find-index   _compand !   0 keys |BAG| '_compson BIN-SEARCH ;
\ The memoizer works on a one input one output function.
: MEMOIZE DUP find-index key[] 2DUP @ = IF keys - values + @
  NIP RDROP ELSE   DROP DUP  CO SWAP DUP find-index key[] >R
  R@ keys BAG-INSERT   DUP R@ keys - values + values BAG-INSERT
  RDROP THEN ;
( EXCHANGE PAIR[] SORT-B SORT-X ) \ AvdH B3dec22
WANT QSORT
\ Exchange the content at ADDR1 and ADDR2 over a fixed LENGTH.
: EXCHANGE 0 ?DO   OVER I +     OVER I +  OVER C@   OVER C@
                   >R SWAP C!  R> SWAP C! LOOP 2DROP ;
\ For INDEX1 and INDEX2 and TABLE, return ADDR1 and ADDR2 .
: PAIR[] >R   CELLS R@ + SWAP   CELLS R@ + SWAP   RDROP ;
\ For TABLE and LENGTH sort cells, must be aligned, len>1
: SORT-X SWAP 1 CELLS / SWAP 1- OVER + '_x<_ '_x<--> QSORT ;

VARIABLE _table_         VARIABLE _blen_
: _l_  _blen_ @ * _table_ @ + ;
: _b<_ _l_ SWAP _l_ _blen_ @ CORA 0 > ;
: _b<-->  _l_ SWAP _l_ _blen_ @ EXCHANGE ;
\ For TABLE and LENGTH BLEN sort bytes in collating order.
: SORT-B _blen_ ! >R _table_ ! 0 R> 1- '_b<_ '_b<--> QSORT ;
( QSORT 1 ) \ AvdH B3dec22
WANT DEFER
\ Compare item N1 and N2. Return ``N1'' IS lower and not equal.
DEFER *<
\ Exchange item N1 and N2.
DEFER *<-->

\ Comparison if addresses used as indices.
: _x<_ CELLS @ SWAP CELLS @   > ;
\ Exchanges if addresses used as indices.
: _x<--> CELLS SWAP CELLS OVER @ OVER @ >R SWAP ! R> SWAP ! ;





( QSORT 2 ) \      AvdH A7feb28
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
( QSORT SORT ) \ AvdH A7feb28
\ Sort the range LOW to HIGH inclusive observing
\ ``LOW'' and ``HIGH'' must be indices compatible with the
\   current values of *< and *<-->
: (QSORT)             ( lo hi -- )
    PARTITION                ( lo_1 hi_1 lo_2 hi_2)
    2DUP < IF  RECURSE  ELSE  2DROP  THEN
    2DUP < IF  RECURSE  ELSE  2DROP  THEN ;
\ Sort the range FIRST to LAST (inclusive) of item compared
\ by the xt COMPARING and exchanged by the xt EXHANGING.
\ The xt's are filled in into *< and *<--> and must observe the
\ interface. All indices be compatible with these xt's.
\ After the call we have that :
\ ``For FIRST<=I<J<=LAST      I J *< EXECUTE leaves TRUE.''
: QSORT   IS *<-->   IS *<   (QSORT) ;
: SORT    IS *<-->   IS *<   1- (QSORT) ; \ Exclusive
\ (MERGE)       \ AvdH A3dec02
\ list : sorted   ulist : unsorted   listp : list level
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

( MERGE-SORT )  WANT (MERGE)  \ AvdH A3dec02

\ Merge LIST1 and LIST2, leave merged LIST.
: MERGE   LL< IF SWAP THEN   DUP >R (MERGE) R> ;
\ Cut ULIST in two. Return LIST and remaining ULIST.
: SNIP DUP   BEGIN DUP >N   DUP IF LL< ELSE 0 THEN
    WHILE SWAP DROP REPEAT   >R 0 SWAP LINK! R> ;
\ Keep on merging as long as two listp 's have the same level.
: TRY-MERGES   BEGIN >R  OVER R@ =
    WHILE SWAP DROP MERGE R> 1+ REPEAT R> ;
\ Expand zero, ulist into zero list, level .... list, level
: EXPAND BEGIN SNIP >R 1 TRY-MERGES R> DUP WHILE REPEAT DROP ;
\ Keep on merging list-level pairs until end-sentinel.
: SHRINK DROP BEGIN OVER WHILE SWAP DROP MERGE REPEAT ;
\ For compare XT, next XT, linked LIST , leave a sorted LIST1.
: MERGE-SORT   *->M !  *<M !   0 SWAP EXPAND SHRINK SWAP DROP ;
( SORT-WID SORT-VOC )           \ AvdH A3dec02
WANT COMPARE         WANT MERGE-SORT
\ Note that xt's form a linked list
\ For XT1 and XT2 return XT1 and XT2 plus "xt IS lower".
    : GET-NAME >NFA @ $@   ;  \ Aux. For EL, return NAME.
: NAMES< DUP >R OVER GET-NAME    R> GET-NAME    COMPARE 0 < ;

\ Sort the WID alphabetically on names.
\ A wid is a head of a list and doesn't take part in the
\ sorting (expect for the link field) , so it may be a dummy.
: SORT-WID >LFA DUP >R   @ 'NAMES< '>LFA MERGE-SORT   R> ! ;

\ Sort the namespace given its namespace XT.

: SORT-VOC >WID SORT-WID ;

( CRC-MORE CRC ) CF:                    \ AvdH
1 CELLS 4 < ?LEAVE-BLOCK
WANT BOUNDS   WANT NEW-IF    HEX
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
: GET DEBUG-STACK @ 1 CELLS - @ ;










( SQRT SQ SQ? ** )                               \ AvdH B4dec01
\ For N return FLOOR of the square root of n.
: SQRT  DUP >R 10 RSHIFT 1024 MAX  \ Minimize iterations.
   BEGIN R@ OVER / OVER + 1 RSHIFT  2DUP > WHILE
    SWAP DROP REPEAT   DROP RDROP ;

: SQ DUP * ;

\ FOR N:"it IS a square."
: SQ?   DUP SQRT SQ = ;

\ For A and N, return a to the POWER n.
: ** 1 SWAP 0 ?DO OVER * LOOP NIP ;



( #BITS )  CF:                                   \ AvdH B4dec01
8 BASE !
3333333333333333333333 CONSTANT _select-2/3
0707070707070707070707 CONSTANT _select-odd

\ For MASK, return how MANY bits are up.
: #BITS   DUP 1 RSHIFT _select-2/3 AND
  DUP 1 RSHIFT _select-2/3 AND + -
  DUP 3 RSHIFT + _select-odd AND
    0 100 UM/MOD   0 77 UM/MOD DROP   + ;





DECIMAL
( +m -m *m /m **m x^x )              \ AvdH B4nov2
WANT :I WANT XGCD      VARIABLE m ( Modulo number)
\ suffix n : normalized number.
:I _norm_-m  DUP 0< m @ AND + ; ( x -- xn ) \ -m<xn<+m
:I +m   + m @ - _norm_-m  ;   ( an bn -- sumn )
:I -m   - _norm_-m  ;         ( an bn -- diffn)
:I *m   M* m @ SM/REM DROP ;  ( a b -- prodn) \ a>=0 b>=0
:I /m    m @ XGCD DROP _norm_-m  *m ; ( a b -- quotn)

\ Both steps: For A B and C: return A B en C.  Invariant A*B^C.
:I reduce_1-  1- >R >R    R@ *m   R> R> ;
:I reduce_2/   2/ SWAP   DUP *m   SWAP ;
:I **m    1 ROT ROT BEGIN   DUP 1 AND IF   reduce_1-   THEN
    reduce_2/ DUP 0= UNTIL   2DROP   ;  ( a b -- apowbn )

:I x^x    m @ >R m !   **m   R> m ! ;   ( a b m -- a^b mod m )
( PRIME? FACTOR GCD XGCD                       ) \ AvdH B4Nov03
\ For N and HINT return FACTOR >= hint, maybe n.
: FACTOR   BEGIN   2DUP /MOD SWAP
    0= IF DROP SWAP DROP EXIT THEN
    OVER < IF DROP EXIT THEN
    1+ 1 OR AGAIN ;

\ For N return: "It IS prime" ( Cases 0 1 return FALSE)
: PRIME?   DUP 4 < IF 1 > ELSE DUP 2 FACTOR = THEN ;

\ For M N , return their GCD.
: GCD   BEGIN OVER MOD DUP WHILE SWAP REPEAT DROP ;

\ For A B return C GCD where C*A+B*x=GCD
: XGCD 1 0 2SWAP   BEGIN OVER /MOD OVER WHILE >R SWAP
   2SWAP OVER R> * - SWAP 2SWAP REPEAT 2DROP NIP ;
( /STRING -LEADING DROP-WORD        )           \ AvdH B@aug12

\ From SC trim N char's
: /STRING >R  R@ - SWAP R> + SWAP ;

\ Like -TRAILING sc-sc
 : -LEADING BEGIN OVER C@ ?BLANK OVER 0= 0=  AND
    WHILE 1 - SWAP 1 + SWAP REPEAT  ;

\ From a STRING remove the first word. Leave the rest STRING.
: DROP-WORD   -LEADING BL $/ 2DROP ;
\ : DROP-WORD   BEGIN BL $/ 0= WHILE DROP REPEAT DROP ;




( RAND ) CF: ?32         \ EDN 1991JAN21, pg 151 \ AvdH B2aug12
WANT TICKS
VARIABLE SEED       HEX
( . -- . ) ( Use the nanosecond counter to start)
: RANDOMIZE TICKS DROP SEED ! ;

( -- N  Leave a random number )
: RAND SEED @ 107465 * 234567 + DUP SEED ! ;

( N -- R Leave a random number < N)
: CHOOSE RAND UM* SWAP DROP ;


( RANDOM-SWAP ( R N -- )
( 1 - CHOOSE 1+ CELLS OVER + @SWAP ;)  DECIMAL
RANDOMIZE
( RAND ) CF: ?64         \ EDN 1991JAN21, pg 151 \ AvdH B2aug12
WANT TICKS        HEX
VARIABLE SEED
( . -- . ) ( Use the nanosecond counter to start)
: RANDOMIZE TICKS DROP SEED ! ;

( -- N  Leave a random number 32 bits )
: RAND SEED @ 107,465 * 23,4567 + 0FFFF,FFFF AND DUP SEED ! ;

( N -- R Leave a random number < N)
: CHOOSE RAND * 20 RSHIFT ;


( RANDOM-SWAP ( R N -- )
( 1 - CHOOSE 1+ CELLS OVER + @SWAP ;)  DECIMAL
RANDOMIZE
( @SWAP @+ )                                    \ AvdH B4nov06
WANT ALIAS

'$@ ALIAS @+

( Swap the number at ADDRESS1 with one at ADDRESS2 )
: @SWAP  OVER @   OVER @   SWAP   ROT !   SWAP ! ;

\ maybe !+







( **************Non ISO language extension *******************)


Contains language extensions that are not ISO in the sense
that they are neither a standard word nor implemented using
standard words.
We put here also reference implementations.









( ?EXIT ?LEAVE )                        \ AvdH B2aug12
\ Exit current definitions if FLAG.
: ?EXIT   IF RDROP THEN ;
\ Exit current loop if FLAG.
: ?LEAVE  IF RDROP RDROP RDROP THEN ;










\
( DEPTH $@ $! $+! $C+ )                 \ AvdH B5feb27
: DEPTH   S0 @ DSP@ -   [ 0 CELL+ ] LITERAL /   1- ;
\ Fetch a constant string c from s
: $@ ( s -- cs ) DUP CELL+ SWAP @ ;
\ Store a constant string cs into s
: $! ( cs s -- ) 2DUP ! CELL+ SWAP CMOVE ;
\ Store a brain-dead string cs into s
: $!-BD ( cs s -- ) 2DUP C! 1+ SWAP CMOVE ;
\ Append a constant string cs to s
: $+! ( cs s -- ) DUP @ >R 2DUP +! CELL+
    R> CHARS +   SWAP CMOVE ;
\ Append a character char to s
: $C+ ( char s -- ) DUP >R DUP @ CHARS + CELL+ C!
   1 R> +! ;


( OBSOLETE L! L@ )       \ AvdH A6may13
\ Use of these words in modern programs, i.e. released
\ after 2006 may, may be risky.
\ ``want L!'' may not work, because L! is defined as 32 bit
\ : L! FAR! ;
\ : L@ FAR@ ;









\
( $-PREFIX #-PREFIX  ESC ) \ AvdH A1apr15



\ ONLY DEFINITIONS ... PREVIOUS doesn't work because ONLY
\ terminates the search order.
'ONLY >WID CURRENT !
\ Define $ as a prefix for hex.
: $ BASE @ >R HEX (NUMBER) R> BASE ! POSTPONE SDLITERAL ;
PREFIX IMMEDIATE
\ Define # as a prefix for decimal.
: # BASE @ >R DECIMAL (NUMBER) R> BASE ! POSTPONE SDLITERAL ;
PREFIX IMMEDIATE
DEFINITIONS

$1B CONSTANT ESC
( +THRU ) \ AvdH A1oct05
\ Load current block plus N1 to current block plus N2.
: +THRU   SRC @ 2 CELLS - @ >R
    R@ + SWAP   R> + SWAP
    THRU ;











(  H. B. DH. BASE? HEX: ) \ AvdH B2mar13
\ Switch to hex for the duration of the definition.
: HEX:    R> BASE @ >R  >R HEX CO R> BASE ! ;
: DEC:    R> BASE @ >R  >R DECIMAL CO R> BASE ! ;
( Add a , after 4 digits )
 : 4?  1+ 4 MOD 0= IF &, HOLD THEN ;
 : 3?  1+ 3 MOD 0= IF &, HOLD THEN ;
( Generate string with hex format of DOUBLE of LEN digits)
 : (DH.) HEX: <# 1- 0 ?DO # I 4? LOOP # #> ;
 : B.  S>D 2 (DH.) TYPE ; ( print BYTE in hex )
 : H.  S>D 2 CELLS (DH.) TYPE ; ( print SINGLE in hex )
 : DH. 4 CELLS (DH.) TYPE ; (  print DOUBLE in hex )
(  print DOUBLE in decimal )
 : DEC. 5 CELLS DEC: <# 1- 0 ?DO # I 3? LOOP # #> TYPE ;
 : BASE?  BASE @ B. ;                ( 0/0 TRUE VALUE OF BASE)

( HIDE INCLUDE IVAR )            \ AvdH B4Dec03



: HIDE NAME FOUND DUP 0= 11 ?ERROR HIDDEN ;


: IVAR DATA , ;





: INCLUDE NAME INCLUDED ;


(  ALIAS )                       \ AvdH B4Dec03

: ALIAS  NAME (CREATE) LATEST 3 CELLS MOVE ;













( SLITERAL $. $? ."$" )           \ AvdH B2aug12

\ ISO
: SLITERAL POSTPONE SKIP $, POSTPONE LITERAL POSTPONE $@ ;
IMMEDIATE
\ ISO





 : $. TYPE ;  \ Print a STRING constant.
 : $? $@ $. ;  \ Print a string at ADDRESS.
\ Print STRING, as a quoted string, reconsumable.
: ."$" BEGIN &" $/ &" EMIT TYPE &" EMIT OVER 0= UNTIL 2DROP ;
\
( SCAN-WORD DOC ANEW )                          \ AvdH B2aug18
WANT 2>R      WANT MARKER

\ Like name but abort on eof.
: _name NAME DUP 0= 13 ?ERROR ;
\ Skip words until and including STRING.
: SCAN-WORD 2>R BEGIN BEGIN _name R@ <> WHILE DROP REPEAT
   2R@ CORA WHILE REPEAT RDROP RDROP ;
: DOC "ENDDOC" SCAN-WORD ;  \ Skip till "ENDDOC".

\ Destructive MARKER
: ANEW PP @ >R NAME FOUND DUP IF EXECUTE _ THEN DROP
  R> PP ! MARKER ;


\
( TICKS PAST? ) CF: ?64 \ AvdH A2oct21
\ We can't use the assembler in 64 bits.
WANT ASSEMBLER HEX    \ Just for using CODE

CODE (TICKS)
    0F C, 31 C, \ RDTSC,
    50 C, 52 C, \ PUSH|X, AX| PUSH|X, DX|
    48 C, AD C, FF C, 20 C,   \  NEXT,
END-CODE   \ Code now in 2 32 bit things.
\ Leave a DOUBLE value.
: TICKS   (TICKS) 20 LSHIFT OR   0 ;

\ For a TIME in ticks: it IS in the past.
: PAST? DNEGATE TICKS D+ SWAP DROP 0< 0= ;
DECIMAL

( TICKS PAST? ) CF: ?32 \ AvdH A2oct21
\ Assuming we run on an 486 or better, and a 32 bits Forth
WANT ASSEMBLERi86-HIGH HEX
CODE  TEST-EF POPF, PUSHF, NEXT, END-CODE
DECIMAL
1 21 LSHIFT CONSTANT ID-FLAG

ID-FLAG TEST-EF 0 TEST-EF XOR ID-FLAG AND 0= ?LEAVE-BLOCK
HEX CODE CPUID POP|X, AX| CPUID, PUSH|X, DX| NEXT, END-CODE
1 CPUID 10 AND 0= DECIMAL ?LEAVE-BLOCK HEX
CODE TICKS RDTSC, PUSH|X, AX| PUSH|X, DX| NEXT, END-CODE

TRIM \ The assembler
\ For a TIME in ticks: it IS in the past.
: PAST? DNEGATE TICKS D+ SWAP DROP 0< 0= ;
DECIMAL
( TICKS-PER-SECOND ) CF: \ AvdH A7feb28

"MS" PRESENT 0= ?LEAVE-BLOCK

DECIMAL TICKS DNEGATE 1000 MS TICKS D+ DROP
   CONSTANT TICKS-PER-SECOND










( TICKS-PER-SECOND ) \ AvdH A2oct21





." What is the speed of your Pentium (in Mhz)?"
PAD DUP 80 ACCEPT EVALUATE 1000000 *
  CONSTANT TICKS-PER-SECOND







( MS@ ) CF: ?LI \ AvdH B3apr23
WANT -syscalls-



DECIMAL

: MS@ 0 0 0 __NR_times XOS 10 * ;








( MS@ ) \ AvdH A9jun8
WANT TICKS   "TICKS" PRESENT 0= ?LEAVE-BLOCK
WANT TICKS-PER-SECOND


: MS@ TICKS TICKS-PER-SECOND 1000 / M/MOD DROP SWAP DROP ;










( MARK-TIME .mS .uS ELAPSED )     \ AvdH A9jun08
WANT TICKS   "TICKS" PRESENT 0= ?LEAVE-BLOCK
WANT TICKS-PER-SECOND
DECIMAL
\ Mark a point in time by leaving its tick COUNT.
: MARK-TIME   TICKS ;
\ Print a TIME interval, given in uS as ms.
: .mS   SPACE 0 <# # # # [CHAR] . HOLD #S #> TYPE ." mS "  ;
\ Print a TIME interval, given in uS, as us.
: .uS SPACE . ." uS "  ;
\ For the TIME (in ticks) on the stack return ELAPSED time
\ since then, in uS.
: ELAPSED   DNEGATE TICKS D+ DROP   1,000,000 TICKS-PER-SECOND
    DUP 0< IF 1 RSHIFT SWAP 1 RSHIFT SWAP THEN */ ;
DECIMAL

( MEASURE-PRIME test_for_TIME ) \ AvdH A1oct05
 : TASK ;

WANT DO-PRIME-ISO   WANT MARK-TIME


: MEASURE-PRIME
  TICKS DO-PRIME-ISO DROP ELAPSED
  CR  ." THE ISO BYTE BENCHMARK LASTED " .mS  ;

  MEASURE-PRIME

CR ." FORGET ``MEASURE-PRIME'' Y/N" KEY DUP EMIT
&Y <>  ?LEAVE-BLOCK
  FORGET TASK

( SWAP-DP T] T[ scratch_dictionary_area ) \ AvdH A9mar31
VARIABLE FAR-DP         \ Alternative DP
DSP@ 1 RSHIFT HERE 1 RSHIFT + ALIGNED FAR-DP !
\ Use alternative dictionary area or back.
: SWAP-DP   DP @ FAR-DP @   DP ! FAR-DP ! ;
\ Remove all words from the scratch area.
: TRIM   HERE 'FORGET-VOC FOR-VOCS DROP ;

\ While compiling, T[ just throws away the state pushed by T].
\ Interpreting:
\ Start compiling at temporary place : return START and STATE.
: T] STATE @ 0= IF SWAP-DP HERE THEN STATE @ ] ;
\ Execute code at START dropping STATE, restore dictionary.
: T[ 0= IF POSTPONE (;) SWAP-DP POSTPONE [ >R THEN ; IMMEDIATE


( NEW-IF -scripting-                           ) \ AvdH B2sep21
: NEW-IF ;    \ Get rid!
WANT T]      WANT :2
:2 IF           T] POSTPONE IF                    ; IMMEDIATE
:2 DO           T] POSTPONE DO                    ; IMMEDIATE
:2 ?DO          T] POSTPONE ?DO                   ; IMMEDIATE
:2 BEGIN        T] POSTPONE BEGIN                 ; IMMEDIATE
:2 THEN            POSTPONE THEN      POSTPONE T[ ; IMMEDIATE
:2 LOOP            POSTPONE LOOP      POSTPONE T[ ; IMMEDIATE
:2 +LOOP           POSTPONE +LOOP     POSTPONE T[ ; IMMEDIATE
:2 REPEAT          POSTPONE REPEAT    POSTPONE T[ ; IMMEDIATE
:2 UNTIL           POSTPONE UNTIL     POSTPONE T[ ; IMMEDIATE
:2 AGAIN           POSTPONE AGAIN     POSTPONE T[ ; IMMEDIATE

\ Last scripting block!
CREATE -scripting-
( :2 :F :R :I                                 )  \ AvdH B4oct14
WANT ALIAS
\ Alias of : , redefine an existing(!) word. Or crash.
: :2   PP @ NAME FOUND >R R@ HIDDEN PP !   :   R> HIDDEN ;
\ Use for dummy forward definitions.
': ALIAS :F
\ Resolve an earlier dummy definition for recursion.
: :R   PP @ NAME FOUND >R R@ HIDDEN PP !   :   R@ HIDDEN
  LATEST >DFA @ R> >DFA ! ;
\ Alias of :, define a word that inlines it code.
: :I   CREATE IMMEDIATE ]    LATEST HIDDEN !CSP
   DOES>   STATE @ IF BEGIN $@ DUP '(;) <> WHILE , REPEAT 2DROP
   ELSE >R THEN ;



( OLD: RESTORED POSTFIX ) \ AvdH A2jun12
\ WARNING: use these facilities only with high level words.

\ Compile the current execution behaviour of "name".
\ This behaviour remains the same if "name" is revectored.
: OLD:   NAME FOUND >DFA @ POSTPONE LITERAL   POSTPONE >R
    POSTPONE CO ; IMMEDIATE
\ Have the original behaviour of DEA restored.
: RESTORED   DUP >PHA SWAP >DFA ! ;
\ Do nothing for one call of ``NAME''
: NAME-NEW   'NAME RESTORED ;
\ Make the following defining word postfix for one execution.
\ The name must be a string constant on the stack
\ Use only while compiling, or you crash the system
: POSTFIX ( ?COMP ) 'NAME-NEW >DFA @ 'NAME >DFA ! ;
\ Example: : :P POSTFIX : !CSP ;
( Z$@ CTYPE C$.S ) \ AvdH A3mar20

\ For a CSTRING (pointer to zero ended chars) return a STRING.
: Z$@ DUP BEGIN COUNT 0= UNTIL 1- OVER - ;

\ Print a CSTRING.
: CTYPE Z$@ TYPE ;

\ Print a zero-pointer ended ARRAY of ``CSTRINGS'' . Abuse $@.
: C$.S BEGIN $@ DUP WHILE CTYPE CR REPEAT 2DROP ;






( ARGC ARGV ARG[] SHIFT-ARGS ENV       ) CF: ?LI \ AvdH B2sep21
WANT Z$@
\ Return the NUMBER of arguments passed by Linux
: ARGC   ARGS @   @ ;
\ Return the argument VECTOR passed by Linux
: ARGV   ARGS @   CELL+ ;
\ Return the environment POINTER passed by Linux
: ENV ARGS @   $@ 1+ CELLS +  ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[] CELLS ARGV + @ Z$@ ;
\ Return POINTER behind the end-0 of the environment.
: ENV0 ENV BEGIN $@ WHILE REPEAT ;
\ Shift the arguments, so as to remove argument 1.
: SHIFT-ARGS  ARGV >R
    R@ CELL+ CELL+   R@ CELL+  ENV0 R> -   MOVE
    -1 ARGS @ +! ;
( ARGC ARGV ARG[] SHIFT-ARGS ENV      ) CF: ?OSX \ AvdH B4apr19
WANT Z$@
\ Return the NUMBER of arguments passed by Linux
: ARGC   ARGS @   @ ;
\ Return the argument VECTOR passed by Linux
: ARGV   ARGS @   CELL+ ;
\ Return the environment POINTER passed by Linux
: ENV ARGS @   $@ 1+ CELLS +  ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[] CELLS ARGV + @ Z$@ ;
\ Return POINTER behind the end-0 of the environment.
: ENV0 ENV BEGIN $@ WHILE REPEAT ;
\ Shift the arguments, so as to remove argument 1.
: SHIFT-ARGS  ARGV >R
    R@ CELL+ CELL+   R@ CELL+  ENV0 R> -   MOVE
    -1 ARGS @ +! ;
( ARG$ ARGC ARG[] SHIFT-ARGS           ) CF: ?PC \ AvdH B2sep21
HEX    WANT DROP-WORD
\ Return argument STRING for (prot) DOS.
: ARG$   80 COUNT -LEADING -TRAILING ;

\ Return the NUMBER of arguments.
: ARGC  ARG$ 80 1 DO DUP 0= IF I LEAVE THEN DROP-WORD LOOP
   >R 2DROP R> ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[]   >R ARG$   R@ 1 < 0D ?ERROR
    R> 1 ?DO DROP-WORD LOOP   -LEADING BL $/ 2SWAP 2DROP ;

\ Shift the arguments, so as to remove argument 1. Keep cr!
: SHIFT-ARGS   ARG$ DROP-WORD   80 $!-BD   ^M ARG$ + C! ;
DECIMAL

( ARG$ ARGC ARG[] SHIFT-ARGS 2 ) CF: ?WI        \ AvdH B1jul10
HEX    WANT DROP-WORD
\ Return argument STRING for Windows.
: ARG$   ARGS @ -1 0 $/ 2SWAP 2DROP ;

\ Return the NUMBER of arguments.
: ARGC  ARG$ DUP 0 DO DUP 0= IF I LEAVE THEN DROP-WORD LOOP
   >R 2DROP R> ;
\ Find argument INDEX, counting from one. Return as a STRING.
: ARG[]   >R ARG$   R@ 1 < 0D ?ERROR
    R> 0 ?DO DROP-WORD LOOP   -LEADING BL $/ 2SWAP 2DROP ;

\ Shift the arguments, so as to remove argument 1. Keep cr!
: SHIFT-ARGS   ARG$ DROP-WORD   DROP ARGS ! ;
DECIMAL

( SRC>EXEC 1  ) CF: ?LI \ AvdH A3mar20

\ Given a source file NAME, return the binary file NAME.
: SRC>EXEC   4 -   2DUP + ".frt" CORA IF 2DROP "a.out" THEN ;












( SRC>EXEC 2 ) CF:                    \ AvdH B1aug16
HEX
\ Given a source file NAME, return the binary file NAME.
: SRC>EXEC   PAD $!   PAD $@ + 4 - >R
   R@ 4 + R@ 1 + DO I C@ 20 INVERT AND I C! LOOP \ Uppercase
   R> ".FRT" CORA IF "AOUT.EXE" ELSE
   -4 PAD +!   ".EXE" PAD $+!   PAD $@ THEN ;
DECIMAL







\
( LOAD-DLL: DLL-ADDRESS: K32 GET-ENV ) CF: ?WI \ AvdH B2aug9
( sc -- adr) : Z 0 , DROP ;
( n adr -- )
: make-constant
   BODY> >R    R@ >DFA  !   'BL >CFA @   R> >CFA ! ;
( sc -- u )
: LOAD-DLL: CREATE $, DROP DOES> DUP >R $@  LOAD-DLL
    DUP R> make-constant ;
( sc xt -- adr )
: DLL-ADDRESS:   CREATE , $, DROP     DOES> DUP >R   CELL+ $@
    R@ @ EXECUTE DLL-ADDRESS   DUP R> make-constant ;

"kernel32.dll" LOAD-DLL: K32
"GetEnvironmentVariableA" 'K32 DLL-ADDRESS: _gev
( sc -- sc )
: GET-ENV    _gev >R ZEN 4096 OVER DUP R> CALL ;
( GET-ENV ) CF: ?LI \ AvdH A3mar20

WANT Z$@   WANT COMPARE   WANT ENV

\ For SC and ENVSTRING leave SC / CONTENT and GOON flag.
: (MENV)   DUP 0= IF   DROP 2DROP 0. 0   ELSE
    Z$@ &= $/ 2SWAP >R >R 2OVER COMPARE
    IF   RDROP RDROP 1   ELSE   2DROP R> R> 0   THEN THEN ;
( Find a STRING in the environment, -its VALUE or NULL string)
: GET-ENV ENV BEGIN $@ SWAP >R (MENV) WHILE R> REPEAT RDROP ;






( SAVE-SYSTEM TURNKEY ) CF: ?WI HEX \ AvdH   B1oct1
: _BOOT-SECTION    BM 2000 -   BM  400 - 200 MOVE ;
: _KERNEL-SECTION  BM 1000 - BM  200 - 200 MOVE ;
: _FIXUP  SAVE >R  \ Fix up the kernel section at ADDRESS
   R@ 0C + @ 1000 -   R@ +   DUP 200 +   1FF INVERT AND
   OVER - SET-SRC    NAME 2DROP   R@ 10 + @  1000 -  R@ +
   BEGIN NAME WHILE  2 - 1000 + R@ - OVER !
    CELL+ REPEAT DROP RDROP RESTORE ;
: SAVE-USER-VARS U0 @   0 +ORIGIN   40 CELLS  MOVE ;
: INCREMENT    HERE   'DP >DFA @ +ORIGIN @ 'TASK DROP - ;
: SAVE-SYSTEM  ( Save the system in a file with NAME )
   >R >R _BOOT-SECTION INCREMENT BM  400 - 1B0 + +!
  _KERNEL-SECTION BM  200 - _FIXUP  SAVE-USER-VARS
   BM  400 - HERE  OVER - 200 + R> R> PUT-FILE ;
: TURNKEY  ( Save a system to do ACTION in a file witH NAME .)
  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ; DECIMAL
( SAVE-SYSTEM TURNKEY ) CF: ?LI ?32 HEX \ AvdH
\ The magic number marking the start of an ELF header
 CREATE MAGIC 7F C, &E C, &L C, &F C,
\ Return the START of the ``ELF'' header.
 : SM BM BEGIN DUP MAGIC 4 CORA WHILE 1- REPEAT ;
 SM 44 + CONSTANT D-SIZE  \ Where to patch for dictionary.
 SM 48 + CONSTANT G-SIZE  \ Where to patch for GROW.
\ Return the VALUE of ``HERE'' when this forth started.
 : HERE-AT-STARTUP  'DP >DFA @ +ORIGIN @ ;
 : SAVE-SYSTEM \ Save the system in a file with NAME .
  HERE BM - D-SIZE !  \ Fill in dict size (.text)
   U0 @   0 +ORIGIN   40 CELLS  MOVE \ Save user variables
\ Now write it. Consume NAME here.
   SM    HERE OVER -   2SWAP   PUT-FILE ;  DECIMAL
: TURNKEY  ( Save a system to do ACTION in a file witH NAME .)
  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ; DECIMAL
( SAVE-SYSTEM TURNKEY ) CF: ?LI ?64 HEX \ AvdH
\ The magic number marking the start of an ELF header
 CREATE MAGIC 7F C, &E C, &L C, &F C,
\ Return the START of the ``ELF'' header.
 : SM BM BEGIN DUP MAGIC 4 CORA WHILE 1- REPEAT ;
 SM 20 + @ SM + 20 + CONSTANT D-SIZE \ Dictionary size.
 D-SIZE 8 + CONSTANT G-SIZE
\ Return the VALUE of ``HERE'' when this forth started.
 : HERE-AT-STARTUP  'DP >DFA @ +ORIGIN @ ;
 : SAVE-SYSTEM ( ISO )   HERE BM - D-SIZE ! ( dict size)
   0 SM 28 + ! 1 SM 38 + ! ( Kill sections)
   U0 @   0 +ORIGIN   40 CELLS  MOVE ( Save user variables)
\ Now write it. Consume NAME here.
   SM    HERE OVER -   2SWAP   PUT-FILE ;  DECIMAL
\ Save a system to do ACTION in a file with NAME .
: TURNKEY  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ;
( SAVE-SYSTEM TURNKEY ) CF: ?OSX ?32 HEX        \ RS A8
CREATE MAGIC FEEDFACE , CREATE __DATA &_ C, &_ C, &D C, &A
C, &T C, &A C, CREATE __LINKEDIT &_ C, &_ C, &L C, &I C, &N
C, &K C, &E C, &D C, &I C, &T C, : HERE-AT-STARTUP'DP >DFA @
+ORIGIN @ ; : SM BM BEGIN DUP MAGIC 4 CORA WHILE 1- REPEAT ;
: FIND__DATA SM BEGIN DUP __DATA 6 CORA WHILE 1+ REPEAT ;
: FIND__LINKEDIT SM BEGIN DUP __LINKEDIT A CORA
WHILE 1+ REPEAT ; : KILL__LINKEDIT 20 10 DO 0 FIND__LINKEDIT
I + ! 4 +LOOP 48 38 DO 0 FIND__LINKEDIT I + ! 4 +LOOP ;
FIND__DATA 1C + CONSTANT __DATASIZE FIND__DATA 18 +
CONSTANT __FILEOFFSET
: SAVE-SYSTEM \ Save the system in a file
KILL__LINKEDIT HERE SM - __FILEOFFSET @  -  __DATASIZE !
U0 @   0 +ORIGIN   40 CELLS  MOVE \ Save user variables
SM HERE OVER - 2SWAP PUT-FILE ; DECIMAL
: TURNKEY  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ;
( SAVE-SYSTEM TURNKEY ) CF: ?PC HEX \ AvdH A7feb28
\ Write an MSDOS ``EXEHEADER'' structure over the PSP.
VARIABLE HEAD-DP  \ Fill in pointer
: W,   HEAD-DP @ >R   \ Add a 16 bit WORD to the header.
  DUP R@ C!   8 RSHIFT R@ 1+ C! R> 2 + HEAD-DP ! ;
\ Return the SIZE of the system, including header,
: SIZE HERE 10 + 1- 1FF OR 1+ ;  \ 10 byte stack. Whole pages.
\ Return OFFSET of the stack, using $100 bytes after ``SHELL''
: STACK-OFFSET   SHELL -2 CELL+ IF 200 + ELSE 120 + THEN ;
\ Fill in ``EXEHEADER'' struct, leave POINTER to where checksum
\ must be filled in.
: EXEHEADER    0  HEAD-DP !  5A4D W,   0 W,   SIZE  200 / W,
  0 W,   10 W,   10 W,   10 W,   0 W,   STACK-OFFSET 100 -  W,
  HEAD-DP @  0 W,   100 W,   -10 W,   SIZE W,   0 W, ;
 _PREV CONSTANT G-SIZE  \ Give -g a harmless address.
 BM CONSTANT SM          \ Start for -g.
( SAVE-SYSTEM TURNKEY ) CF: ?PC HEX \ AvdH A7feb28
\ Fill in checksum at the required POSITION in the header.
: CHECKSUM   HEAD-DP !
    0   SIZE 0 DO I @ + 2 +LOOP  NEGATE   W, ;
: SAVE-PSP   0 PAD 100 CMOVE ;
: SAVE-NAME PAD 100 + $! ;  \ Save NAME , it may point in psp!
: RESTORE-PSP  PAD 0 100 CMOVE ;
: RESTORE-NAME PAD 100 + $@ ;  \ Return NAME from ``SAVE-NAME''
: SAVE-USER-VARS U0 @   0 +ORIGIN   40 CELLS  MOVE ;
\ Save the system in a file with NAME .
: SAVE-SYSTEM   10 ALLOT ( startup stack) SAVE-PSP SAVE-NAME
  SAVE-USER-VARS   EXEHEADER CHECKSUM
  0 SIZE RESTORE-NAME PUT-FILE   RESTORE-PSP ;  DECIMAL
\ Actually this is the same than in Linux.
\ Save a system to do SOMETHING in a file with NAME .
: TURNKEY  ROT >DFA @  'ABORT >DFA !  SAVE-SYSTEM BYE ;
( CVA  aux_for_threads ) \ AvdH A2jul5
WANT TASK-SIZE   \ Fails unless kernel prepared for threads

HEX   40 CELLS CONSTANT US

\ Clone the stack frame to ``TASK-SIZE'' lower in memory.
\ This area extends up to ``R0 @ US + '' .
\ The Forth is left running in the new area.
: CVA  DSP@   DUP TASK-SIZE -   U0 @ DSP@ - US +   MOVE
       DSP@ TASK-SIZE - DSP!    RSP@ TASK-SIZE - RSP!
       4 -1 DO   TASK-SIZE NEGATE U0 I CELLS +   +!   LOOP ;




\
( THREAD-PET KILL-PET PAUSE-PET ) CF: ?LI \ A2nov16
WANT CVA   WANT -syscalls-      HEX
\ Exit a thread. Indeed this is exit().
: EXIT-PET 0 _ _ __NR_exit XOS ;
\ Do a preemptive pause. ( abuse MS )
: PAUSE-PET 1 MS ;

\ Create a thread with dictionary SPACE. Execute XT in thread.
: THREAD-PET CREATE S0 @ 2 CELLS - , R0 @ , 0 , CVA ALLOT
    DOES> >R  ( xt) R@ @ CELL+ !   R@ CELL+ @  ( R0) R@ @ !
    112 R@ @ _ __NR_clone XOS DUP 0< IF THROW THEN
    DUP IF ( Mother) R> 2 CELLS + !
    ELSE ( Child) DROP RSP! EXECUTE EXIT-PET THEN ;
\ Kill a THREAD-PET , preemptively. Throw errors.
: KILL-PET >BODY 2 CELLS + @ 9 _ __NR_kill XOS ?ERRUR ;
DECIMAL
( TASK-TABLE NEXT-TASK PAUSE-COT) HEX \ AvdH A2jul5
WANT SET
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
WANT TASK-TABLE   WANT CVA

\ Exit: remove current task, then chain to first one.
: EXIT-COT  THIS TASK-TABLE SET-REMOVE
    SET-FIRST-TASK   THIS @ RSP! R> DSP! ;

\ Create a thread with dictionary SPACE. Execute XT in thread.
: THREAD-COT   CREATE R0 @ 3 CELLS - , S0 @ , CVA ALLOT
    DOES> $@ >R  \ R@ is rsp of new task
    @                   R@ !
    >DFA @              R@ CELL+ !
    'EXIT-COT >DFA @    R@ CELL+ CELL+ !
    R> TASK-TABLE SET+! ;

\
( {{ }} )                               \ AHCH   B5mar03
WANT NESTED-COMPILE
\ New context for definitions, maybe in the middle of a word.
: {{ ( NESTED-COMPILE ) POSTPONE SKIP (FORWARD R>
        CSP @ >R DPL @ >R UNLINK-LATEST >R STATE @ >R
    >R POSTPONE [ ; IMMEDIATE

: }} FORWARD)  R>
        R> STATE ! R> LINK-LATEST R> DPL ! R> CSP !
    >R ;






( NESTED-COMPILE ) \ AvdH A2oct28

\ Isolate the latest word from the dictionary. Leave its DEA.
: UNLINK-LATEST LATEST CURRENT @ >LFA DUP @ >LFA @ SWAP ! ;

\ Link DEA into the dictionary, as the latest.
: LINK-LATEST LATEST OVER >LFA ! CURRENT @ >LFA ! ;

\ Save all compilation information on the return stack. It is
\ restored upon exit of the calling word.
: NESTED-COMPILE R>   CSP @ >R DPL @ >R UNLINK-LATEST >R
  STATE @ >R   >R CO    R> STATE ! R> LINK-LATEST R> DPL !
  R> CSP !   ;


\
\ LATEST-WORD (WORD-BACK)             \ A2oct28 AvdH
\ Trim a possible leading &' from a word.
: TRIM' OVER C@ &' = IF 1- SWAP 1+ SWAP THEN ;
\ Fpr POINTER into/past word, return START of word.
: (WORD-BACK) BEGIN 1- DUP C@ ?BLANK 0= UNTIL 1+
    BEGIN 1- DUP C@ ?BLANK UNTIL 1+ ;
\ Return SC the latest word in the input.
: LATEST-WORD PP @ (WORD-BACK) SRC @ MAX PP ! NAME ( TRIM') ;
\ The compiled program can't run.
VARIABLE FAILED    0 FAILED !
\ The compiled program can run, after reload.
VARIABLE SECOND-PASS 0 SECOND-PASS !
: .SUCCESS "WANT " TYPE TYPE CR ;
: .FAILURE "Find out about " TYPE TYPE CR -1 FAILED ! ;


( REMEDY FIX-DEA FIX-NMB )             \ A2oct28 AvdH
WANT SWAP-DP    WANT LATEST-WORD   WANT NESTED-COMPILE
\ Try to add the current, missing word to the dictionary: DEA.
: REMEDY NESTED-COMPILE   POSTPONE [ SWAP-DP LATEST-WORD 2DUP
 WANTED SWAP-DP   2DUP PRESENT IF 2DUP .SUCCESS FOUND ELSE
.FAILURE 'NOOP THEN ;
\ Make words that look like malformed numbers (like 2R> )
\ compile without error, but with run time errors.
\ Loading the same code another time will give correct code.
: FIX-NMB REMEDY 0 DSP@ 3 CELLS + ! DROP -1 PP +!
-1 SECOND-PASS !   -1 POSTPONE LITERAL   13 POSTPONE LITERAL
 POSTPONE ?ERROR   " Recompile!" TYPE CR ;
\ Fix up errors caused by unknown words, if the library can
\ resolve them. At least go on compiling.
: FIX-DEA REMEDY 0 DSP@ 3 CELLS + ! DSP@ 3 CELLS + ! ;

( ?ERROR-FIXING AUTOLOAD NO-AUTOLOAD ) \ A2oct28 AvdH
WANT OLD: WANT FIX-NMB WANT FIX-DEA
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
WANT ASSEMBLERi86 HEX
CODE PC PUSH|CS, NEXT, END-CODE
CODE PD PUSH|DS, NEXT, END-CODE
CODE PE PUSH|ES, NEXT, END-CODE
CODE PS PUSH|SS, NEXT, END-CODE

DECIMAL








( GET-SEL PUT-SEL NEW-SEL handle_DPMI_selectors) CF: ?DP
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

EXIT

Tools and utilities











( INSTALL-TRAPS ) CF: ?WIMS \ AvdH A9sep15
\ Nobody knows how to do this on Bill's systems.
: INSTALL-TRAPS  ;
: INSTALL-NO-TRAPS  ;












( SET-TRAPS  INSTALL-TRAPS ) CF: ?LI \ AvdH A3jun12
WANT -syscalls-
\ Make sure any traps restart Forth at ADDRESS .
: SET-TRAPS  32 0 DO I OVER _ __NR_signal XOS DROP LOOP DROP ;

\ Still fig tradition: warm and cold starts below origin
: SET-TRAPS-WARM   -2 CELLS +ORIGIN   SET-TRAPS ;
: INSTALL-NO-TRAPS   0 SET-TRAPS ;

: NEW-WARM    SET-TRAPS-WARM   OLD: WARM ;

\ Install traps such that they are reinstated if invoked.
: INSTALL-TRAPS SET-TRAPS-WARM
   'NEW-WARM >DFA @   'WARM >DFA ! ;


( DO-DEBUG NO-DEBUG ) \ AvdH A6sep19
WANT OLD:    WANT INSTALL-TRAPS
\ An alternative ``OK'' message with a stack dump.
: NEW-OK   .S ."  OK " ;
\ Print index line of SCREEN .
: .INDEX-LINE  CR DUP 4 .R 0 SWAP (LINE) -TRAILING TYPE ;
\ An alternative ``THRU'' that displays first and last index.
: NEW-THRU  OVER .INDEX-LINE " -- " TYPE  DUP .INDEX-LINE CR
  OLD: THRU ;
\ Install and de-install the alternative ``OK'' and traps
: DO-DEBUG   INSTALL-TRAPS   'NEW-OK >DFA @   'OK >DFA !
   'NEW-THRU >DFA @   'THRU >DFA ! ;
: NO-DEBUG   INSTALL-NO-TRAPS  'OK RESTORED
 'WARM RESTORED 'THRU RESTORED ;
: break   SAVE  BEGIN '(ACCEPT) CATCH DUP -32 <> WHILE ?ERRUR
    SET-SRC INTERPRET REPEAT   DROP RESTORE ; \ End by ^D
( DO-SECURITY NO-SECURITY NO-SECURITY: ) \ AH B2jun15
WANT RESTORED
\
\ Want a high level definition, to replace ?PAIRS
: ?NO-PAIRS  2DROP ;

\ Install and de-install the security
: NO-SECURITY   '?NO-PAIRS >DFA @   '?PAIRS >DFA ! ;

: DO-SECURITY   '?PAIRS RESTORED ;

\ Install no-security with automatic recovery.
: NO-SECURITY:    R> '?PAIRS >DFA @ >R  >R NO-SECURITY CO
    R> '?PAIRS >DFA ! ;


( CASE-INSENSITIVE CASE-SENSITIVE CORA-IGNORE ) \ AvdH A7oct11
WANT RESTORED HEX
\ Characters ONE and TWO are equal, ignoring case.
: C=-IGNORE DUP >R   XOR DUP 0= IF 0= ELSE
     20 <> IF 0 ELSE
     R@ 20 OR &a &z 1+ WITHIN THEN THEN  RDROP ;
\ ( ad1 ad2 cnt -- f) f means equal (0) or not. No lexicography
: CORA-IGNORE 0 ?DO   OVER I + C@   OVER I + C@   C=-IGNORE 0=
    IF   2DROP -1 UNLOOP EXIT   THEN LOOP   2DROP 0 ;
\ Caseinsensitive version of ~MATCH
: ~MATCH-IGNORE   >R   2DUP   R@ >NFA @ $@   ROT MIN
    CORA-IGNORE   R> SWAP ;
: CASE-SENSITIVE?  '~MATCH DUP >DFA @ SWAP >PHA = ;
\ Install matchers
: CASE-INSENSITIVE   '~MATCH-IGNORE >DFA @ '~MATCH >DFA ! ;
: CASE-SENSITIVE '~MATCH RESTORED ; DECIMAL
( DUMP ) WANT B.  HEX: \ AvdH A1oct02
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
;
( OOPS EDIT: ) \ AvdH A1oct12

"EDIT" PRESENT 0= ?LEAVE-BLOCK
">SFA" PRESENT 0= ?LEAVE-BLOCK

\ edit the following word
: EDIT: NAME FOUND >SFA @  1 MAX 255 MIN EDIT ;


\ edit the latest word, the one with the bug
: OOPS LATEST >SFA @ EDIT ;





( SUPER-QUAD CONDENSED ) CF: ?PC
WANT VIDEO-MODE
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


( FOR-BLOCKS SHOW-BLOCK .BL Testing_of_block ) \ AvdH A1oct09
WANT H.
: FOR-BLOCKS >R _PREV @
    BEGIN DUP R@ EXECUTE +BUF WHILE REPEAT R> DROP DROP ;
: SHOW-BLOCK

    DUP CR H.
    DUP @ IF
        ." #"  DUP ?
        CELL+ DUP @ IF ."     LOCKED" ELSE ." NOT LOCKED" THEN
        CELL+ &| EMIT 50 TYPE &| EMIT
    ELSE
        ." FREE " DROP
    THEN ;
: .BL 'SHOW-BLOCK FOR-BLOCKS ;

( DB-INSTALL DB-UNINSTALL Show_block_properties) \ AvdH A1oc08
WANT ALIAS
: .CON &| EMIT   48 TYPE   &| EMIT ;
: .HEAD $@ " BLOCK NR IS " TYPE   .   $@
"LOCK IS " TYPE   . ;
: .SPECIAL
DUP _PREV @ = IF &> ELSE

BL THEN EMIT ;
: DB 0 _LIMIT FIRST DO CR DUP . 1+
I .SPECIAL .HEAD .CON B/BUF CELL+ CELL+ +LOOP
DROP KEY DROP .S ;
'BLOCK ALIAS BLOCK2
: NEW-BLOCK BLOCK2 DB ;
: DB-INSTALL 'NEW-BLOCK 'BLOCK 3 CELLS MOVE ;
: DB-UNINSTALL 'BLOCK2 'BLOCK 3 CELLS MOVE ;
( KRAAK CRACK CRACK-CHAIN ) \ AvdH A2mar21
WANT SEE














( SEE -see0-table-  )                        \ AvdH B5Feb24
WANT BAG    WANT DO-BAG
60 BAG SELTAB   SELTAB !BAG
\ Put SEARCHED XT in select table)
: SEL! SWAP SELTAB BAG+! SELTAB BAG+! ;
( N--M,TRUE/ N,FALSE ) \ Look up N in table.
: SEL@    DUP SELTAB BAG-WHERE DUP IF CELL+ @ NIP -1 THEN ;
\ Decompile a word from its DEA
 : CRACKED   DUP @ SEL@ IF EXECUTE ELSE
   DROP DUP >CFA @ OVER >PHA = IF CR ." Code definition : "
     ELSE CR ." Can't handle : " THEN ID. CR  THEN ;

\ When finding DEA decompile by "name"
: by:   ' SEL! ;
\ When finding words with same cfa of DEA decompile by "name"
: example-by:   >CFA @    by: ;
( SEE -see1-io-  )                 \ AvdH B5Feb20
WANT H.         WANT BAG    WANT DO-BAG
: H.. H. SPACE ;       \ Hex with comma's
: SH.. HEX: . ;        \ Signed but hex.
\ From DECOMPILER-POINTER print, leave incremented POINTER
: ID.+   $@ ID. ;
: H.+ CELL+ DUP @ H.. CELL+ ;
: SH.+ CELL+ DUP @ SH.. CELL+ ;
: '.+ CELL+ DUP @ &' EMIT ID. CELL+ ;







( SEE -see2-simple-decompilers- )              \ AvdH B5Feb24
\ all -words: ( dip --dip' ) decompile pointer
: -do CR ." DO " CELL+ CELL+ ;     '(DO) by: -do
: -qdo CR ." ?DO " CELL+ CELL+ ;   '(?DO) by: -qdo
: -lo CR ." LOOP " CELL+ CELL+ ;   '(LOOP) by: -lo
: -pl CR ." +LOOP " CELL+ CELL+ ;  '(+LOOP) by: -pl

: -pc CR ." ;CODE plus code (suppressed)"
   ( DIRTY TRICK : make decompile pointer point to exit!)
   DROP 'TASK >DFA @ ;          ' (;CODE) by: -pc

\ Decompile inline string constant
: -sk CELL+ CR ." [ " &" EMIT DUP $@ TYPE &" EMIT
   ."  ] DLITERAL " $@ + 4 CELLS + ;    ' SKIP by: -sk


( SEE -see3-example-decompilers- )             \ AvdH B5Feb20
: -co DUP >DFA @ CR H.. ." CONSTANT " ID. CR ;
   ' BL example-by: -co
: -va DUP >DFA @ @ CR &( EMIT SPACE H.. ." ) VARIABLE "
   ID. CR ;                     ' _PREV example-by: -va
: -us DUP >DFA C@ CR B. ."  USER " ID. CR ;
   ' BASE example-by: -us









( SEE   -see4-auxiliary- )
( For the DEA : it IS immediate / it IS a denotation )
: ?IM >FFA @ 4 AND ;     : ?DN >FFA @ 8 AND ;

\ For DEA1 get DEA2 the word defined later then DEA1
: NEXT-DEA CURRENT @ BEGIN ( CR DUP ID.) 2DUP >LFA @ <>
    WHILE >LFA @ DUP 0= IF 1000 THROW THEN REPEAT SWAP DROP ;

( For the NUMBER : it IS a proper `dea' )
( The <BM is not only optimisation, else `LIT 0' goes wrong.)
: DEA? DUP BM < IF DROP 0 ELSE
    DUP 'NEXT-DEA CATCH IF 2DROP 0 ELSE >LFA @ = THEN THEN ;




( SEE   -see5-does-objecten- )
\ For ADR: it IS a dea (heuristically).
: HEAD? DUP >DFA @ SWAP  >PHA = ;
\ For ADDRESS: find DEA to which this address belongs.
: FIND-HEAD ALIGNED ( Important for CREATE )
     BEGIN DUP HEAD? 0= WHILE 1 CELLS - REPEAT ;
\ For DEA: hl POINTER in ``DOES>'' code.
: TO-DOES   >DFA @ @ ;
: -dd   DUP TO-DOES FIND-HEAD ." ( data ) " ID. ID. CR ;
    ' FORTH  example-by: -dd  \ Use namespace as example.






( SEE   -see6-colon&literals- )
( dip -- dip )
: ITEM DUP @ SEL@ IF EXECUTE ( special) ALIGNED ELSE
    DUP ?IM IF ." POSTPONE " THEN ID. CELL+ THEN ;
: CRACK-COLON CR BEGIN DUP @ LIT (;) <> WHILE
    ITEM REPEAT DROP ;
: -hi CR ." : " DUP DUP ID. >DFA @ CRACK-COLON CR ." ;"  DUP
   ?IM IF ."  IMMEDIATE " THEN ?DN IF ."  PREFIX" THEN CR ;
    ' TASK example-by: -hi

: -lit DUP CELL+ @ DEA? IF '.+ ELSE H.+ THEN ;
    ' LIT by: -lit




( SEE   -see7-branch-decompilers- )
: TARGET DUP 1 CELLS - @ + ; ( dip -- jump target )
: .DEA? DUP DEA? IF ID. ELSE DROP ." ? " THEN ; ( dea --. )
: PRINT-TARGET DUP ( dip -- dip )   ." ( between " TARGET DUP
  1 CELLS - @ .DEA? @ .DEA? ." ) " ;
: -0br CR ." 0BRANCH [ " SH.+ ." , ] " PRINT-TARGET ;
    ' 0BRANCH by: -0br
: -br  CR ." BRANCH  [ " SH.+ ." , ] " PRINT-TARGET ;
    ' BRANCH by: -br







( SEE  -see8-                                   \ AvdH B2oct02
\ Use CRACK "ITEM" to decompile the word ITEM)
: CRACK ' CRACKED ;
: KRAAK CRACK ;
: SEE   CRACK ;
\ Crack from "name" then newer words.
: CRACK-FROM   '   BEGIN DUP CRACKED 'NEXT-DEA CATCH UNTIL
   DROP ;








( ASSEMBLER CODE END-CODE C; )  \ AvdH A0oct21
NAMESPACE ASSEMBLER
\ ISO standard words.
: CODE NAME (CREATE) ASSEMBLER !CSP  ;
: ;CODE
?CSP   POSTPONE (;CODE)   [COMPILE] [   ASSEMBLER
; IMMEDIATE
: END-CODE ?CSP PREVIOUS ;
\ Non standard. A traditional alias for END-CODE .
: C; END-CODE ;





\
( ASSEMBLERi86-HIGH )  CF:          \ B1oct16 AvdH
WANT ASSEMBLER   WANT SWAP-DP   WANT ALIAS
\ Disallow case-insensitive assembler
'~MATCH DUP >DFA @ SWAP >PHA <> 13 ?ERROR
"ASSEMBLERi86" PRESENT ?LEAVE-BLOCK
SWAP-DP
: ASSEMBLERi86 ;
ASSEMBLER DEFINITIONS
 HEX
 WANT  ASSEMBLER-GENERIC
 WANT  ASSEMBLER-CODES-i86
 WANT  ASSEMBLER-CODES-PENTIUM
 WANT  ASSEMBLER-MACROS-i86
 DECIMAL
SWAP-DP
PREVIOUS DEFINITIONS
( ASSEMBLERi86 )  CF:               \ B4feb28 AvdH
WANT ASSEMBLER   WANT ALIAS


"ASSEMBLERi86" PRESENT ?LEAVE-BLOCK

: ASSEMBLERi86 ;
ASSEMBLER DEFINITIONS
 HEX
 WANT  ASSEMBLER-GENERIC
 WANT  ASSEMBLER-CODES-i86
 WANT  ASSEMBLER-CODES-PENTIUM
 WANT  ASSEMBLER-MACROS-i86
 DECIMAL

PREVIOUS DEFINITIONS
( ASSEMBLER-GENERIC SPLIT 1PI FIR 1FAMILY, )  \ A7oct19 AvdH
: SPLIT 0 100 UM/MOD SWAP   ; \ Split X : ls BYTE and REMAINDER
\ Post INSTRUCTION of LENGTH.  Big endian specific!
: POST  SWAP , 1 CELLS - ALLOT ;
\ Fixup with ms byte of FIX below ADDR, leave next FIX ADDR
: FIX| 1- >R   SPLIT R@ SWAP TOGGLE   R> ;
: 1PI CREATE , DOES>  @ 1 POST  ;   \ 1 byte post-it opcode
: 2PI CREATE , DOES>  @ 2 POST  ;   \ 2 byte post-it opcode
: 3PI CREATE , DOES>  @ 3 POST  ;   \ 3 byte post-it opcode
\ Fixup from behind starting with ls byte.
: FIR CREATE , DOES> @ HERE BEGIN FIX| OVER 0= UNTIL 2DROP ;
\ Create a family adding INC to OPCODE with COUNT members
: 1FAMILY, 0 DO DUP 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP 3PI OVER + LOOP DROP DROP ;
: FAMILY|R 0 DO DUP FIR OVER + LOOP DROP DROP ;
( ASSEMBLER-CODES-i86 AX| ) CF: ?16 \ A2oct21 AvdH
 40 00 4 FAMILY|R ZO| BO| XO| R|
 01 00 8 FAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
 01 00 8 FAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
 08 00 8 FAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
 08 00 8 FAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
 08 00 4 FAMILY|R ES| CS| SS| DS|
0100 0000 2 FAMILY|R B| X|   0200 0000 2 FAMILY|R F| T|

\ No strict need to prime thingies when there are no unprimed.
06 FIR MEM| ( OVERRULES ZO| [BP] )
06 FIR MEM|% ( OVERRULES ZO| [BP] )
( 07) 1 0 8 FAMILY|R [BX+SI] [BX+DI] [BP+SI] [BP+DI]
    [SI] [DI] [BP] [BX]
( 07) 1 0 8 FAMILY|R% [BX+SI]% [BX+DI]% [BP+SI]% [BP+DI]%
    [SI]% [DI]% [BP]% [BX]%
( ASSEMBLER-CODES-i86 [AX] AX| ) CF: ?32+ \ A4sep27 AvdH

 40 00 4 FAMILY|R ZO| BO| XO| R|
 05 FIR MEM| ( instead of ZO| BP| )
 01 00 8 FAMILY|R [AX] [CX] [DX] [BX] ~SIB| [BP] [SI] [DI]
 01 00 8 FAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
 01 00 8 FAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
 08 00 8 FAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
 08 00 8 FAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
 08 00 6 FAMILY|R ES| CS| SS| DS| FS| GS|
0100 0000 2 FAMILY|R B| X|   0200 0000 2 FAMILY|R F| T|

( Percented thingies are for 16 bits protected code, e.g. AS: )
( C7) 6 FIR MEM|%
( 07) 1 0 8 FAMILY|R [BX+SI]% [BX+DI]% [BP+SI]% [BP+DI]%
[SI]% [DI]% [BP]% [BX]%
( ASSEMBLER-CODES-i86  SIB,, [AX )   CF: ?32+ \ A4sep27 AvdH
\ Fixups from this pages must come after all others
\ and start with a [xx .
0 1PI SIB,,      \ Required after SIB|

08 00 8 FAMILY|R AX] CX] DX] BX] 0] BP] SI] DI]
40 0 4 FAMILY|R +1* +2* +4* +8*

: [AX   ~SIB| SIB,, AX| ;       : [SP   ~SIB| SIB,, SP| ;
: [CX   ~SIB| SIB,, CX| ;       : [BP   ~SIB| SIB,, BP| ;
: [DX   ~SIB| SIB,, DX| ;       : [SI   ~SIB| SIB,, SI| ;
: [BX   ~SIB| SIB,, BX| ;       : [DI   ~SIB| SIB,, DI| ;
'[BP ALIAS [MEM



( ASSEMBLER-CODES-i86 IL, IW, ) \ A4sep27 AvdH
: lsbyte, SPLIT C, ;
: (W,) lsbyte, lsbyte, DROP ;
: (L,) lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

( O=obligatory R=Relative I=Immediate )
' (W,) ALIAS OW,        ' (L,) ALIAS IL,
' (L,) ALIAS (RL,)      ' (W,) ALIAS IW,
' (W,) ALIAS (RW,)      ' C,   ALIAS IB,
' C,   ALIAS (RB,)      ' (L,) ALIAS L,
' (W,) ALIAS SG,        ' (W,) ALIAS W,
' C,   ALIAS P,         ' C,   ALIAS B,
' C,   ALIAS IS,



( ASSEMBLER-CODES-i86 --opcodes1 ) \ A4sep27 AvdH
08 06 4 1FAMILY, PUSH|ES, PUSH|CS, PUSH|SS, PUSH|DS,
08 07 4 1FAMILY, POP|ES, -- POP|SS, POP|DS,
08 26 4 1FAMILY, ES:, CS:, SS:, DS:,
08 27 4 1FAMILY, DAA, DAS, AAA, AAS,
01 00 2 FAMILY|R B'| X'|
08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A,
                 XORI|A, CMPI|A,
02 A0 2 1FAMILY, MOV|TA, MOV|FA,

70 1PI J,  ( As in J, L| Y| <CALC> S, )
    01 00 2 FAMILY|R Y| N|
    02 00 8 FAMILY|R O| C| Z| CZ| S| P| L| LE|

08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
90 1PI XCHG|AX,
( ASSEMBLER-CODES-i86 --opcodes2 ) \ A4sep27 AvdH
08 00 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
02 84 2 2FAMILY, TEST, XCHG,
01 98 8 1FAMILY, CBW, CWD, IR2, WAIT, PUSHF, POPF, SAHF, LAHF,
02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
08 B0 2 1FAMILY, MOVI|B, MOVI|X,
08 C3 2 1FAMILY, RET,  RETFAR,  08 C2 2 1FAMILY, RET+, RETFAR+,
01 C4 2 2FAMILY, LES, LDS,  00C6 2PI MOVI,   0CD 1PI INT,
01 CC 4 1FAMILY, INT3, -- INTO, IRET,
01 D4 4 1FAMILY, AAM, AAD, -- XLAT,
01 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
02 E4 2 1FAMILY, IN|P, OUT|P,  2 EC 2 1FAMILY, IN|D, OUT|D,
01 E8 2 1FAMILY, CALL, JMP,

0088 2PI MOV,           008C 2PI MOV|SG,        008D 2PI LEA,
EA 1PI JMPFAR,  EB 1PI JMPS,    9A 1PI CALLFAR, A8 1PI TESTI|A,
( ASSEMBLER-CODES-i86 --opcodes3 ) \ A2oct21 AvdH
01 F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
01 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD, ( 38FE)
800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI,
    CMPI,
0800 83 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
800 10F6 6 2FAMILY, NOT, NEG, MUL|AD, IMUL|AD, DIV|AD, IDIV|AD,
0800 00FE 2 2FAMILY, INC, DEC,
0800 10FF 4 2FAMILY, CALLO, CALLFARO, JMPO, JMPFARO,

0200 0000 2 FAMILY|R 1| V|
0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,
0800 C0 8 2FAMILY, ROLI, RORI, RCLI, RCRI, SHLI, SHRI, -- SARI,

00F6 2PI TESTI,         008F 2PI POP,           30FF 2PI PUSH,
00,AF0F 3PI IMUL,
( ASSEMBLER-CODES-i86 --opcodes4 )  CF: ?32+    \ A4sep27 AvdH
01 60 2 1FAMILY, PUSH|ALL, POP|ALL,
01 62 2 2FAMILY, BOUND, ARPL,
01 64 4 1FAMILY, FS:, GS:, OS:, AS:,
02 68 2 1FAMILY, PUSHI|X, PUSHI|B,
0002 0069 2 2FAMILY, IMULI, IMULSI,
02 6C 2 1FAMILY, INS, OUTS,
08 00 5 FAMILY|R CR0| -- CR2| CR3| CR4|
0008 0100 8 FAMILY|R DR0| DR1| DR2| DR3| DR4| DR5| DR6| DR7|

00900F 3PI SET,
100 0 2 FAMILY|R Y'| N'|
200 0 8 FAMILY|R O'| C'| Z'| CZ'| S'| P'| L'| LE'|

C8 1PI ENTER,           C9 1PI LEAVE,           060F 2PI CLTS,
C0200F 3PI MOV|CD,      800F 2PI J|X,
( ASSEMBLER-CODES-i86 --opcodes4 )  CF: ?32+   \ A4sep27 AvdH
00,0100 00,020F 2 3FAMILY, LAR, LSL, ( 3)
0100 A00F 3 2FAMILY, PUSH|FS, POP|FS, CPUID,
00,0800 00,A30F 4 3FAMILY, BT, BTS, BTR, BTC,
00,0100 00,BC0F 2 3FAMILY, BSF, BSR,
00,0800 00,A40F 2 3FAMILY, SHLDI, SHRDI,
00,0800 00,A50F 2 3FAMILY, SHLD|C, SHRD|C,
00,0100 00,B20F 4 3FAMILY, LSS, -- LFS, LGS,
00,0800 00,B60F 2 3FAMILY, MOVZX|B, MOVSX|B,
00,0800 00,B70F 2 3FAMILY, MOVZX|W, MOVSX|W,
0100 A80F 2 2FAMILY, PUSH|GS, POP|GS,
08,0000 00,000F 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,
08,0000 20,BA0F 4 3FAMILY, BTI, BTSI, BTRI, BTCI,
08,0000 00,010F 7
    3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, -- LMSW,

( ASSEMBLER-CODES-PENTIUM --opcodes ) CF: ?32+ \ A7oct19 AvdH


\ 0F prefix
0100 080F 2 2FAMILY, INVD, WBINVD,
AA0F 2PI RSM,
0B0F 2PI Illegal-1,
B90F 2PI Illegal-2,
C80F 2PI BSWAP,
0100 300F 3 2FAMILY, WRMSR, RDTSC, RDMSR,
1000 B00F 2 3FAMILY, CMPXCHG, XADD,

38010F 3PI INVLPG,
08C70F 3PI CMPXCHG8B,


( ASSEMBLER-CODES-PENTIUM --fixups_fp ) CF: ?32+ \ B4feb28 AvdH
   01 00 8 FAMILY|R ST0| ST1| ST2| ST3| ST4| ST5| ST6| ST7|
0400 00 2 FAMILY|R s| d|     \ Single/Double 16/32
0400 00 2 FAMILY|R |32 |16   \ memory int
0008 00 2 FAMILY|R n| a|     \ Normal reverse
0400 00C0 2 FAMILY|R u| m|










( ASSEMBLER-CODES-PENTIUM --fp_1 ) CF: ?32+   \ A7oct19 AvdH
0800 00D8 7 2FAMILY, FADD, FMUL, FCOM, FCOMP, FSUB, -- FDIV,

0800 10D9 6 2FAMILY, FST, FSTP, FLDENV, FLDCW, FSTENV, FSTCW,
0100 E0D9 6 2FAMILY, FCHS, FABS, -- -- FTST, FXAM,
0100 E8D9 4 2FAMILY, FLD1, FLDL2T, FLDL2E, FLDPI,
0100 ECD9 3 2FAMILY, FLDLG2, FLDLN2, FLDZ,
0100 F0D9 4 2FAMILY, F2XM1, FYL2X, FPTAN, FPATAN,
0100 F4D9 4 2FAMILY, FXTRACT, FPREM1, FDECSTP, FINCSTP,
0100 F8D9 4 2FAMILY, FPREM, FYL2XP1, FSQRT, FSINCOS,
0100 FCD9 4 2FAMILY, FRNDINT, FSCALE, FSIN, FCOS,
00D9 2PI FLD,           C8D9 2PI FXCH,          D0D9 2PI FNOP,

0800 00DA 4 2FAMILY, FIADD, FIMUL, FICOM, FICOMP,
1000 20DA 2 2FAMILY, FISUB, FIDIV,
E9DA 2PI FUCOMPP,
( ASSEMBLER-CODES-PENTIUM --fp_2) CF: ?32+ \ A7oc17 AvdH
0800 00DB 4 2FAMILY, FILD, -- FIST, FISTP,
1000 28DB 2 2FAMILY, FLD|e, FSTP|e,
E2DB 2PI FCLEX,         E3DB 2PI FINIT,

0800 20DD 4 2FAMILY, FRSTOR, -- FSAVE, FSTSW,
0800 C0DD 6 2FAMILY, FFREE, -- FST|u, FSTP|u, FUCOM, FUCOMP,

0800 C0DE 2 2FAMILY, FADDP, FMULP,
1000 E0DE 2 2FAMILY, FSUBP, FDIVP,
D9DE 2PI FCOMPP,

0800 20DF 4 2FAMILY, FBLD, FILD|64, FBSTP, FISTP|64,
E0DF 2PI FSTSW|AX,


( ASSEMBLER-MACROS-i86 NEXT, TEST-NEXT ) \ A7oct19 AvdH
: NEXT,     \ Works even in 16 bits.
     LODS, X'|
     MOV, X| F| AX'| R| BX|
     JMPO, ZO| [BX]    \ JMPO, [AX] not available in 16 bits.
;
: PUSH PUSH|X, AX| NEXT, ;
: PUSH2 PUSH|X, DX| PUSH ;

( Tests applicable always )
  CODE TEST-NEXT NEXT,  END-CODE
  " Testing next " TYPE
  TEST-NEXT
  " next Tested " TYPE

\
( ASSEMBLER-MACROS-i86 NOP, CP, COPY-SEG ) \ A7oct19 AvdH
\ Using plain comma's here to work for 16/32 bits.

\ Copy data from ADDRESS1 to ADDRESS2
: CP, MOV|TA, B| SWAP DUP , 1 + MOV|FA, B| SWAP DUP , 1 + ;
\                      L, / W,                  L, / W,

\ Do nothing.
: NOP, XCHG|AX, AX| ;

: COPY-SEG
    MOV, X| T| AX'| MEM| ( DAT -- )  L,
    MOV|SG, T| DS| R| AX|
    MOV|SG, T| ES| R| AX|
    MOV|SG, T| SS| R| AX|  ;

( ASSEMBLER-MACROS-i86 TO-PROT, TO-REAL, ) CF: ?32+ \ B4may28

: GET-CR0   MOV|CD, F| CR0| AX| ;
: PUT-CR0   MOV|CD, T| CR0| AX| ;
: TO-PROT,  GET-CR0  INC, X|  R| AX| PUT-CR0 ;
: TO-REAL,  GET-CR0  DEC, X|  R| AX| PUT-CR0 ;









\
( --special_macros_1 JMP-REAL, JMP-PROT, REAL, PROT, ) CF: ?32+
HEX   \ Not part of assembler!
WANT TO-PROT, TO-REAL,
\ These macro's are useful for protected mode under MSDOS
\ or for stand alone booting systems.
 7C0 CONSTANT SWITCH_DS 17C0 CONSTANT GDT_DS
 10 CONSTANT GDT_CS
: JMP-PROT, JMPFAR, HERE CELL+  , GDT_CS SG, ;
: JMP-REAL,
    JMPFAR, HERE CELL+ SWITCH_DS 10 * -  , SWITCH_DS SG, ;
: REAL, JMP-REAL,  TO-REAL, ;
: PROT,  TO-PROT, JMP-PROT, ;



DECIMAL
( --special_macros_2 TEST-JUMP ) CF: ?32  \ AvdH A5sep13
\ Not part of assembler!
\ These must always assemble, but run only on booted systems.
( Test applicable to 32 bit mode and special versions.)
WANT TO-PROT,     WANT JMP-PROT,

CODE TEST-JUMP JMP-REAL, JMP-PROT, NEXT, END-CODE
CODE TEST-MORE TO-REAL,   TO-PROT, NEXT, END-CODE
CODE TEST-SWITCH   TO-REAL,   SWITCH_DS COPY-SEG   TO-PROT,
    GDT_DS COPY-SEG   NEXT, END-CODE





DECIMAL
( LOCATED LOCATE .SOURCEFIELD ) CF: \ AvdH A6jan31
">SFA" PRESENT 0= ?LEAVE-BLOCK
\ Interpret a SOURCEFIELD heuristically.
: .SOURCEFIELD
    DUP 0 = IF "Belongs to the kernel" TYPE CR DROP ELSE
    DUP 1000 U< IF LIST ELSE
    DUP TIB @ 40000 WITHIN IF "Typed in" TYPE CR ELSE
    50 - 200 TYPE THEN THEN THEN ;
\ Show the screen or text how SC is defined
: LOCATED FOUND DUP 0= 11 ?ERROR >SFA @ .SOURCEFIELD ;
\ Idem but string from input.
: LOCATE NAME LOCATED ;




( OS-IMPORT cdED ) CF: \ AvdH A2feb05
"SYSTEM" PRESENT 0= ?LEAVE-BLOCK
CREATE cmdbuf 1000 ALLOT
: OS-IMPORT ( sc "name-forth"  -- )
     CREATE , ,
     DOES>
     2@ cmdbuf $! BL cmdbuf $C+ \ Command
     ^J PARSE cmdbuf $+!        \ Append
     cmdbuf $@ SYSTEM          \  Execute
;
?LI WANT -syscalls-
\ Change directory to SC .
: cdED   ZEN HERE HERE __NR_chdir XOS ?ERRUR ;



( cat cp echo diff grep more ls make man rm ee l unix) CF: ?LI
WANT OS-IMPORT ( and cdED )          \ AvdH A30325
"cat    "   OS-IMPORT cat
: cd NAME cdED ;      \ Change directory to "SC"
"cp     "   OS-IMPORT cp
"echo   "   OS-IMPORT echo
"diff   "   OS-IMPORT diff
"grep   "   OS-IMPORT grep
"more   "   OS-IMPORT more
"ls     "   OS-IMPORT ls
"make   "   OS-IMPORT make
"man    "   OS-IMPORT man
"rm  -i "   OS-IMPORT rm
\ "ee     "   OS-IMPORT ee  \ My favorite editor
"vi     "     OS-IMPORT ed  \ Less favorite editor
""          OS-IMPORT !!
( cat cd cp echo ed more ls rm   ee l unix) CF: ?WIMS \ AvdH
WANT OS-IMPORT       HEX
"TYPE   "   OS-IMPORT cat
"ECHO   "   OS-IMPORT echo
"MORE<  "   OS-IMPORT more
\ "LIST  "   OS-IMPORT more
"DIR    "   OS-IMPORT ls
"COPY   "   OS-IMPORT cp
"DEL    "   OS-IMPORT rm
"EDIT   "   OS-IMPORT ed
"ee     "   OS-IMPORT ee  \ My favorite editor
""          OS-IMPORT !!
DECIMAL   "CALL" PRESENT ?LEAVE-BLOCK   HEX
: cd   NAME ZEN _ _ 3B00 BDOSN 1 AND SWAP ?ERROR ;
"A:" OS-IMPORT A:   "C:" OS-IMPORT C:   "D:" OS-IMPORT D:
DECIMAL
( cat ECHO MORE more DIR COPY DEL CD edit ) CF: ?WIMS  \ AvdH
WANT OS-IMPORT       HEX
"TYPE   "   OS-IMPORT cat   \ type is really embarassing.
"ECHO   "   OS-IMPORT ECHO
"MORE<  "   OS-IMPORT MORE
\ "LIST   "   OS-IMPORT more
"DIR    "   OS-IMPORT DIR
"COPY   "   OS-IMPORT COPY
"DEL    "   OS-IMPORT DEL
"EDIT   "   OS-IMPORT ed  \ Not to conflict with: #BL EDIT
"RENAME "   OS-IMPORT RENAME
DECIMAL   "CALL" PRESENT ?LEAVE-BLOCK   HEX
: CD   NAME ZEN _ _ 3B00 BDOSN 1 AND SWAP ?ERROR ;
"A:" OS-IMPORT A:   "C:" OS-IMPORT C:   "D:" OS-IMPORT D:

DECIMAL
( DEVELOP EDITOR ME ) CF: ?WIMS    \ AvdH A9oct05





"BLOCK-INIT" PRESENT 0= ?LEAVE-BLOCK
\ Get write permission for block file.
: DEVELOP   BLOCK-EXIT   1 WARNING !   2 BLOCK-INIT ;
CR
." In order to edit screens :  `` # EDIT  '' " CR
."                          :  `` # ME    '' " CR
." Make sure to issue `` DEVELOP '' first or your screen" CR
." will not be saved." CR
." Issue `` CASE-SENSITIVE '' if needed, otherwise you" CR
." will execute an `` edit '' command (for files)." CR
( EDITOR ) CF: ?PC    \ AvdH A1oct05
CREATE EDITOR   'EDITOR DUP HIDDEN
WANT +THRU
WANT VIDEO-MODE   WANT $-PREFIX
  1 12 +THRU

HIDDEN  \ Unhide `` EDITOR ''.









( protected_editor_stuff ) CF: ?DP \ AvdH A1nov01
WANT NEW-SEL  HEX
NEW-SEL CONSTANT VID
VID PAD GET-SEL
00 PAD     C!   20 PAD 1 + C!   00 PAD 2 + C!   80 PAD 3 + C!
0B PAD 4 + C!
VID PAD PUT-SEL
: LC@ FAR@ 0FF AND ;
: LC! OVER OVER FAR@ 0FF INVERT AND >R ROT R> OR ROT ROT FAR! ;

DECIMAL





( 16_bit_editor_stuff ) CF: ?16 \ A1oct05
"VID" PRESENT ?LEAVE-BLOCK  \ Already protected mode
HEX
B800 CONSTANT VID
: LC@ FAR@ FF AND ;
: LC! OVER OVER FAR@ FF00 AND >R ROT R> OR ROT ROT FAR! ;




DECIMAL





( 32_bit_editor_stuff ) CF: ?32 \ A1oct05
"VID" PRESENT ?LEAVE-BLOCK  \ Already protected mode
HEX
B800 CONSTANT VID
  HEX 0 CONSTANT CS_START
 : LC@ SWAP 10 * + CS_START - C@ ;
 : LC! SWAP 10 * + CS_START - C! ;




 DECIMAL




( BLUE FRAME NO-FRAME --editor_stuff ) \ AvdH A1nov01
"FRAME" PRESENT ?LEAVE-BLOCK
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
"FRAME" PRESENT ?LEAVE-BLOCK
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
: V> ( buf len offset - )
    SWAP 0 DO
        OVER I +   OVER I + VA    LC@ SWAP C!
    LOOP 2DROP   ;
: PAGE PAD VL 2DUP BLANK 0 >V ;
: PG PAD 10 VW * 2DUP BLANK 0 >V ;
 DECIMAL
( editor ) HEX
: BLK>V 10 0 DO I A-L I VW * >V LOOP ;
: BLK<V   SCR @ BLOCK B/BUF ^J FILL
    10 0 DO I A-L I VW * V> LOOP UPDATE ;
: PAD>V PAD VW ROT VW * >V ;
: PAD<V PAD VW ROT VW * V> ;
13 CONSTANT HW
: ROLL^ DO I 1+ PAD<V I PAD>V LOOP ;
: ROLLv DO I PAD<V I 1+ PAD>V -1 +LOOP ;
: DEL-L DUP PAD<V VH  PAD>V VH SWAP ROLL^ ;
: POP-L DUP VH ROLLv VH PAD<V PAD>V ;
: DUP-L HW ROLLv ;
: UFL-L DUP HW ROLLv VH 1 - PAD<V PAD>V ;


 DECIMAL
( editor CURSOR ) HEX
VARIABLE CURSOR    0 CURSOR !
: CURL CURSOR @ VW / ; : CP CURSOR @ VW MOD  ;
: BIOS-CURSOR CURSOR @ VW /MOD 100 * + ;
: SET-CURSOR   BIOS-CURSOR _ 0 200 10 BIOSN 2DROP ;
: MOVE-CURSOR   ( WORD STAR)
DUP ^D = IF  1 ELSE       DUP ^S = IF -1 ELSE
DUP ^X = IF VW ELSE       DUP ^E = IF 0 VW - ELSE
DUP ^C = IF VW 8 * ELSE   DUP ^R = IF 0 VW 8 * - ELSE
DUP ^I = IF  8 ELSE       DUP ^M = IF VW CP - ELSE
0    THEN THEN THEN THEN THEN THEN THEN THEN CURSOR +! ;
: DELSTORING
    DUP ^Y = IF CURL DEL-L ELSE
    DUP ^P = IF CURL POP-L ELSE
    DUP ^U = IF CURL UFL-L ELSE
    THEN THEN THEN ;                                 DECIMAL
( editor ) HEX
: PAD-B PAD VW BLANK  ;
: GET-R PAD VW CP - CURL VW * CP + >V ;
: GET-P PAD    CP   CURL VW *      >V ;
: PUT-R PAD VW CP - CURL VW * CP + V> ;
: RUBOUT-M PUT-R NEGATE CURSOR +! GET-R ;
: INSERT-M PUT-R DUP CURSOR +! GET-R NEGATE CURSOR +! ;
: VTYPE CURSOR @ >V ;
: RUB-C PAD-B 1 RUBOUT-M SET-CURSOR  ;
: DEL-C 1 CURSOR +! RUB-C ;
: INS-C 1 INSERT-M ;
: EOL PAD-B GET-R ;  : FOL PAD-B GET-P ;
: SPL CURL DUP-L PAD-B PUT-R CURL 1+ PAD>V EOL ;
: JOL PAD-B CURL 1+ PAD<V GET-R CURL 1+ 10 SWAP ROLL^ ;

DECIMAL
( INSELETING JOINITTING )
HEX VARIABLE I-MODE   0 I-MODE !
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
   CURSOR ! SET-CURSOR  SPL CURL 1+ DEL-L JOL ;
: GET-W VH 1 - PAD<V PAD VW -TRAILING 1+ ;
: UDL-W   GET-W  INSERT-M DROP   GET-W VTYPE ;
: POP-W    UDL-W   HW VH ROLLv ;
: WORDING
  DUP ^F = IF NEXT-W ELSE   DUP ^A = IF BACK-W ELSE
  DUP ^T = IF DEL-W ELSE    DUP ^Z = IF UDL-W ELSE
  DUP ^W = IF POP-W ELSE    THEN THEN THEN THEN THEN ;
( DISPATCHER )    HEX
: AT-END VH 1 - VW * CURSOR ! SET-CURSOR ;
: DEBUG CURSOR @ AT-END .S CURSOR ! ;
: EXITING KEY 20 OR &q <> IF BLK<V THEN ;
: ROUTE BEGIN KEY
    PRINT       DELSTORING
    INSELETING  JOINITTING
    WORDING     MOVE-CURSOR     SET-CURSOR
( DEBUG)
ESC = UNTIL ;
: E-S  ( EDIT CURRENT SCREEN )
    1 I-MODE ! FRAME 0 CURSOR ! SET-CURSOR   PG
    BLK>V ROUTE EXITING  AT-END NO-FRAME ;
:  EDIT SCR ! E-S ;
: E-R 3 VIDEO-MODE EDIT ;
DECIMAL  ( Attempts at comamnd line editor)
( P ME Mini_editors )           \ AvdH A7nov25
( FORTH DIM iii/2 @ SHAPIN)
( Usage : to change line 1 of screen 3 : 3 SCR ! 1 P ... )
BASE @ HEX
: TEXT HERE C/L 1+ BLANK WORD PAD C/L 1+ CMOVE ;
: LINE DUP 0FFF0 AND 17 ?ERROR SCR @ (LINE) DROP ;
: -MOVE LINE C/L 1- ( leave \n) CMOVE ;
: P 1 TEXT PAD 1+ SWAP -MOVE ;
( Mini editor by retyping, Usage DEVELOP 12 ME)
: EL LINE C/L 1 - BLANK ;
: GL PAD C/L  ACCEPT C/L 1- MIN >R LINE PAD SWAP R> MOVE ;
: OEPS SCR @ LIST "PROCEED?" TYPE KEY 20 OR
  &y <> 2000 ?ERROR  "GO" TYPE CR ;
: ME SCR ! OEPS 10 0 DO I EL I GL LOOP UPDATE ;

BASE !
( CLEAN LO-S L-S C-S Handy_screen_tools ) \ AvdH A1oct05






: CLEAN BLOCK B/BUF OVER + SWAP DO
  I C@ 0= IF BL I C! THEN  LOOP ;

: L-S SCR @ LIST ;
: LO-S SCR @ LOAD ;
: C-S SWAP BLOCK SWAP BLOCK B/BUF CMOVE UPDATE FLUSH ;
: LISTP BASE @ 10 - 25 ?ERROR LIST ;


( BIOSI VIDEO-MODE DISK-INIT ) CF: ?PC \ AvdH A1oct05
HEX



: BIOSI SWAP 2SWAP SWAP BIOSN 2DROP ;  ( Ignore result)
: VIDEO-MODE  >R _ _ _ R> 10 BIOSN 2DROP ;
\ Reset DISK (C: 80 D: 81 A: 0 B: 1 etc.)
: DISK-INIT   _ _ 0 13 BIOSN 2DROP  ;






DECIMAL
( FARDUMP ) CF: ?PC \ AvdH A1oct
HEX:
:  FARDUMP   ( SEG ADDRESS AMOUNT - ..)
    OVER + SWAP FFF0 AND
    DO
        CR DUP H. I H. ." : "
        I
        10 0 DO
            2DUP I + FAR@ B.
            I 2 MOD IF SPACE THEN
        LOOP  [CHAR] | EMIT
        10 0 DO 2DUP I + FAR@ FF AND TO-PRINT EMIT LOOP
        [CHAR] | EMIT DROP
    10 +LOOP CR DROP
;

( FREE-BLOCK ) CF:   ?LI   \ AvdH A3sep02
\ Return the first BLOCK not written yet.
: FREE-BLOCK
    1 BEGIN DUP 'BLOCK CATCH 0= WHILE DROP 1+ REPEAT DROP ;












( SET-MEMORY TEST-MEMORY MEM-SIZE ) CF: ?PC ?32 \ AvdH A1oct17
_PREV CONSTANT PROBE  \ Use this as a probe.
DECIMAL  123456789 CONSTANT MAGIC  HEX
HERE 10,0000 PROBE + > 0D ?ERROR \ Probe must be in first Mb!
VARIABLE (MEM-SIZE)   1000 (MEM-SIZE) ! \ Megabytes
: FAIL?   DUP >R   @ MAGIC =   MAGIC R@ ! R> @ MAGIC <>  OR ;
: SET-MEMORY   (MEM-SIZE) @ 2 DO
   \ ^M EMIT ." probing " I . 4 SPACES
   I 14 LSHIFT  PROBE +
   DUP FAIL? IF DROP I (MEM-SIZE) ! LEAVE THEN   I SWAP !
LOOP ;
: TEST-MEMORY   (MEM-SIZE) @ 2 DO
   I 14 LSHIFT  PROBE +  @ I <>  IF I (MEM-SIZE) ! LEAVE THEN
   LOOP ;
PROBE @   MAGIC PROBE !   SET-MEMORY TEST-MEMORY PROBE !
(MEM-SIZE) @ CONSTANT MEM-SIZE DECIMAL
( SEL-DUMP dump_a_selector ) \ AvdH A1nov02
WANT DH. HEX 1 1 +THRU
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

( ************** application *********************************)

EXIT

Useful for user programs:
   - midi










( -midi_driver- sendmidi ) ?WI               \ AHCHB5apr15
WANT LOAD-DLL:           WANT DLL-ADDRESS:
"WINMM.DLL" LOAD-DLL: WINMM
"midiOutOpen"     'WINMM DLL-ADDRESS: midiOutOpen
"midiOutShortMsg" 'WINMM DLL-ADDRESS: midiOutShortMsg
"midiOutClose"    'WINMM DLL-ADDRESS: midiOutClose

VARIABLE MidiHandle

\ Open midi, i.e. fill MidiHandle.
: openmidi  0 0 0 -1 MidiHandle midiOutOpen CALL 2001 ?ERROR ;
\ Close midi channel in MidiHandle
: closemidi MidiHandle @ midiOutClose CALL 2002 ?ERROR ;
\ Send out 3 byte MESSAGE contained in one cell.
: sendmidi MidiHandle @ midiOutShortMsg CALL 2003 ?ERROR ;

( **************communication with stand alone hd ************)















( --hd_LBA utils_for_stand_alone_disk ) CF: ?PC ?16
WANT ASSEMBLERi86   WANT DISK-INIT   WANT +THRU
( backup and restore a stand alone hard disk system to floppy )
( run from a booted floppy system )
( this is for a 16 bit system, because the assembly assumes )
( so. If you have a 32 bit system, there are easier ways.   )
: --hd_LBA ;
256 CONSTANT #BLOCKS
1 5 +THRU DECIMAL







( --hd_LBA R|W-BLOCK READ-BLOCK RW-BUFFER ) CF: ?16 ?PC HEX
HERE DUP 3 + 3 INVERT AND SWAP - ALLOT HERE B/BUF ALLOT
CONSTANT RW-BUFFER
CREATE PARAM-BLOCK 10 C, 0 C, 2 , ( 2 sectors/block)
RW-BUFFER , 0 , HERE 2 CELLS ALLOT 0 , 0 , CONSTANT BL#
ASSEMBLER
 : R|W-BLOCK   POP|X, AX|   ADD, X| T| AX'| R| AX|
  MOV|FA, X'| BL# W,   PUSH|X, SI|
  MOVI|X, BX| ( FUNCTION CODE ) IW,   MOVI|X, DX| 0080 IW,
  MOVI|X, SI| PARAM-BLOCK SWITCH_DS 10 * -  IW,
  TO-REAL, SWITCH_DS COPY-SEG
 XCHG|AX, BX| INT, 13 IB, PUSHF, POP|X, BX|
 TO-PROT, GDT_DS COPY-SEG
  POP|X, SI|   PUSH|X, BX|  NEXT, ; PREVIOUS
CODE READ-BLOCK 4200 R|W-BLOCK  END-CODE
CODE WRITE-BLOCK 4300 R|W-BLOCK  END-CODE     DECIMAL
\ --hd_LBA (HWD) (HRD) (FRD) (FWD) \ CF: ?16 ?PC HEX
 CF: ?16 ?PC

( Write the default buffer to hard disk at 32-bit POSITION)
: (HWD) SWAP WRITE-BLOCK 1 AND . ;
( Read the default buffer from hard disk at 32-bit POSITION)
: (HRD) SWAP READ-BLOCK 1 AND . ;
DECIMAL
( As R|W but relative, counting from ``OFFSET''. )
: RELR|W SWAP OFFSET @ + SWAP R|W ;
( Read absolute BLOCK from floppy into default buffer.)
: (FRD) RW-BUFFER SWAP 1 R|W ;
( Write absolute BLOCK to floppy from default buffer.)
: (FWD) RW-BUFFER SWAP 0 R|W ;


( --hd_LBA SWAP-FLOPPY ) CF: ?PC ?16 \ AvdH A1oct07
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
WANT --hd_LBA
\ Prompt for floppy created with ``BACKUP>FLOPPY''
\ Restore to hard disk ``DBS'' 1400 K from floppy.
: RESTORE<FLOPPY SWAP-FLOPPY
1400 0 DO I (FRD) 2DUP I S>D D+ (HWD) LOOP 2DROP ;

\ Copy the kernel (first 64K of ``DBS'' to raw floppy.
: BACKUP-KERNEL  SWAP-FLOPPY 64 0 DO I S>D (HRD) I (FWD) LOOP ;

\ Copy the BLOCKS (#BLOCKS at 64K of ``DBS'') to BLOCKS.BLK.
: BACKUP-BLOCKS
#BLOCKS 0 DO I 64 + S>D (HRD) RW-BUFFER I 0 RELR|W LOOP ;



( INSTALL-KERNEL RESTORE-BLOCKS ) CF: ?PC ?16
WANT --hd_LBA

\ Copy the kernel (first 64K of ``DBS'') from raw floppy.
: INSTALL-KERNEL SWAP-FLOPPY 64 0 DO I (FRD) I S>D (HWD) LOOP ;

\ Copy the BLOCKS (#BLOCKS at 64K of ``DBS'') from BLOCKS.BLK.
: RESTORE-BLOCKS
#BLOCKS 0 DO RW-BUFFER I 1 RELR|W I 64 + S>D (HWD) LOOP ;







( SECTORS/TRACK #HEADS ) CF: \ AvdH A1oct10
1 2 +THRU
EXIT
Get information directly after booting aboot the hard disk
The BIOS puts this in the real memory area, and it is
no longer available if e.g. from DOS sufficiently large
programs have been run.
Directly after a Forth has booted, it is safe,
of course.







( SECTORS/TRACK #HEADS ) CF: ?16 ?PC \ AvdH A1oct10
HEX
\ See Ralph Brown's table 03196
\ Far address of interrupt 41
: (int41) 0 041 4 * ;
\ Far address of HD 1 table.
: (hd1) (int41) CELL+ FAR@ (int41) FAR@ ;
\ Number of heads on hard disk one.
(hd1) 2 + FAR@ 0FF AND CONSTANT #HEADS
\ Sectors per track for hard disk one.
(hd1) 0E + FAR@ 0FF AND CONSTANT SECTORS/TRACK
DECIMAL




( SECTORS/TRACK #HEADS FORTH-SIZE MEDIA-HD ) CF: ?32 ?PC
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
F8 CONSTANT MEDIA-HD \ For hard disk.
DECIMAL
\ The SIZE of Forth (kernel +blocks) in blocks.
#BLOCKS OFFSET @ + CONSTANT FORTH-SIZE

( INSTALL-FORTH-ON-HD ) CF: ?SA ?32 \ AvdH A1oct11
WANT +THRU
\ Elective and configuration screen
\ You can overrule here for manual installation
\ ?? CONSTANT FORTH-SIZE   ?? CONSTANT MEDIA-HD
WANT FORTH-SIZE         WANT MEDIA-HD
\ ?? CONSTANT #HEADS    ?? CONSTANT SECTORS/TRACK
\ ?? CONSTANT MEM-SIZE
WANT #HEADS  WANT SECTORS/TRACK   WANT MEM-SIZE
1 6 +THRU
CR ." If you really want to install on hard disk"
CR ." issue the command: INSTALL-FORTH-ON-HD"
\ Re-installs a sector-and-track ciforth to a hard disk (or
\ floppy). This is a user utility, so it can be run for other
\ type ciforth's. But then it only explains to the user what is
\ going wrong.
( --disclaimer_INSTALL_FORTH_ON_HD ) CF: ?HD \ AvdH A1oct11

\ This utility is intended for sectors & track installations
\ so for floppy compatible hard disks.

." This is the wrong utility for your Forth ." CR
." Try INSTALL-KERNEL and RESTORE-BLOCKS instead." CR

QUIT







( flush-key fatal-questiON last-chance show ready ) CF: ?32
\ Throw away any keys that are typed ahead
: flush-key   BEGIN KEY? WHILE KEY DROP REPEAT ;
\ Stop, unless the user types Y.
: stop?   KEY &Y <>  IF ." ABONDANNED! " CR QUIT THEN ;
\ Ask QUESTION and require ``Y''
: fatal-question   flush-key   "??  Y/N" TYPE CR stop? ;
: last-chance
   ." Last chance to quit!" CR
   ." If you type Y your hard disk will be overwritten." CR
   ." Are you sure you want to install?" fatal-question ;

: show ^M EMIT ." BLOCK" 4 .R 5 SPACES ;

: ready ." Press the reset button, to boot your new FORTH"
    CR ;
( PATCH-NEW-FORTH PATCH-THIS-FORTH ) CF: ?FD ?32 \ AvdH A1oct12
WANT #HEADS  WANT MEDIA-HD          HEX
WANT FORTH-SIZE

\ What we need then
CREATE buffer FORTH-SIZE B/BUF * ALLOT
\ Now patch into the boot record the hard disk dimensions
\ And into the access definition of this Forth
: PATCH-NEW-FORTH   \ Overlapping 32 bits stores, big endian!
   MEDIA-HD          buffer 15 + C!
   SECTORS/TRACK     buffer 18 + !     \ Actually 16 bit
   #HEADS            buffer 1A + ! ;   \ Actually 16 bit
: PATCH-THIS-FORTH   \ This Forth now accesses hard disk
   80                DRIVE C!
   SECTORS/TRACK     DRIVE 1+ C!
   #HEADS            DRIVE 2 + C! ;
( --disclaimer_INSTALL_FORTH_ON_HD ) CF: ?FD \ AvdH A1oct11

CR ." You are about to install Forth on your hard disk"
CR ." making it unusable for any other purpose."
CR ." No dual boot system, no partition or even a master"
CR ." boot record will remain, and on some computers e.g. "
CR ." Compaq some data will be permanently lost."
CR ." Go on at your own risk. No guarantees."
CR CR " GO ON " fatal-question

CR ." Analysing..." CR
CR ." The number of heads on your hard disk is reported: "
WANT #HEADS    WANT B.    DECIMAL
#HEADS DUP . B. &H EMIT


( --disclaimer_INSTALL_FORTH_ON_HD ) CF: ?FD ?32 \ AvdH A1oct17
CR ." The number of sectors/track is reported: "
SECTORS/TRACK DUP . B. &H EMIT
CR ." (If this is incorrect, you have to configure manually)"
CR CR " Do you believe this" fatal-question
CR ." Analysing..." CR

DECIMAL
CR ." The amount of Megabytes on your system is probed as: "
WANT MEM-SIZE   DECIMAL
MEM-SIZE DUP . HEX 0 .R &H EMIT
CR ." (If this is incorrect, you have to configure manually)"
CR CR " Do you believe this?" fatal-question



( INSTALL-FORTH-ON-HD PATCH-MEM ) CF: ?FD \ AvdH A5sep26
\ Read into BUFFER absolute block NUMBER , leave NEXT buffer.
: read+ OFFSET @ -   BLOCK     OVER B/BUF MOVE   B/BUF + ;
\ Cannot read directly into the buffer because it is above 1 Mb
: READ-FORTH  buffer FORTH-SIZE 0 DO I show I read+ LOOP DROP ;
\ Cannot write directly into the buffer because it is above 1 M
: write+   OFFSET @ - (BUFFER) CELL+ CELL+   OVER SWAP   B/BUF
   MOVE     UPDATE B/BUF + ;
: WRITE-FORTH  last-chance
   buffer FORTH-SIZE 0 DO I show I write+ LOOP DROP ;
: PATCH-MEM BM -  \ Patch the memory size of a BUFFER.
 5 0 DO MEM-SIZE 14 LSHIFT EM - OVER I CELLS + +ORIGIN +!
LOOP  MEM-SIZE 14 LSHIFT SWAP 'EM >DFA + ! ;
: INSTALL-FORTH-ON-HD  DRIVE @ 0 WARNING ! READ-FORTH
buffer PATCH-MEM PATCH-NEW-FORTH PATCH-THIS-FORTH WRITE-FORTH
EMPTY-BUFFERS 1 WARNING ! DRIVE ! ready ;
( --hd_driver_standalone_) CF: ?PC ?32 ?HD HEX

  1 8 +THRU













( 250 Redefine R|W to accomodate larger addresses. A1may05AH)
NAMESPACE SYS ONLY FORTH
 DP @ LOW-DP @  DP ! LOW-DP ! SYS DEFINITIONS
( 247 248 ) THRU HEX
: NEW-COLD
EMPTY-BUFFERS   FIRST _PREV !
0 CELLS +ORIGIN DUP CELL+ @  40 CELLS CMOVE
1<>64 LOAD-ALL (ABORT) ;
'NEW-COLD 'COLD 3 CELLS MOVE
DP @ LOW-DP @  DP ! LOW-DP ! PREVIOUS DEFINITIONS DECIMAL
  SYS
: SAVE-SYSTEM
   U0 @   0 +ORIGIN   40 CELLS CMOVE ( Save user variables)
   STORE-ALL ;
PREVIOUS

( 247: Redefine R|W to accomdate larger addresses. A1aug05AH)
HEX
( The screen BUFFER is apparently virgin)
: (FREE?) B/BUF 0 DO DUP I + C@ &v - IF UNLOOP DROP 0 EXIT THEN
   LOOP DROP -1 ;
8,F000 CONSTANT RW-BUFFER    ( A 64 kBYTE BUFFER)
( Switch between reading 64K and 1 K)
: 1<>64 LBAPAR 2 + 82 TOGGLE ;

( All: address block -- addres' block' And : SIZE 64K)
: READ++   1<>64 DUP RW-BUFFER SWAP 1 R|W
OVER RW-BUFFER SWAP 1,0000 MOVE   40,0001,0000. D+ 1<>64 ;
: WRITE++  1<>64 OVER RW-BUFFER 1,0000 MOVE
  DUP RW-BUFFER SWAP 0 R|W 40,0001,0000. D+ 1<>64 ;

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
( hd_driver1 PATCH-CHUNK ) CF: ?PC ?32 HEX \ AH&CH A1sep01
SYS  0800,0000 B/BUF / CONSTANT CHUNK-SIZE  ( BLOCKS PER CHUNK)
: CHUNK-START  OFFSET @ 40 - ;
: CURRENT-CHUNK
CHUNK-START  CHUNK-SIZE / -6 +ORIGIN OVER SWAP C! ;
( -- offset and bl# where to patch OFFSET )
: OFFSET-/MOD 'OFFSET >DFA @ +ORIGIN 7C00 - B/BUF /MOD ;
: CHECK CURRENT-CHUNK = 0D ?ERROR ;
: PATCH-CHUNK
     OFFSET-/MOD >R DROP
     RW-BUFFER  CURRENT-CHUNK CHUNK-SIZE * R@ + 1 R|W
     CHUNK-SIZE * DUP 40 + OFFSET-/MOD DROP RW-BUFFER + !
     RW-BUFFER  SWAP R> + 0 R|W
; DECIMAL


( hd_driver2 WIPE-HD )  CF: ?PC ?32 \ AH A1may3
: WIPE-BUFFER RW-BUFFER B/BUF &v FILL ;
: WRITE-BUFFER RW-BUFFER SWAP OFFSET @ + 64 - 0 R|W ;
: CHECK-RANGE 589 64 + CHUNK-SIZE WITHIN 0= 13 ?ERROR ;
: SHOW DUP 100 MOD 0= IF . ^M EMIT ELSE DROP THEN ;
: WIPE-RANGE  DUP CHECK-RANGE SWAP DUP 1- CHECK-RANGE SWAP
    WIPE-BUFFER
    DO I SHOW I WRITE-BUFFER LOOP ;
HEX 130,3BBF CONSTANT LAST-BLOCK DECIMAL
CHUNK-SIZE 8 * CONSTANT FIRST-BLOCK
: doit  CHUNK-SIZE 589 64 + CR WIPE-RANGE ;
: WIPE-HD WIPE-BUFFER FIRST-BLOCK
     BEGIN RW-BUFFER OVER 0 R|W DISK-ERROR 1 AND 0= WHILE
        DUP SHOW 1+ REPEAT DROP ;


( hd_driver3 FIRST-FREE ) CF: ?PC ?32 \ AH A1may3
WANT BIN-SEARCH
: FREE? RW-BUFFER SWAP 1 R|W
DISK-ERROR 1 AND   RW-BUFFER (FREE?)   OR ;
: NON-FREE? FREE? INVERT ;
: FIRST-FREE  ( -- FIRST FREE BLOCK IN BACKUP AREA)
FIRST-BLOCK LAST-BLOCK 'NON-FREE? BIN-SEARCH 1 + ;
: FNTB ( First free in current chunk)
  CHUNK-START CHUNK-SIZE OVER + 'NON-FREE? BIN-SEARCH 1+ ;
: SAVE-COMMENT 200 BLOCK FIRST-FREE 0 R|W ;






( hd_driver4 BLMOVE BLMOVE-FAST BACKUP ) CF: ?PC ?32 \ AvdH
HEX
: BLMOVE 0 DO  ( as MOVE for blocks.)
  SWAP RW-BUFFER OVER 1 R|W 1+   SWAP RW-BUFFER OVER 0 R|W 1+
  DUP SHOW KEY? IF UNLOOP EXIT THEN  LOOP . . ;
: BLMOVE-FAST  ( as MOVE for blocks, ONLY MULTIPLES OF 64K.)
 1<>64 0 DO
         SWAP RW-BUFFER OVER 1 R|W 40 +
         SWAP RW-BUFFER OVER 0 R|W 40 +
         DUP SHOW KEY? IF UNLOOP EXIT THEN
40 +LOOP . . 1<>64 ;
: BACKUP ( BACKUP THE CURRENT CHUNK TO PRISTINE DISK )
   CHUNK-START FNTB OVER - FIRST-FREE SWAP BLMOVE-FAST ;
: SAVE-CHUNK DUP CHECK
 CURRENT-CHUNK CHUNK-SIZE * OVER CHUNK-SIZE * CHUNK-SIZE
BLMOVE-FAST PATCH-CHUNK ; DECIMAL
( hd_driver5 I-INSPECT AH) CF: ?PC ?32 HEX \ AH A1sep01
: ASCII? DUP BL 7F WITHIN SWAP ^J = OR ;
( SC contains all ``ASCII'' )
: ALL-ASCII? OVER + SWAP DO I C@ DUP ASCII? 0=
IF DROP UNLOOP 0 EXIT THEN 0= IF UNLOOP -1 EXIT THEN LOOP -1 ;
: INSPECT RW-BUFFER SWAP 1 R|W
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

( ERATOSTHENES SIEVE 1 Variables - A. van der Horst         )
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

( ERATOSTHENES SIEVE 2 Pretty printing - A. van der Horst   )
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

( ERATOSTHENES SIEVE 3 Bit manipulation - A. van der Horst  )
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


( ERATOSTHENES SIEVE 4 Bit manipulation - A. van der Horst  )
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

( ERATOSTHENES SIEVE 5 Main program - A. van der Horst     )
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
WANT BIN-SEARCH
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



( ************** end of lab ****************************)
( This block has an empty index line to serve as an     )
(  end sentinel                                         )











( 3344  last line.)
