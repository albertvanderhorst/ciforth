\ #  Copyright 2000 (c): Albert van der Horst, Dutch Forth Worksshop by GPL
8 LOAD  \ get libraries
38 LOAD         \ $@=
243 LOAD        \ Binary search library
146 LOAD            \ Linux system access ARGC ARGV TURNKEY

' $@ ALIAS @+

\ Print an error message and return to the OS
\ In good Unix style, the name fo the program and
\ usage is printed.
: MY-ERROR
    " : FATAL! " TYPE CR
    "Usage: " TYPE CR
    ARGV @ CTYPE " <integer1> <integer2> <integer3>" TYPE CR
    BYE ;

\ Install it as the handler for uncaught exceptions.
\ No way one could use the ``uchi1'' program
\ to fire up a Forth interpreter and hose Linux.
: INSTALL-ERROR
    -1 WARNING !
    ' MY-ERROR >PFA @   ' (ABORT) >PFA !
;

\     : \D ;    IMMEDIATE \ Use debug lines.
      : \D POSTPONE \ ;  IMMEDIATE \ Comment out debug lines

\ Create an array of LEN elements of size STRIDE .
\ Run time: For an INDEX leave the ADDRESS of the element
\ with that INDEX
: ARRAY CREATE DUP , * ALLOT DOES> >R R@ @ * R> CELL+ + ;
\D 3 2 CELLS ARRAY MONEY   0 MONEY 2 3 * CELLS ERASE
\D 12 1 MONEY ! 22 2 MONEY !
\D ." EXPECT : 8 0 0 12 0 22 0 123 :" 123
\D ' MONEY >BODY @+ .  @+ .  @+ .  @+ .  @+ .
\D @+ .  @+ . DROP . CR

VARIABLE LINE#     0 LINE# !  \ Line counter
30000 CONSTANT MAX-TIB
10000 CONSTANT MAX-LINES
MAX-LINES 2 CELLS ARRAY LINES

: LAST LINE# @ 1- ;

\ Save the two strings SC1 and SC2 permanently
\ and leaves pointers to those at POSITION.
: SAVE-AT-POS
    LINES >R
    $, R@ CELL+ !    $, R@ !
    RDROP ;

\ Analyse a string CONSTANT and save at a new
\ position in the ``LINES'' array
: SAVE-LINE
    ^: I $S     \ Split at ^I
    LINE# @ SAVE-AT-POS
    1 LINE# +!
;
\D ." EXPECT aap123 :" 123 "aap" SAVE-LINE 0 LINES @ $@ TYPE . CR

\ Compares record INDEX with the last record:
\ "that record COMPARES less than the last one"
: COMP-LINE
    DUP LINES @   LAST LINES @ $@= DUP IF
        0 < SWAP DROP
    ELSE
        DROP
        LINES CELL+ @  LAST LINES CELL+ @ $@= 0 <
    THEN
;

\ Get the INDEX in the array where the last record
\ really belongs
: WHERE 0 LAST ' COMP-LINE BIN-SEARCH 1+
   DUP 1 = 0 COMP-LINE 0= AND IF
        DROP 0
   THEN
;

\ 2 CONSTANT WHERE

\ Move the last in the array to the right position
: MOVE-IN-POSITION
\     "WHERE IS" TYPE WHERE . CR EXIT
    LINE# @ 1 = IF EXIT THEN
    WHERE >R
    R@ LINES
    R@ 1+ LINES
    LINE# @ LINES R@ LINES -  MOVE
    LINE# @ LINES   R@ LINES  2 CELLS   MOVE
    RDROP
;

\ Get a new record to the end of LINES,
\ leave "There IS more"
: GET-RECORD
    TIB @ DUP MAX-TIB ACCEPT DUP        \D .S
    IF SAVE-LINE -1 ELSE SWAP DROP THEN
;
\ Get a new record to the end of LINES,
\ leave "There IS no more"
\ THIS ONE HANGS! WHY?
: GET-RECORD2
    TIB @ DUP MAX-TIB ACCEPT >R
    R@ .S IF SAVE-LINE ELSE 2DROP THEN
    R> 0= ;

: PRINT-FULL
            LINES @+ $@ TYPE
            ^: I EMIT
\D          &| EMIT     \ Show we are not reflecting input
            @ $@ TYPE
;
: PRINT-HALF
            LINES CELL+ @ $@ TYPE
;
: PUT-ALL
        0 PRINT-FULL
        LINE# @ 1 ?DO
          I LINES @ I 1- LINES @ $@= IF
               CR I PRINT-FULL
          ELSE
                ^: I EMIT   I PRINT-HALF
          THEN

        LOOP
        CR
;
: GET-ALL
    0 LINE# !
    BEGIN
        GET-RECORD
    WHILE
\D        LINE# ? ^: M EMIT
        MOVE-IN-POSITION
    REPEAT
;
: main INSTALL-ERROR GET-ALL PUT-ALL BYE ;
' main "uchi3" TURNKEY
