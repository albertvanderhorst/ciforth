\ Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
\ $Id: toblk.frt,v 5.5 2017/10/26 03:40:58 albert Exp $
\ Convert a text file <arg1> to a .lab file.<arg2> .
\ Without args act like a filter.

\ Blocks with built in separators ciforth style.
C/L 1- CONSTANT C/C     \ Content characters
^J CONSTANT LINE-END    \ Line separator
\ Classical blocks.
\ C/L CONSTANT C/C
\ BL CONSTANT LINE-END   \ Not a separator, and may be overwritten.

WANT WRITE-LINE
WANT READ-LINE
WANT ARG[]

\ TYPE on the error channel
: (etype) 2 WRITE-FILE DROP ;
'(etype) 'ETYPE 3 CELLS MOVE

: ECR ^J DSP@ 1 ETYPE DROP   ^M DSP@ 1 ETYPE DROP ;
: E. S>D 0 (D.R) ETYPE ;

\ For STRING : it IS a possible index line.
: INDEX?   0= SWAP C@ &( = OR ;

\ The information that there are some non-index lines has not been printed.
VARIABLE INFO-LINE   1 INFO-LINE !

\ Print the message about non-index lines, if this is the first.
: INFO-MESSAGE
    INFO-LINE @ IF
        "Possible non-index line in block file" ETYPE ECR   0 INFO-LINE !
    THEN ;

\ Current input line.
VARIABLE LINE   0 LINE !

\ Print error MESSAGE and exit.
: ERROR
    "Error at input line " ETYPE   LINE @ E. ECR
    "   " ETYPE ( m) ETYPE ECR
    1 EXIT-CODE !   BYE ;

\ Check STRING for being an index line, and give message if not.
: CHECK-INDEX-LINE
    2DUP INDEX? IF
        2DROP
    ELSE
       INFO-MESSAGE
       " at block " ETYPE   LINE @ 16 / E. ":" ETYPE ETYPE ECR
    THEN ;

VARIABLE INPUT        VARIABLE OUTPUT   1 OUTPUT !
\ Check and open files passed as arguments, leaving in above variables.
: GET-HANDLES
    3 ARGC = IF
    1 ARG[] 0 OPEN-FILE THROW INPUT !
    2 ARG[] "" 2OVER PUT-FILE 1 OPEN-FILE THROW OUTPUT !
    ELSE 1 ARGC = IF   0 INPUT !   1 OUTPUT ! ELSE
    "Usage: toblock [fromfile tofile]" ERROR THEN THEN ;

\ Close the files passed as arguments, using above variables.
: CLOSE-HANDLES INPUT @ CLOSE-FILE THROW OUTPUT @ CLOSE-FILE THROW ;

\ Move the area from START plus one to END down by one character.
: DOWN  OVER - 1- >R   DUP 1+ SWAP R> MOVE ;

\ For STRING leave the first POSITION of compressible white space.
: FIND-WHITE OVER + 2 - DO
    I C@ ?BLANK I 1+ C@ ?BLANK AND IF I UNLOOP EXIT THEN
-1 +LOOP 0 ;

\ Make STRING one char smaller by leaving out white space, return IT.
: SHOEHORN
    2DUP FIND-WHITE DUP 0= IF "Shoehorn failed" ERROR THEN
    >R 2DUP + R> SWAP DOWN   1- ;

\ Checks a line STRING, leaves an equivalent STRING that fits in C/C
\ characters, possibly shoehorned. Or die in so trying.
: CHECK
    DUP C/C > IF
    "Input line " ETYPE   LINE @ E. " is too long: " ETYPE 2DUP ETYPE ECR
    BEGIN DUP C/C > WHILE SHOEHORN REPEAT
    "Shoehorned into: " ETYPE 2DUP ETYPE ECR
    1 INFO-LINE !
    THEN
;

CREATE OUTPUT-BUFFER C/L ALLOT
  LINE-END OUTPUT-BUFFER C/C + C!

\ Output STRING as a C/L char line, but abort if too long.

: OUTPUT-LINE
    CHECK
    OUTPUT-BUFFER C/C BL FILL  OUTPUT-BUFFER SWAP CMOVE
    OUTPUT-BUFFER C/L OUTPUT @ WRITE-FILE THROW ;


\ make all lines C/L chars and warn
\ for any inproper index lines.
: MAIN    GET-HANDLES
    BEGIN PAD DUP 256 INPUT @ READ-LINE THROW WHILE
        -TRAILING   1 LINE +!
        LINE @ 16 MOD 1 = IF 2DUP CHECK-INDEX-LINE THEN
        OUTPUT-LINE
    REPEAT
    CLOSE-HANDLES
;
