\ Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
\ $Id$
\ Convert a text file to a .lab file.

REQUIRE ARG[]

\ TYPE on the error channel
: ECR ^J DSP@ 1 ETYPE DROP ;
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
    "Error in program " ETYPE   0 ARG[] ETYPE   ECR
    "    at input line " ETYPE   LINE @ E. CR
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

\ Output STRING as a 64 char line, but abort if too long.
: OUTPUT-LINE
    DUP 63 > IF "line too long" ERROR THEN
    63 OVER - >R TYPE R> SPACES CR ;

\ Filter from input to output, make all lines 64 chars and warn
\ for any inproper index lines.
: MAIN
    BEGIN '(ACCEPT) CATCH 0= WHILE
        -TRAILING   1 LINE +!
        LINE @ 16 MOD 1 = IF 2DUP CHECK-INDEX-LINE THEN
        OUTPUT-LINE
    REPEAT ;
