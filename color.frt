( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ Fix up the analyser with information about what are duplicators.

HEX  1B CONSTANT ESC DECIMAL

CREATE ESCAPE-COLOR
ESC C, &[ C, HERE _ C, _ C, &; C, HERE _ C, _ C, &; C, &1 C, &m C,
CONSTANT ESCAPE-FORE   CONSTANT ESCAPE-BACK

: SEND-COLOR-ESCAPE ESCAPE-COLOR 10 TYPE ;

: fore ESCAPE-FORE SWAP CMOVE ;
: back ESCAPE-BACK SWAP CMOVE ;

: FORE-COLOR CREATE $, DROP DOES> $@ fore SEND-COLOR-ESCAPE ;

: BACK-COLOR CREATE $, DROP DOES> $@ back SEND-COLOR-ESCAPE ;

: COLOR-ESCAPE CREATE $, DROP DOES> ESC EMIT &[ EMIT $@ TYPE ;

\ Put the screen in a mode such as to print the chars in the color.
"37"    FORE-COLOR    white
"37;1m" COLOR-ESCAPE  white2
"36"    FORE-COLOR    aqua
"32"    FORE-COLOR    green
"33"    FORE-COLOR    yellow
"31"    FORE-COLOR    red
"35"    FORE-COLOR    pink
"34"    FORE-COLOR    fblue
"30"    FORE-COLOR    f30

\ Put the screen in a mode such as the background color.
"40"    BACK-COLOR    black
"41"    BACK-COLOR    b41
"42"    BACK-COLOR    b42
"43"    BACK-COLOR    b43
"44"    BACK-COLOR    blue
"45"    BACK-COLOR    b45
"46"    BACK-COLOR    b46
"47"    BACK-COLOR    b47
"48"    BACK-COLOR    b48
"49"    BACK-COLOR    bwhite


\ Print text in white, not bold.
\ This is sufficient to overrule coloring.
"0m" COLOR-ESCAPE default-white
\ Print text with foreground and background colors swapped.
"7m" COLOR-ESCAPE reverse

CREATE RENDER-TABLE    \ Colors in relation to stack effect.
\ Unknown     0              1              2
'pink     , 'white    ,     'aqua    ,     'green   ,
\ 3          >3             ....
'yellow   , 'red      ,     'red      ,     'red      ,
'red      , 'red      ,     'red      ,     'red      ,
\   ...                      >3             variable
'red      , 'red      ,     'red      ,     'white      ,
\ 'red      , 'red      ,     'red      ,     'pink     ,

\ Select the color belonging to the stack EFFECT.
: SELECT-COLOR  DUP CELLS RENDER-TABLE + @ EXECUTE
   15 = IF blue THEN ;

\ For an XT get the stack effect according to table.
\ If it is a duplicator, make it look like a no input thingy.
: SE DUP >FFA @ FMASK-DUP AND >R SE@ SE:1>2
      R> IF SWAP - 1+ 1 SWAP THEN
;

\ For an XT return its name as a (Forth) STRING constant.
: NAME@  >NFA @ $@ ;

\ Print a blob in color belonging to stack EFFECT.
\ (Actually because of limitations it is printed in the non-bright
\ version, such that yellow looks as light brown.)
: COLORED-BLOB    SELECT-COLOR   reverse   BL EMIT ;

\ For DEA print the name using colors showing the stack effect.
: COLOR-ID.
   DUP >FFA @ 1 AND IF DROP EXIT THEN
   DUP SE SWAP SELECT-COLOR >R
   NAME@
   OVER C@ EMIT
   SWAP 1+ SWAP 1-
   DUP IF
       1- white 2DUP TYPE   + C@
       R> SELECT-COLOR EMIT
    ELSE
        2DROP R> COLORED-BLOB
   THEN
   black default-white 2 SPACES ;

\ ******************* PATCH UP THE DECOMPILER WITH COLORS ***************
\                      DOESN'T BELONG HERE

WANT RESTORED

\ Install the behaviour of DEA into the behaviour of DEA.
: REVECTOR 3 CELLS MOVE ;

\ Have an indication of N outputs or inputs on the stack.
: X-BLOB 1 +  COLORED-BLOB default-white ;

\ Revector to have colors.
\ If the user defines a blob not belonging to the cracker: oops!
: DO-COLOR
   'COLOR-ID.   'ID.    REVECTOR
   "BLOB" PRESENT DUP IF   'X-BLOB OVER REVECTOR   THEN  DROP
;

\ No more colors.
: NO-COLOR   'ID. RESTORED   'X-BLOB RESTORED ;

FILL-ALL
