\ Fix up the analyser with information about what are duplicators.

REQUIRE ALIAS

HEX  1B CONSTANT ESC DECIMAL

'COUNT ALIAS C@+

HEX  1B C
CREATE ESCAPE-COLOR
1B C, &[ C, HERE _ C, _ C, &; C, HERE _ C, _ C, &; C, &1 C, &m C,
CONSTANT ESCAPE-FORE   CONSTANT ESCAPE-BACK

\ This character doesn't belong to a word, just shows the stack effect.
\ FE CONSTANT SHOWING-CHAR
DECIMAL


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
\ HEX : default-white    black white 1B EMIT &[ EMIT &m EMIT ; DECIMAL
"0m" COLOR-ESCAPE default-white
"7m" COLOR-ESCAPE reverse

CREATE RENDER-TABLE    \ Colors in relation to stack effect.
\ Unknown     0              1              2
'pink     , 'white    ,     'aqua    ,     'green   ,
\ 3          >3             ....
'yellow   , 'red      ,     'red      ,     'red      ,
'red      , 'red      ,     'red      ,     'red      ,
\   ...                      >3             variable
'red      , 'red      ,     'red      ,     'pink     ,

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

\ Print the name observing colors.
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

\ Install the behaviour of DEA into the behaviour of DEA.
: REVECTOR 3 CELLS MOVE ;

\ Print the name of a DEA in the old uncolored fashion.
: OLD-ID.   OLD: ID. ;

\ Eliminate space after the old ``ID.'' .
: BACK-ID. ^H EMIT ^H EMIT ^H EMIT ;

\ Plain decompilation of item at ADDRESS. Leave incremented ADDRES.
: ITEM   DUP @ ID. CELL+ CELL+ ;

\ Plain decompilation of control item at ADDRESS. Leave incremented ADDRES.
\ (These have an offset following inline.)
: C-ITEM ITEM CELL+ ;

\ Have an indication of one output on the stack.
: 1-OUTPUT 1 COLORED-BLOB default-white SPACE ;

\ Print CONSTANT colored.
: COLOR-H..   H. 1-OUTPUT ;

\ Replacement for ``-dea'' decompiler
: -dea-new    CELL+ DUP @ &' EMIT OLD-ID. BACK-ID. 1-OUTPUT CELL+ ;

\ Replacement for ``-br' decompiler
: -br-new     DUP @ ID. ." [ " -con ." , ] " ( -target) ;

: DO-COLOR
   'COLOR-ID.   'ID.    REVECTOR \ Install it.
   'COLOR-H..   'H..    REVECTOR \ Print literals colored
   'ITEM        '-dea   REVECTOR \ Print 'SOME colored
   'C-ITEM      '-do    REVECTOR \ Print
   'C-ITEM      '-qdo   REVECTOR \  controls
   'C-ITEM      '-lo    REVECTOR \  more primitive
   'C-ITEM      '-pl    REVECTOR \  but colored.
   '-br-new     '-0br   REVECTOR \ Print stack effect
   '-br-new     '-br    REVECTOR \  of branching.
;

: NO-COLOR
   'ID.    RESTORED
   'H..    RESTORED
   '-dea   RESTORED
   '-do    RESTORED
   '-qdo   RESTORED
   '-lo    RESTORED
   '-pl    RESTORED
   '-0br   RESTORED
   '-br    RESTORED
;
"Jetz kommen die Kamelen" TYPE
default-white    DECIMAL
