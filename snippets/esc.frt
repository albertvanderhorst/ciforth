
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ Fix up the analyser with information about what are duplicators.

WANT $-PREFIX

$1B CONSTANT ESC

: esc-seq   CREATE $, DROP DOES> ESC EMIT $@ TYPE ;

"[?25l" esc-seq INVIS  \ Make cursor invisible
"[?25h" esc-seq CVVIS  \ Make cursor visible
"[H"    esc-seq HOME   \ Set cursor home
"[2J"   esc-seq CLEAR  \ Clear Page
"[4h"   esc-seq enter_insert_mode
"[4l"   esc-seq exit_insert_mode
"[L"    esc-seq insert_line
"[M"    esc-seq delete_line
"[1A"   esc-seq scroll-up
"[1B"   esc-seq scroll-down
"[P"    esc-seq delete_character
"[C"    esc-seq cursor_right
"[D"    esc-seq cursor_left
"[A"    esc-seq cursor_up
"[B"    esc-seq cursor_down

\ ISO ``PAGE'' command
: PAGE   HOME CLEAR ;
: scroll-down  ^J EMIT ;

\ Print N but no space and decimal.
: .no BASE @ >R DECIMAL 0  <# #S #> TYPE R> BASE ! ;

\ ISO ``AT-XY'' command.
: AT-XY   ESC EMIT   &[ EMIT   1+ .no   &; EMIT   1+ .no   &H EMIT ;

: change_scroll_region   ESC EMIT   &[ EMIT   1+ .no   &; EMIT   1+ .no   &r EMIT ;

CREATE escape-color
ESC C, &[ C, HERE _ C, _ C, &; C, HERE _ C, _ C, &; C, &1 C, &m C,
CONSTANT escape-fore   CONSTANT escape-back

: send-color-escape escape-color 10 TYPE ;

: fore escape-fore SWAP CMOVE ;
: back escape-back SWAP CMOVE ;

: fore-color CREATE $, DROP DOES> $@ fore send-color-escape ;

: back-color CREATE $, DROP DOES> $@ back send-color-escape ;

: color-escape CREATE $, DROP DOES> ESC EMIT &[ EMIT $@ TYPE ;

\ Put the screen in a mode such as to print the chars in the color.
"37"    fore-color    white
"37;1m" color-ESCAPE  white2
"36"    fore-color    aqua
"32"    fore-color    green
"33"    fore-color    yellow
"31"    fore-color    red
"35"    fore-color    pink
"34"    fore-color    fblue
"30"    fore-color    f30

\ Put the screen in a mode such as the background color.
"40"    back-color    black
"41"    back-color    b41
"42"    back-color    b42
"43"    back-color    b43
"44"    back-color    blue
"45"    back-color    b45
"46"    back-color    b46
"47"    back-color    b47
"48"    back-color    b48
"49"    back-color    bwhite


\ Print text in white, not bold.
\ This is sufficient to overrule coloring.
"0m" color-ESCAPE default-white
\ Print text with foreground and background colors swapped.
"7m" color-ESCAPE reverse

VARIABLE I-MODE   0 I-MODE !
DECIMAL
80 CONSTANT width
20 CONSTANT mh    \ Mid height where lines are lost.
25 CONSTANT height
width height * CONSTANT size
CREATE e-buf  size ALLOT        \ Edit buffer
CREATE l-buf  width ALLOT       \ line buffer
\ For LINE return START in e-buf.
: >l    width * e-buf + ;
\ Save LINE to l-buf.
: save  >l l-buf width MOVE ;
\ Restore LINE from l-buf.
: restore  >l l-buf SWAP width MOVE ;
\ Rotate LINEH through LINEL up, within e-buf.
: roll^ DUP save 2DUP - width * >R >l DUP width + SWAP R> MOVE restore ;
\ Rotate LINEH through LINEL down, within e-buf.
: rollv OVER save 2DUP - width * >R DUP >l DUP width + R> MOVE restore DROP ;
\
: showl    0 OVER AT-XY  exit_insert_mode   >l width TYPE ;
\ : showl    0 OVER AT-XY  exit_insert_mode   . 20 SPACES &* EMIT ;
: BLK>V  SCR @ BLOCK 1024 TYPE ;
: blk>e  e-buf size BLANK
SCR @ BLOCK 16 0 DO DUP e-buf  I width * + C/L 1- MOVE C/L + LOOP DROP ;
: e>blk SCR @ BLOCK 16 0 DO e-buf  I width * + OVER C/L 1- MOVE C/L + LOOP DROP ;
VARIABLE cursor-x    VARIABLE cursor-y
\ Delete character at cursor position in the e-buffer.
: del-c   e-buf cursor-y @ width * + >R  R@ cursor-x @ + DUP 1+ SWAP
    R@ width + OVER - MOVE    BL R> width + 1- C! ;
: cur-l@ e-buf cursor-y @ width * + ;
\ Insert KEY at cursor position in the e-buffer.
: ins-c   cur-l@ >R  R@ cursor-x @ + DUP 1+
    R@ width + OVER - 1- MOVE    R> cursor-x @ + C! ;
\ Store KEY at cursor position in the e-buffer.
: sto-c   cur-l@ cursor-x @ + C! ;

: CP cursor-x @ ;
: SET-CURSOR   cursor-x @ cursor-y @  AT-XY ;
: clamp-x 0 MAX width 1- MIN cursor-x ;
: MOVE-X ( WORD STAR)
DUP ^D = IF  1 ELSE       DUP ^S = IF -1 ELSE
DUP ^I = IF  8 ELSE       DUP ^M = IF width CP - ELSE
\ DUP BL 127 WITHIN IF  1 ELSE
0    THEN THEN THEN THEN
cursor-x @ + clamp-x ! ;
: clamp-y 0 MAX height 1- MIN ;
: MOVE-Y ( WORD STAR)
DUP ^X = IF 1 ELSE
DUP ^H = IF -1 ELSE
DUP ^E = IF -1 ELSE
DUP ^C = IF 8 ELSE   DUP ^R = IF -8 ELSE
DUP ^M = IF 1 ELSE
0    THEN THEN THEN THEN THEN THEN
cursor-y @ + clamp-y cursor-y ! ;
: set-c  DUP BL 127 WITHIN IF
    I-MODE @ IF DUP ins-c ELSE DUP sto-c THEN THEN ;
: EM-C EMIT set-c 1 CURSOR-X +! ;
: PRINT ( C --C . Print it if printable)
  DUP BL $7F WITHIN IF   DUP EM-C   THEN ;
: update_insert I-MODE @ IF enter_insert_mode ELSE exit_insert_mode THEN ;
: toggle_insert   I-MODE 1 TOGGLE update_insert ;
: DELSTORING
    DUP ^Y = IF
    height 1- cursor-y @ roll^   delete_line
    0 height 1- AT-XY insert_line l-buf width 1- TYPE
ELSE
    DUP ^U = IF
    mh 1- cursor-y @ rollv
    0 cursor-y @ AT-XY insert_line height 1- >l width 1- TYPE
    0 mh AT-XY delete_line
ELSE
    DUP ^P = IF
    height 1- cursor-y @ rollv
    0 cursor-y @ AT-XY insert_line l-buf width 1- TYPE
    0 height AT-XY delete_line
ELSE
    THEN THEN THEN ;                                 DECIMAL
: INSELETING
      DUP ^H = IF delete_character  del-c ELSE
      DUP ^G = IF delete_character  del-c ELSE
      DUP ^V = IF toggle_insert ELSE
      THEN THEN THEN ;
: ROUTE BEGIN KEY
  PRINT
  DELSTORING
\ JOINITTING
\ WORDING
move-x
move-y
SET-CURSOR
 INSELETING
( DEBUG)
ESC = UNTIL ;
: E-S  ( EDIT CURRENT SCREEN )
    1 I-MODE ! update_insert
\    FRAME
    PAGE   BLK>V blk>e
    0 cursor-x !
    0 cursor-y !   SET-CURSOR
    ROUTE
\    EXITING

   e>blk
\    AT-END
\    NO-FRAME
   0 height AT-XY exit_insert_mode
;
:  EDIT SCR ! E-S ;
