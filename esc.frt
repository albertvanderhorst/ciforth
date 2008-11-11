
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

\ Fix up the analyser with information about what are duplicators.

HEX  1B CONSTANT ESC DECIMAL

: esc-seq   CREATE $, DROP DOES> ESC EMIT $@ TYPE ;

"[?25l" esc-seq INVIS  \ Make cursor invisible
"[?25h" esc-seq CVVIS  \ Make cursor visible
"[H"    esc-seq HOME   \ Set cursor home
"[2J"   esc-seq CLEAR  \ Clear Page

\ ISO ``PAGE'' command
: PAGE   HOME CLEAR ;

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
