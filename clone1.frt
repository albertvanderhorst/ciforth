HEX
\ Originally returnstacksize, doing double duty as tib size.
\ Now it is more transient area size, all areas where a new task
\ needs a duplicate of : returnstack , tib data stack, user area .
10000 CONSTANT RTS
40 CELLS CONSTANT US
: DUMP-IT
    "S0  "  TYPE S0  DUP H. SPACE @ H. CR
    "TIB "  TYPE TIB DUP H. SPACE @ H. CR
    "R0  "  TYPE R0  DUP H. SPACE @ H. CR
    "U0  "  TYPE U0  DUP H. SPACE @ H. CR CR
;

\ Clone the stack frame to ``RTS'' in memory.
\ This area extends up to ``R0 @ US + '' .
: CLONE-VARIABLE-AREA DUMP-IT
    DSP@
    DUP RTS -
    U0 @ DSP@ - US +
    ^ MOVE
    DSP@ RTS - DSP!
    DUMP-IT
    RSP@ RTS - RSP!
    DUMP-IT
    4 -1 DO   RTS NEGATE U0 I CELLS +   +!   LOOP
    DUMP-IT
;

: CLONE 78 LINOS ;
: FORK _ _ _ 2 LINOS ;

: hoezee
    8 0 DO  I . 400 MS  CR LOOP
    "hello" TYPE CR
    8 0 DO  I . 400 MS  CR LOOP
;

: SWAP-RETURN-STACK
\   DSP@ 1000 - DSP!
;

: doit
    RSP@ RTS + RSP!
    S0 @ DSP!
    DUMP-IT
    hoezee
    _ _ _ 1 LINOS DROP
;

: (clone)
\     >DFA @ >R
    ( DSP@ 400 - DSP!)
    _
    DSP@ 1000 -
    1FF
    78 LINOS
    DUP
    IF
    400 MS
\    _ _ _ 1 LINOS
\     RDROP
    DUMP-IT
    "RESULT:" TYPE . CR
    ELSE
    DUMP-IT
    "RESULT:" TYPE . CR
    doit
    THEN
\   1 CLOSE-FILE ABORT" ERROR CLSOING STDIN"
\   0 CLOSE-FILE ABORT" ERROR CLSOING STDOUT"
\   7000,0000 MS
;

\ The 1D (pause) waits for a signal.
: clone   'doit (clone) ;

: CVA   CLONE-VARIABLE-AREA ;
