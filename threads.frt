\               clone1.frt : an example of clonding in Forth
\  $Id$
\  Copyright (2002): Albert van der Horst by GNU Public License

VARIABLE APIE

HEX
\ Originally returnstacksize, doing double duty as tib size.
\ Now it is more transient area size, all areas where a new task
\ needs a duplicate of : returnstack , tib data stack, user area .
10000 CONSTANT RTS
40 CELLS CONSTANT US
78 CONSTANT sys_clone
\ 2  CONSTANT sys_clone
100 CONSTANT CLONE_VM
12  CONSTANT SIGCHLD


: DUMP-IT
    "S0  "  TYPE S0  DUP H. SPACE @ H. CR
    "TIB "  TYPE TIB DUP H. SPACE @ H. CR
    "R0  "  TYPE R0  DUP H. SPACE @ H. CR
    "U0  "  TYPE U0  DUP H. SPACE @ H. CR CR
;

\ Clone the stack frame to ``RTS'' lower in memory.
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

\ Clone the stack frame to ``RTS'' lower in memory.
\ This area extends up to ``R0 @ US + '' .
: CVA
    DSP@   DUP RTS -   U0 @ DSP@ - US +   MOVE
    DSP@ RTS - DSP!    RSP@ RTS - RSP!
    4 -1 DO   RTS NEGATE U0 I CELLS +   +!   LOOP
;

\ Clone the current process, share memory and interrupts.
\ Leave PID (or zero for the clone)
\ It has been experimentally verified that :
\  - the first parameter matters
\  - the second parameter is indeed the stack of the child
\  - the third parameter matters not
: CLONE SIGCHLD CLONE_VM OR DSP@ 400 - _ sys_clone LINOS DUP 0< IF THROW THEN ;

\ Keep item ONE and TWO on the data stack while
\ switching the stack frame to new return stack POINTER.
\ This also switches the user variables.
\ This code must be inlined, you could'nt switch the return stack like
\ that while it is engaged.
: SWITCH-VARIABLE-AREA RSP! >R S0 @ DSP! R> ;

\ Create: allocate a new stack frame and size dictionary SPACE
\ Does: use that space to run XT in a thread.
\ Note: if applicable allocate sufficient dictionary space to use
\ the area to format numbers (below ``PAD'') and / or ``PAD'' itself.
\ NOTE: NOTHING AFTER DOES> CAN BE HIGH LEVEL, BECAUSE THE RETURN STACK
\ CANNOT BE USED.
: THREAD CREATE 0 , RSP@ ,  CVA ALLOT
\ Split the interpreter pointer (program counter) over two execution paths
\ with separate data stacks, one original, one temporary.
\ Transport the 2 items on the stack via the return stack.
DOES> 0 OVER ! >R >R
SIGCHLD CLONE_VM OR DSP@ 400 - _ sys_clone LINOS DUP 0< IF THROW THEN
\ BFFF,FFFF DSP@ 400 - _ sys_clone LINOS DUP 0< IF THROW THEN
IF R> R> BEGIN DUP @ UNTIL 2DROP ELSE R> R>
\ Install return stack
DUP [ 0 CELL+ ] LITERAL + @ RSP! \ CELL+ is high level!
\ Install data stack trasnporting one item via return stack.
>R >R S0 @ DSP! R> R> -1 SWAP !
\ Run and stop
EXECUTE 0 _ _ 1 LINOS THEN ;
