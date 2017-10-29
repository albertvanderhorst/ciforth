\              An example of cloning in Forth
\  $Id$
\  Copyright (2002): Albert van der Horst by GNU Public License

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
    .S MOVE
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
: CLONE SIGCHLD CLONE_VM OR DSP@ 400 - _ sys_clone XOS DUP 0< IF THROW THEN ;

\ Keep item ONE and TWO on the data stack while
\ switching the stack frame to new return stack POINTER.
\ This also switches the user variables.
\ This code must be inlined, you could'nt switch the return stack like
\ that while it is engaged.
: SWITCH-VARIABLE-AREA RSP! >R S0 @ DSP! R> ;

\ Create: allocate a new stack frame and size dictionary SPACE
\ Note: if applicable allocate sufficient dictionary space to use
\ the area to format numbers (below ``PAD'') and / or ``PAD'' itself.
\ Does: use that space to run XT in a thread.
\ The body contains the pid of the linux process of the thread, if running.
\ NOTE: NOTHING AFTER DOES> CAN BE HIGH LEVEL, BECAUSE THE RETURN STACK
\ CANNOT BE USED.
: THREAD CREATE 0 , R0 @ ,  CVA ALLOT
\ Split the interpreter pointer (program counter) over two execution paths
\ with separate data stacks, one original, one temporary.
\ Transport the 2 items on the stack via the return stack.
DOES> 0 OVER ! >R >R
SIGCHLD CLONE_VM OR DSP@ 400 - _ sys_clone XOS DUP 0< IF THROW THEN
\ Don't use the return stack until child has been set up.
DUP IF RDROP R> BEGIN DUP @ UNTIL ! ELSE R> R>
\ Install return stack
DUP [ 0 CELL+ ] LITERAL + @ RSP! \ CELL+ is high level!
\ Install data stack transporting two items via return stack.
>R >R S0 @ DSP! R> R> -1 SWAP !
\ Run and stop
EXECUTE 0 _ _ 1 XOS THEN ;

\ Kill the thread via its DEA. Throw errors.
: KILL >BODY @ 9 _ 25 XOS ?ERRUR ;

\ Do a preemptive pause.
\ In fact wait on nothing with a 10^6 nanoseconds timeout.
: PAUSE 0 0 1000 0 DSP@ 8E XOS5 DROP ;

\ Example. From the transputer world.

\ Convert a CHARACTER to uppercase. Return IT.
: >UPC DUP &a &z 1+ WITHIN IF 20 XOR THEN ;

\ A channel. It is inactive (contains no character) if it is zero.
: CHANNEL CREATE 0 C, ;

\ Store a CHARACTER to a CHANNEL.
: CHANNEL-C! BEGIN PAUSE DUP C@ 0= UNTIL C! ;

\ Read from a CHANNEL. Return the CHARACTER.
: CHANNEL-C@ BEGIN PAUSE DUP C@ UNTIL DUP C@ 0 ROT C! ;

CHANNEL M>S  \ Communication channel from master to slave.
CHANNEL S>M  \ Communication channel from slave to master.

: do-master
BEGIN
    KEY DUP M>S CHANNEL-C! ^D <> WHILE
    S>M CHANNEL-C@ EMIT
REPEAT ;

: do-slave
BEGIN
    M>S CHANNEL-C@ DUP ^D <> WHILE
    ." received" DUP EMIT
    >UPC S>M CHANNEL-C!
REPEAT DROP ;

\ This thread need no ``PAD'' or other dictionary space.
0 THREAD SLAVE

\ Send hither and thither characters until an ``ASCII''
\ End Of Text (ETX or ^D) is pressed.
: do-it-pet   'do-slave SLAVE   do-master ;

\ -----------------------------
WANT BAG


100 BAG TASK-TABLE   TASK-TABLE !BAG
VARIABLE TASK-POINTER

\ Make first task current.
: SET-FIRST-TASK TASK-TABLE CELL+ TASK-POINTER ! ;

\ Add this task and make it current.
_ TASK-TABLE BAG+!     SET-FIRST-TASK

\ Switch to next task, only administration.
: NEXT-TASK   TASK-POINTER @ CELL+ TASK-POINTER !
    TASK-POINTER @ TASK-TABLE @ = IF SET-FIRST-TASK THEN ;

\ Switch from current task to next one.
: PAUSE
    DSP@ >R RSP@ TASK-POINTER @ !
    NEXT-TASK
    TASK-POINTER @  @ RSP! R> DSP! ;

\ Exit: remove current task, then chain to first one.
: EXIT-COT  TASK-POINTER @ TASK-TABLE BAG-REMOVE
    SET-FIRST-TASK
    TASK-POINTER @ @ RSP! R> DSP! ;

\ Contrary to PET threads this one prepares a return stack frame,
\ then switches return stacks.
: THREAD-COT CREATE R0 @ 3 CELLS - , S0 @ , CVA ALLOT DOES>
>R
R@ CELL+ @   R@ @   !
>DFA @       R@ @ CELL+   !
'EXIT-COT >DFA @   R@ @ CELL+ CELL+ !
R> @ TASK-TABLE BAG+! ;

0 THREAD-COT SLAVE

\ Send hither and thither characters until an ``ASCII''
\ End Of Text (ETX or ^D) is pressed.
: do-it-cot   'do-slave SLAVE   do-master ;
