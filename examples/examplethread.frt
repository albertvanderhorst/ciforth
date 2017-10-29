\  Copyright (2002): Albert van der Horst by GNU Public License
\  $Id$

\ Example. From the transputer world.
\ Have a character converted to upper case by an other thread.

\ Cooperative multitasking:
WANT THREAD-COT
: THREAD THREAD-COT ;
: PAUSE PAUSE-COT ;

\ Works equally well:
\ Pre-emptive multi-tasking
\ WANT THREAD-PET
\ : THREAD THREAD-PET ;
\ : PAUSE PAUSE-PET ;

\ Convert a CHARACTER to uppercase. Return IT.
HEX
: >UPC DUP &a &z 1+ WITHIN IF 20 XOR THEN ;
DECIMAL

\ A channel. It is inactive (contains no character) if it is zero.
: CHANNEL CREATE 0 C, ;

\ Initialise channel to free.
: !CHANNEL   0 SWAP C! ;

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
REPEAT PAUSE ;

: do-slave
BEGIN
    M>S CHANNEL-C@ DUP ^D <> WHILE
    >UPC S>M CHANNEL-C!
REPEAT DROP ;

\ This thread need no ``PAD'' or other dictionary space.
0 THREAD SLAVE

\ Send hither and thither characters until an ``ASCII''
\ End Of Text (ETX or ^D) is pressed.
: do-it   M>S !CHANNEL   S>M !CHANNEL   'do-slave SLAVE   do-master ;

\ -----------------------------
