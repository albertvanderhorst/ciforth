\               Formula music translation program.
\ $Id$}
\ Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License}

1 LOAD
REQUIRE ASSEMBLERi86
REQUIRE NEW-IF
REQUIRE $.

\ :  ?TEST 1+ 0 DO 0 WORD LOOP ;
: ?TEST DROP ;


: D= DNEGATE D+ OR 0= ;
1 ?TEST
CR ." EXPECT 1 1 0 123 |"  123 -1. -1. D= . 0. 0. D= . 2. 4. D= . .

\ worddoc(  {STRING},{$?},{string_query},{s --- },{},
\ {Output forthdfn({string variable})
\ forthvar({s}) to the terminal.},
\ {{STRING},{$.},{TYPE}})
: $? $@ TYPE ;

\ worddoc( {VECTORS},{VECTOR},{},{---},{},
\ { A define word that can be used to create an execution vector
\ forthvar({V}) . forthvar({V}) initially does nothing. After
\ forthsample({VECTOR V 'O W CFAO 'O V !}) forthvar({V}) has the execution
\ behaviour of forthvar({W}) . },
\ {{}})
: VECTOR [COMPILE] : POSTPONE NOOP [COMPILE] ; ;
: IS  >DFA SWAP >DFA @ SWAP ! ;

5 ?TEST
VECTOR Q    : JAN 1 2 3 ;   'JAN 'Q   IS
CR ." EXPECT 3 2 1 :"  Q . . .
FORGET Q

\ worddoc( {TIME},{TICKS},{ticks},{--- d},{},
\ {Return the number of clock cycles since the
\ computer was started up. This works only on
\ Pentium machines.},
\ {{}})
HEX
CODE TICKS  0F C, 31 C, 50 C, 52 C, NEXT C;

1 ?TEST
CR ." TICKS EXPECT 0 1 :" TICKS DNEGATE TICKS D+ . -1 U< .

DECIMAL

\ worddoc( {FORMULA},{TICKS/SEC},{ticks_per_second},{ --- d},{},
\ {Leave the number of ticks per sec, i.e. for a Pentium 90
\ this is 90,000,000. This is a configuration item, but it
\ should be made automatic.
\ },
\ {{TICKS}})
90,000,000 CONSTANT  TICKS/SEC

\ worddoc(  {EVENT},{PASSED},{passed},{d ---},{},
\ {The time forthvar({d}) indicating a tick count, is in the past.
\ },
\ {{TICKS}})
: PASSED DNEGATE TICKS D+ SWAP DROP 0< 0= ;

\ worddoc(  {EVENT},{EARLIER},{earlier},{d1 d2 --- f},{},
\ {The time forthvar({d1}) indicating a tick count, is earlier
\  or at the same time than the time forthvar({d2}) .
\ },
\ {{TICKS},{PASSED}})
: EARLIER  DNEGATE D+ -1. D+ SWAP DROP ;

2 ?TEST
CR ." TICKS EXPECT -1 :" TICKS TICKS EARLIER .
CR ." TICKS EXPECT 0 :" TICKS TICKS ROT >R ROT R> EARLIER .
CR ." TICKS EXPECT -1 -1 :" 0. 0. EARLIER . 1. 1. EARLIER .
\ Wrap around, will happen in 64,000 years
CR ." TICKS EXPECT -1 -1 :" -1. 1. EARLIER . -1. -1. EARLIER .
CR ." TICKS EXPECT 0 :" 1. -1. EARLIER .

\ worddoc( {EVENT},{MAXEVENT},{maxevent},{ --- n},{},
\ {Leave a constant , the number of events that can be scheduled in
\ the event table.
\ },
\ {{EVENT-TABLE}})
100,000 CONSTANT  MAXEVENT

\ worddoc( {MEMORY},{ARRAY},{array},{ n1 n2 --- },{},
\ {A defining word that generates an array with a lenght forthvar({n1})
\ and a stride (element size) of forthvar({n2}) .
\ Execution semantics of the defined word is forthsample({ n --- addr})
\ where forthcode({n}) is the index and forthvar({addr}) is
\ the place where array item forthvar({n}) is to be
\ },
\ {{TICK}})
: ARRAY CREATE DUP , * ALLOT DOES> >R R@ @ * R> + CELL+ ;

\ worddoc( {EVENT},{EVENT[]},{event_schedule},{ n --- addr},{},
\ {Defined by forthcode({ARRAY}) . forthvar({addr}) the place where event forthcode({n}) is to be
\ found. An event is a time in ticks. In a parallel array
\ forthcode({EVENT-DATA[]}) data applicable to this event may be stored.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{TICK}})
MAXEVENT 8 ARRAY EVENT[]

1 ?TEST
." EVENT[] EXPECT 123 :" 123  123. 1 EVENT[] 2! .

\ worddoc( {EVENT},{FILLED},{filled},{ --- addr},{},
\ {A variable indicating the last index of an
\ forthcode({EVENT[]}) that belongs to the filled part of the buffer.
\ The emptied nor the filled part can be empty.
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{EMPTIED}})
VARIABLE FILLED 1 FILLED !

\ worddoc( {EVENT},{EMPTIED},{emptied},{ --- addr},{},
\ {A variable indicating the last index of an
\ forthcode({EVENT[]}) that belongs to the emptied part of the buffer.
\ The emptied nor the filled part can be empty.
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{FILLED}})
VARIABLE EMPTIED 0 EMPTIED !

\ worddoc( {EVENT},{!EVENT},{init_event},{ --- },{},
\ {Initialise the event table.},
\ {{EVENT},{EMPTIED},{FILLED}})
: !EVENT 0 EMPTIED !  0 FILLED ! ;

\ worddoc( {EVENT},{#EMPTY},{amount_empty},{ --- n},{},
\ {Gives the lenght of the emptied part of the event table.
\ this part contains at least one spare, such that if the two
\ pointers are equal it means the table is totally empty.},
\ {{EVENT[]},{EMPTIED},{FILLED}})
: #EMPTY   EMPTIED @   FILLED @ -
    1 OVER > IF MAXEVENT + THEN ;

\ worddoc( {EVENT},{#FULL},{amount_full},{ --- n},{},
\ {Gives the lenght of the filled part of the event table.
\ },
\ {{EVENT[]},{EMPTIED},{FILLED}})
: #FULL   FILLED @   EMPTIED @ -
    DUP 0< IF MAXEVENT + THEN ;

1 ?TEST       !EVENT
." EXPECT 0 " MAXEVENT . ." 123 :" 123 #FULL . #EMPTY . .

\ worddoc( {EVENT},{FIND-EVENT},{find_event},{ d --- n},{},
\ {Find the place where forthvar({d}) is to be put into the event table,
\ such that it remains sorted.},
\ {{EVENT[]},{INSERT-EVENT},{GET-EVENT},{SET-EVENT}})
: FIND-EVENT FILLED @ EMPTIED @ >R >R
    BEGIN
        R> R> 2DUP >R >R >
        ( This IF prevents testing a non-existing table entry)
        IF 2DUP R@ 1 - EVENT[] 2@ EARLIER ELSE 0 THEN
    WHILE  R> 1 - >R
    REPEAT 2DROP R> R> DROP ;

6 ?TEST
3 FILLED !   12. 0 EVENT[] 2!   14. 1 EVENT[] 2!  16. 2 EVENT[] 2!
: F FIND-EVENT ;
123
." EXPECT 0 0 1 1 :" 11. F . 12. F . 13. F . 14. F .
." EXPECT 2 2 3 123 :" 15. F . 16. F . 17. F . .
FORGET F

\ worddoc( {EVENT},{SHIFT-UP-EVENT},{shift_up_event},{ n ---},{},
\ {Shift the event table open at place forthcode({n}) .},
\ {{EVENT[]},{FIND-EVENT},{GET-EVENT},{SET-EVENT}},{INSERT-EVENT}})
: SHIFT-UP-EVENT
      1 FILLED +!
      FILLED @ 2 -
      2DUP 1+ < IF DO   ( Ungraceful zero looping)
          I EVENT[] 2@   I 1+ EVENT[] 2!
      -1 +LOOP ELSE 2DROP THEN
;

8 ?TEST
2 FILLED !   12. 0 EVENT[] 2!   14. 1 EVENT[] 2!  16. 2 EVENT[] 2!
17. 3 EVENT[] 2! 18. 4 EVENT[] 2!
: T EVENT[] 2@ D. ;
123    1 SHIFT-UP-EVENT
." EXPECT 3 12 14 14 17 123 :" FILLED ? 0 T 1 T 2 T 3 T .
123    3 SHIFT-UP-EVENT
." EXPECT 4 14 14 17 18 123 :" FILLED ? 1 T 2 T 3 T 4 T .
FORGET T

\ worddoc( {EVENT},{INSERT-EVENT},{insert_event},{ d n ---},{},
\ {Insert the event forthvar({d}) into the event table,
\ that is known to have space,
\ at the place forthcode({n}) that is known to keep things sorted .},
\ {{EVENT[]},{FIND-EVENT},{GET-EVENT},{SET-EVENT}})
: INSERT-EVENT
    DUP SHIFT-UP-EVENT
    EVENT[] 2!
;

9 ?TEST
!EVENT
: T EVENT[] 2@ D. ;
123
 12. 0 INSERT-EVENT 14. 1 INSERT-EVENT  16. 2 INSERT-EVENT
." EXPECT 3 12 14 16 123 :" FILLED ? 0 T 1 T 2 T .
123
17. 3 INSERT-EVENT 18. 4 INSERT-EVENT
." EXPECT 5 14 16 17 18 123 :" FILLED ? 1 T 2 T 3 T 4 T .
FORGET T


\ worddoc( {EVENT},{SET-EVENT},{set_event},{ d --- n},{},
\ {Put an event forthvar({d}) into the event table.
\ This may fail, if the event table is full.
\ forthvar({n}) is the position where it was put,
\ and -1 means ``operation has failed''.},
\ {{EVENT[]},{EMPTIED},{FILLED}})
: SET-EVENT
    #EMPTY 1 = IF 2DROP -1 ELSE
        2DUP FIND-EVENT >R
        2DUP R@ EVENT[] 2@ D= IF
            2DROP R> \ Event already present
        ELSE
            R@ INSERT-EVENT R>
        THEN
    THEN ;

\ worddoc( {EVENT},{GET-EVENT},{get_event},{ --- n},{},
\ {Attempt to deschedule the first event from the event
\ table. The event is descheduled if the current time is
\ later, the number of the event is returned into
\ forthvar({n})
\ and -1 means ``no event is due''.},
\ {{EVENT[]},{EMPTIED},{FILLED}})
: GET-EVENT #FULL 0= IF
        -1
    ELSE EMPTIED @ EVENT[] 2@ PASSED IF
       EMPTIED @   1 EMPTIED +!
    ELSE
       -1
    THEN THEN  ;

3 ?TEST
!EVENT 123
." EXPECT 0 1 1 123 :" 123  10. SET-EVENT . 30. SET-EVENT . 20. SET-EVENT . .
." EXPECT 0 1 2 -1 :"   GET-EVENT . GET-EVENT . GET-EVENT . GET-EVENT .
." EXPECT 3 3 -1 :" 40. SET-EVENT . GET-EVENT . GET-EVENT .
." EXPECT 20 123 :" 1 EVENT[] 2@ D. .

.S
\ worddoc({TIME},{NS},{@},{n ---},{},
\ {Wait forthvar({n}) nanoseconds.
\ This is a busy wait
\ _VERBOSE({, it blocks the system and should typically
\ be used for delays of a microsecond or so}).
\ },
\ {{}})
: NS TICKS ROT
    TICKS/SEC 1,000,000,000 */ S>D D+
    BEGIN 2DUP PASSED UNTIL 2DROP ;

." NS EXPECT 1 [ delay of approximately 1 second ] 2 :" 2 1 . 1,000,000,000 NS .

\ worddoc( {FORMULA},{GOLD-PORT},{},{ --- port},{},
\ {Leave the forthvar({addr}) of printer port to be used by the
\ gold tingel tangel. This is a configuration item, and it
\ cannot be made automatic.
\ },
\ {{TICKS}})
\ HEX 0 408 L@ CONSTANT GOLD-PORT
HEX 378 CONSTANT GOLD-PORT
." EXPECT 378 :" GOLD-PORT HEX .
." EXPECT 0 :" GOLD-PORT 1 1 65 LINOS DUP .
DUP "You have no permission to write to the port. Become root"
ROT 0= 0= AND TYPE THROW
DECIMAL

13 ?TEST
0 GOLD-PORT PC!
CR ." Do not yet supply power to the gold tingel!"
CR ." Connect the gold tingel to the parallel port."
CR ." No LEDs should be visible now."
CR ." The green LED (data) should light up for 1 sec"
CR ." Press a key"   KEY   DROP
1 GOLD-PORT PC!   1,000,000,000 NS   0 GOLD-PORT PC!
CR ." The yellow LED (clock) should light up for 1 sec"
CR ." Press a key"   KEY   DROP
2 GOLD-PORT PC!   1,000,000,000 NS   0 GOLD-PORT PC!
CR ." The red LED (strobe) should light up for 1 sec"
CR ." Press a key"   KEY   DROP
4 GOLD-PORT PC!   1,000,000,000 NS   0 GOLD-PORT PC!
CR ." IF IT FAILED, REBOOT AND PRESS THE ICON NEW HARDWARE"
CR ." IF IT PERSISTS, REINSTALL WINDOWS MILLENIUM"
CR ." Press a key"   KEY   DROP

\ worddoc( {GOLD},{GOLD-LATCH},{},{ --- port},{},
\ {Leave the forthvar({addr}) of a variable that latches the
\ output to the forthcode({GOLD-PORT}). _VERBOSE_({ It remembers the
\ bits set in the port, that we don't know, but don't want to change})
\ },
\ {{}})
VARIABLE GOLD-LATCH 0 GOLD-LATCH !

\ worddoc( {GOLD},{GOLD-CLOCK},{},{f --- },{},
\ {Shift one bit of data into the forthcode({GOLD-PORT}) .
\ 0 if forthvar({f}) contains 0 else 1.
\ data sits in
\ bit 1 and is clocked on the rising edge of bit 2 },
\ {{}})
: GOLD-CLOCK
    DUP
        IF 5 ELSE 4 THEN GOLD-PORT PC!    1000 NS
        IF 7 ELSE 6 THEN GOLD-PORT PC!    1000 NS
;
: CLEAR-GOLD 40 0 DO 0 GOLD-CLOCK  LOOP ;

7 ?TEST
CR ." Supply power to the gold tingel. "
CR ." Expect all relais to release and then attract one by one"
CR ." Press a key"   KEY   DROP

CLEAR-GOLD
1 GOLD-CLOCK
40 0 DO 100,000,000 NS 0 GOLD-CLOCK LOOP


\ worddoc( {GOLD},{SET-GOLD-BYTE},{},{c -- },{},
\ {Transfer 8  bit of data from forthvar({c})
\ to the gold tingel, starting with the l.s. bit.
\ This is an auxiliary word for forthcode({SET-GOLD}). },
\ {{GOLD-PORT}})
: SET-GOLD-BYTE    8 0 DO DUP 1 AND GOLD-CLOCK 2 / LOOP DROP ;
CR ." EXPECT THE 4 ODD TINGELS ON THE LOW SIDE"
CR ." Press a key"   KEY   DROP
HEX 55 SET-GOLD-BYTE
CR ." EXPECT THE 4 EVEN TINGELS ON THE LOW SIDE"
CR ." Press a key"   KEY   DROP
HEX AA SET-GOLD-BYTE
CR ." Press a key"   KEY   DROP
CLEAR-GOLD

\ worddoc( {GOLD},{SET-GOLD},{},{addr -- },{},
\ {Transfer 40 bit of data from address forthvar({addr})
\ to the gold tingel, starting with the l.s. bit of the l.s.
\ byte. The last bit shifted agrees with musical note C2. },
\ {{GOLD-PORT}})
: SET-GOLD   5 OVER + SWAP DO I C@ SET-GOLD-BYTE LOOP ;

CR ." EXPECT THE LOWEST C (C2) "
CR ." Press a key"   KEY   DROP
PAD 5 ERASE  HEX 10   PAD 4 + !   DECIMAL
PAD SET-GOLD 70,000,000 NS CLEAR-GOLD
