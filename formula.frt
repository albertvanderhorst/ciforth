\               Formula music translation program.
\ $Id$}
\ Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License}

\ :  ?TEST 1+ 0 DO 0 WORD LOOP ;
: ?TEST DROP ;

\ worddoc(  {ISO},{CHAR},{},{--- c},{ISO},
\ {Parse a word and 
\ leave forthvar({c}) the first non blank char of that word 
\ input stream.},
\ {{}})
: CHAR BL WORD HERE 1+ C@ ;

1 ?TEST
CR ." EXPECT |AB| : |" CHAR A EMIT CHAR B EMIT CHAR | EMIT 

: CHARS ;
: 2DROP DROP DROP ;
: 2SWAP ROT >R ROT R> ;

: CHARS ;
1 ?TEST
CR ." EXPECT 123 124 : |" 124 123 CHARS . . 

1 ?TEST
CR ." EXPECT 1 2 : " 1 2 3 4 2DROP . . 

: 2SWAP ROT >R ROT R> ;
1 ?TEST
CR ." EXPECT 1 2 3 4  : |" 2 1 4 3 2SWAP . . . . 

: D= DMINUS D+ OR 0= ;
1 ?TEST
CR ." EXPECT 1 1 0 123 |"  123 -1. -1. D= . 0. 0. D= . 2. 4. D= . .

\ worddocchapter( {STRING},{ },{},{},
\ { The idea of strings is that a character string (s) is a
\ forthdef({string variable}). This is in fact a counted string (sc) a
\ forthdfn({string constant}) (sc) that has been stored. s (c-addr) is
\ the string, sc (c-addr u) is a forthdfn({constant string}).
\ A string variable has first the number of characters in a
\ forthemph({cell}), than as many characters. Than probably spare room.}, 
\ {{$@},{@!}},{$@},{$!},{$C+},{$+!},{$I},{$S}})

\ worddoc(  {STRING},{$@},{string_fetch},{ s --- sc },{},
\ {Fetch a constant string forthvar({sc}) from forthvar({s}).},
\ {{STRING},{$+!},{$F},{$S}})
: $@   DUP CELL+ SWAP @ ;

\ worddoc(  {STRING},{$!},{string_store},{ sc s --- },{},
\ {Store a constant string forthvar({sc}) into forthvar({s}).},
\ {{STRING},{$@},{$+!},{$C+}})
: $!   OVER OVER ! CELL+ SWAP CMOVE ;

\ worddoc(  {STRING},{$+!},{string_append},{ sc s --- },{},
\ { Append a constant string forthvar({sc}) to forthvar({s}).},
\ {{STRING},{$+}})
: $+! 
   DUP @ >R    ( remember old count )
   2DUP +!
   CELL+   R> CHARS +   SWAP CMOVE
;

\ worddoc(  {STRING},{$C+},{string_char_append},{ c s --- },{},
\ {Append a character c to forthdfn({string variable}) s.},
\ {{STRING},{$+!}})
: $C+ ( char s -- )
  DUP >R
  DUP @ CHARS + CELL+ C!
  1 R> +!
;

\ worddoc(  {STRING},{$.},{string_dot},{ sc --- },{},
\ {Output forthdfn({string constant}) forthvar({sc}) to the
\ terminal.}, 
\ {{STRING},{$?}})
: $. TYPE ;

\ worddoc(  {STRING},{$?},{string_query},{s --- },{},
\ {Output forthdfn({string variable}) 
\ forthvar({s}) to the terminal.},
\ {{STRING},{$.},{TYPE}})
: $? $@ TYPE ;

9 ?TEST
0 VARIABLE PAD 100 ALLOT
S" AAP" PAD $!
CR ." EXPECT |AAP| : |" PAD $? CHAR | EMIT
S" FRED" PAD $+!
CR ." EXPECT |AAPFRED| : |" PAD $? CHAR | EMIT
CHAR Q PAD $C+
CR ." EXPECT |AAPFREDQ| : |" PAD $? CHAR | EMIT
CR ." EXPECT |AAPFREDQ| : |" PAD $@ $. CHAR | EMIT
FORGET PAD

\ worddoc( {STRING},{$I},{string_index},{ sc c - addr},{},
\ {Find forthvar({addr}), the index of forthvar({c}) in forthvar({sc}).
\ This is the first place forthvar({c}) is found in the string else 0.},
\ {{}})
: $I 
OVER 0= IF DROP DROP DROP 0 ELSE  DUP >R
     ROT ROT OVER + SWAP DO
     DUP I C@ = IF DROP I LEAVE THEN
   LOOP R> OVER = IF DROP 0 THEN  ( Tricky)
THEN ;

2 ?TEST
CR ." EXPECT 2 : " S" AAPAA" OVER OVER CHAR P $I ROT - . DROP
CR ." EXPECT 0 : " S" AAPAA" CHAR C $I .


\ worddoc( {VECTORS},{VECTOR},{},{---},{},
\ { A define word that can be used to create an execution vector
\ forthvar({V}) . forthvar({V}) initially does nothing. After
\ forthsample({VECTOR V 'O W CFAO 'O V !}) forthvar({V}) has the execution
\ behaviour of forthvar({W}) . },
\ {{}})
: VECTOR [COMPILE] : COMPILE NOOP [COMPILE] ; ;

5 ?TEST
VECTOR Q    : JAN 1 2 3 ;   'O JAN CFAO    'O Q   ! 
CR ." EXPECT 3 2 1 :"  Q . . . 
FORGET Q 

\ worddoc(  {STRING},{COMPARE-AREA},{},{addr1 addr2 n--- f},{},{
\ Compare two memory area's with length forthvar({n}) and starting
\ at forthvar({addr1}) and forthvar({addr2}) .
\ Return forthvar({f}) the difference between
\ the first two nonequal char's or 0 for all equal.
\ },{{}})
0 VARIABLE EQUALITY
: COMPARE-AREA  
    0 DO   
        OVER I + C@   OVER I + C@  - 
            DUP EQUALITY ! 
        IF LEAVE THEN 
    LOOP 2DROP EQUALITY @ ;

6 ?TEST
S" AAP" S" AAP" ROT DROP COMPARE-AREA 
CR ." EXPECT 0 :"  .
S" CAP" S" AAP" ROT DROP COMPARE-AREA 
CR ." EXPECT 2 :"  .
S" ACP" S" AEP" ROT DROP COMPARE-AREA 
CR ." EXPECT -2 :" .


\ worddoc(  {STRING},{$=},{string_equal},{sc1 sc2 --- f},{},{
\ Return forthvar({f}) indicating that the forthdfn({string constant})s
\ forthvar({sc1}) and forthvar({sc2}) are equal. },
\ {{}})
: $= ROT OVER = 0= IF
        DROP DROP DROP 0  
    ELSE 
        COMPARE-AREA 0= 
    THEN ;

3 ?TEST
S" AAP" S" AAP" $=     CR ." EXPECT 1 :" .
S" CAP" S" AAP" $=     CR ." EXPECT 0 :" .
S" AAP" S" AAPA" $=    CR ." EXPECT 0 :" .

\ worddoc( {TIME},{TICKS},{ticks},{--- d},{},
\ {Return the number of clock cycles since the 
\ computer was started up. This works only on 
\ Pentium machines.},
\ {{}})
HEX
CODE TICKS 0F C, 31 C, ( 90 C, 90 C, 90 C, 90 C, )
    50 C, 52 C,  ( push AX & DX )
    AD C, 89 C, C3 C, FF C, 23 C,  ( next)
C;

1 ?TEST
." EXPECT 0 1 :" TICKS DMINUS TICKS D+ . 0FFFF U< . 

DECIMAL 

\ worddoc( {FORMULA},{TICKS/SEC},{ticks_per_second},{ --- d},{},
\ {Leave the number of ticks per sec, i.e. for a Pentium 90 
\ this is 90,000,000. This is a configuration item, but it 
\ should be made automatic.
\ },
\ {{TICKS}})
90000000 CONSTANT  TICKS/SEC

\ worddoc(  {EVENT},{PASSED},{passed},{d ---},{},
\ {The time forthvar({d}) indicating a tick count, is in the past.
\ },
\ {{TICKS}})
: PASSED DMINUS TICKS D+ SWAP DROP 0< 0= ;

\ worddoc(  {EVENT},{EARLIER},{earlier},{d1 d2 --- f},{},
\ {The time forthvar({d1}) indicating a tick count, is earlier
\  or at the same time than the time forthvar({d2}) .
\ },
\ {{TICKS},{PASSED}})
: EARLIER  DMINUS D+ -1. D+ SWAP DROP ;

2 ?TEST
." EXPECT -1 :" TICKS TICKS EARLIER . 
." EXPECT 0 :" TICKS TICKS ROT >R ROT R> EARLIER .
." EXPECT -1 -1 :" 0. 0. EARLIER . .1. 1. EARLIER . 
\ Wrap around, will happen in 64.000 years 
." EXPECT -1 -1 :" -1. 1. EARLIER . -1. -1. EARLIER . 
." EXPECT 0 :" 1. -1. EARLIER . 

\ worddoc( {EVENT},{MAXEVENT},{maxevent},{ --- n},{},
\ {Leave a constant , the number of events that can be scheduled in 
\ the event table.
\ },
\ {{EVENT-TABLE}})
100000 CONSTANT  MAXEVENT

\ worddoc( {MEMORY},{ARRAY},{array},{ n1 n2 --- },{},
\ {A defining word that generates an array with a lenght forthvar({n1}) 
\ and a stride (element size) of forthvar({n2}) .
\ Execution semantics of the defined word is forthsample({ n --- addr})
\ where forthcode({n}) is the index and forthvar({addr}) is 
\ the place where array item forthvar({n}) is to be 
\ },
\ {{TICK}})
: ARRAY <BUILDS DUP , * ALLOT DOES> >R R @ * R> + CELL+ ;

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
." EXPECT 123 :" 123  123. 1 EVENT[] 2! . 

\ worddoc( {EVENT},{FILLED},{filled},{ --- addr},{},
\ {A variable indicating the last index of an 
\ forthcode({EVENT[]}) that belongs to the filled part of the buffer.
\ The emptied nor the filled part can be empty. 
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{EMPTIED}})
1 VARIABLE FILLED

\ worddoc( {EVENT},{EMPTIED},{emptied},{ --- addr},{},
\ {A variable indicating the last index of an 
\ forthcode({EVENT[]}) that belongs to the emptied part of the buffer.
\ The emptied nor the filled part can be empty. 
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{FILLED}})
0 VARIABLE EMPTIED

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
        IF 2DUP R 1 - EVENT[] 2@ EARLIER ELSE 0 THEN
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
        2DUP R EVENT[] 2@ D= IF
            2DROP R> \ Event already present
        ELSE
            R INSERT-EVENT R> 
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

^
\ worddoc({TIME},{NS},{@},{n ---},{},
\ {Wait forthvar({n}) nanoseconds.
\ This is a busy wait
\ _VERBOSE({, it blocks the system and should typically
\ be used for delays of a microsecond or so}).
\ },
\ {{}})
: NS TICKS ROT
    TICKS/SEC 1000000000 */ S->D D+ 
    BEGIN 2DUP PASSED UNTIL 2DROP ;

." EXPECT 1 [ delay of approximately 1 second ] 2 :" 2 1 1000000000 NS . . 

\ worddoc( {FORMULA},{GOLD-PORT},{},{ --- port},{},
\ {Leave the forthvar({addr}) of printer port to be used by the
\ gold tingel tangel. This is a configuration item, and it 
\ cannot be made automatic.
\ },
\ {{TICKS}})
HEX 0 408 L@ CONSTANT GOLD-PORT
." EXPECT 378 :" GOLD-PORT HEX . DECIMAL

13 ?TEST
CR ." Do not yet supply power to the gold tingel!"
CR ." Connect the gold tingel to the parallel port."
CR ." The red LED should light up for 1 sec" 
CR ." Press a key"   KEY   DROP 
1 GOLD-PORT PC!   1000000000 NS   0 GOLD-PORT PC!   
CR ." The yellow LED should light up for 1 sec" 
CR ." Press a key"   KEY   DROP 
2 GOLD-PORT PC!   1000000000 NS   0 GOLD-PORT PC!   
CR ." Press a key"   KEY   DROP 
CR ." The green LED should light up for 1 sec" 
4 GOLD-PORT PC!   1000000000 NS   0 GOLD-PORT PC!   
CR ." IF IT FAILED, REBOOT AND PRESS THE ICON NEW HARDWARE"
CR ." IF IT PERSISTS, REINSTALL WINDOWS MILLENIUM"
CR ." Press a key"   KEY   DROP 

\ worddoc( {GOLD},{GOLD-LATCH},{},{ --- port},{},
\ {Leave the forthvar({addr}) of a variable that latches the 
\ output to the forthcode({GOLD-PORT}). _VERBOSE_({ It remembers the
\ bits set in the port, that we don't know, but don't want to change})
\ },
\ {{}})
0 VARIABLE GOLD-LATCH 

\ worddoc( {GOLD},{GOLD-CLOCK},{},{f --- },{},
\ {Shift one bit of data into the forthcode({GOLD-PORT}) .
\ 0 if forthvar({f}) contains 0 else 1.
\ data sits in
\ bit 1 and is clocked on the rising edge of bit 2 }, 
\ {{}})
: GOLD-CLOCK
    DUP IF 1 ELSE 0 THEN GOLD-PORT PC!    1000 NS
        IF 3 ELSE 2 THEN GOLD-PORT PC!    1000 NS
;

7 ?TEST
CR ." Supply power to the gold tingel. "
CR ." Expect all relais to release and then attract one by one"
CR ." Press a key"   KEY   DROP 
: DOIT 
    40 0 DO 0 GOLD-CLOCK 
    1 GOLD-CLOCK 
    40 0 DO 100000000 NS 0 GOLD-CLOCK LOOP ;
DOIT FORGET DOIT

\ worddoc( {GOLD},{SET-GOLD-BYTE},{},{c -- },{},
\ {Transfer 8  bit of data from forthvar({c}) 
\ to the gold tingel, starting with the l.s. bit.
\ This is an auxiliary word for forthcode({SET-GOLD}). }, 
\ {{GOLD-PORT}})
: SET-GOLD-BYTE    8 0 DO 1 AND GOLD-CLOCK 2 / LOOP ;

\ worddoc( {GOLD},{SET-GOLD},{},{addr -- },{},
\ {Transfer 40 bit of data from address forthvar({addr}) 
\ to the gold tingel, starting with the l.s. bit of the l.s.
\ byte. The last bit shifted agrees with musical note C2. }, 
\ {{GOLD-PORT}})
: SET-GOLD   5 OVER + SWAP DO I C@ SET-GOLD-BYTE LOOP ;

CR ." EXPECT THE LOWEST C (C2) "
PAD 5 ERASE  HEX 80   PAD 4 + !   PAD SET-GOLD
PAD 5 ERASE  70000000 NS          PAD SET-GOLD

