\               Formula music translation program.
\ $Id$}
\ Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License}

: ?TEST DROP ;

\ worddoc( {ISO},{CHAR},{--- c},{ISO},
\ {Parse a word and 
\ leave forthvar({c}) the first non blank char of that word 
\ input stream.},
\ {{}})
: CHAR BL WORD HERE 1+ C@ ;

: CHARS ;

1 ?TEST
CR ." EXPECT |AB| : |" CHAR A EMIT CHAR B EMIT CHAR | EMIT 

\ worddocchapter( {STRING},{ },{},{},
\ { The idea of strings is that a character string (s) is a
\ forthdef({string variable}). This is in fact a counted string (sc) a
\ forthdfn({string constant}) (sc) that has been stored. s (c-addr) is
\ the string, sc (c-addr u) is a forthdfn({constant string}).
\ A string variable has first the number of characters in a
\ forthemph({cell}), than as many characters. Than probably spare room.}, 
\ {{$@},{@!}},{$@},{$!},{$C+},{$+!},{$I},{$S}})

\ worddoc( {STRING},{$@},{string_fetch},{ s --- sc },{},
\ {Fetch a constant string forthvar({sc}) from forthvar({s}).},
\ {{STRING},{$+!},{$F},{$S}})
: $@   DUP CELL+ SWAP @ ;

\ worddoc( {STRING},{$!},{string_store},{ sc s --- },{},
\ {Store a constant string forthvar({sc}) into forthvar({s}).},
\ {{STRING},{$@},{$+!},{$C+}})
: $!   OVER OVER ! CELL+ SWAP CMOVE ;

\ worddoc( {STRING},{$+!},{string_append},{ sc s --- },{},
\ { Append a constant string forthvar({sc}) to forthvar({s}).},
\ {{STRING},{$+}})
: $+! 
   DUP @ >R    ( remember old count )
   2DUP +!
   CELL+   R> CHARS +   SWAP CMOVE
;

\ worddoc( {STRING},{$C+},{string_char_append},{ c s --- },{},
\ {Append a character c to forthdfn({string variable}) s.},
\ {{STRING},{$+!}})
: $C+ ( char s -- )
  DUP >R
  DUP @ CHARS + CELL+ C!
  1 R> +!
;

\ worddoc( {STRING},{$.},{string_dot},{ sc --- },{},
\ {Output forthdfn({string constant}) forthvar({sc}) to the
\ terminal.}, 
\ {{STRING},{$?}})
: $. TYPE ;

\ worddoc( {STRING},{$?},{string_query},{s --- },{},
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

\ worddoc({STRING},{$I},{string_index},{ sc c - addr},{},
\ {Find forthvar({addr}), the index of forthvar({c}) in forthvar({sc}).
\ This is the first place forthvar({c}) is found in the string else 0.
\ It is assumed del cannot be a valid addr},
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


\ worddoc({VECTORS},{VECTOR},{vector},{---},{},
\ { A define word that can be used to create an execution vector
\ forthvar({V}) . forthvar({V}) initially does nothing. After
\ forthsample({VECTOR V ' W CFA ' V !}) forthvar({V}) has the execution
\ behaviour of forthvar({W}) . },
\ {{}})
: VECTOR [COMPILE] : COMPILE NOOP [COMPILE] ; ;

5 ?TEST
VECTOR Q    : JAN 1 2 3 ;   ' JAN CFA    ' Q   ! 
CR ." EXPECT 3 2 1 :"  Q . . . 
FORGET Q 

\ worddoc( {STRING},{COMPARE-AREA},{compare_area},{addr1 addr2 n--- f},{},{
\ Compare two memory area's with length forthvar({n}) and starting
\ at forthvar({addr1}) and forthvar({addr2}) .
\ Return forthvar({f}) the difference between
\ the first two nonequal char's or 0 for all equal.
\ },{{}})
VARIABLE EQUALITY
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
S" AAP" S" ACP" ROT DROP COMPARE-AREA 
CR ." EXPECT -2 :" .


\ worddoc( {STRING},{$=},{string_equal},{sc1 sc2 --- f},{},{
\ Return forthvar({f}) indicating that the forthdfn({string constant})s
\ forthvar({sc1}) and forthvar({sc2}) are equal. },
\ {{}})
: $= ROT OVER = 0= IF
        DROP DROP DROP 0  
    ELSE 
        COMPARE-AREA 0= 
    THEN ;

3 ?TEST
S" AAP" S" AAP" $=     CR ." EXPECT 0:" .
S" CAP" S" AAP" $=     CR ." EXPECT 0:" .
S" AAP" S" AAPA" $=    CR ." EXPECT 0 :" .

\ worddoc({TIME},{TICKS},{ticks},{--- d},{},
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

\ worddoc({FORMULA},{TICKS/SEC},{ticks_per_second},{ --- d},{},
\ {Leave the number of ticks per sec, i.e. for a Pentium 90 
\ this is 90,000,000. This is a configuration item, but it 
\ should be made automatic.
\ },
\ {{}})
\ {{TICKS}})
90000000 CONSTANT  TICKS/SEC

\ worddoc( {EVENT},{PASSED},{passed},{d ---},{},
\ {The time forthvar({d}) indicating a tick count, is in the past.
\ },
\ {{TICKS}})
: PASSED DMINUS TICKS D+ SWAP DROP 0< 0= ;

\ worddoc( {EVENT},{EARLIER},{earlier},{d1 d2 ---},{},
\ {The time forthvar({d1}) indicating a tick count, is earlier
\  then the time forthvar({d2}) .
\ },
\ {{TICKS},{PASSED}})
: EARLIER  DMINUS D+ SWAP DROP ;

2 ?TEST
." EXPECT -1 :" TICKS TICKS EARLIER . 
." EXPECT 0 :" TICKS TICKS ROT >R ROT R> EARLIER .

\ worddoc({EVENT},{MAXEVENT},{maxevent},{ --- n},{},
\ {Leave a constant , the number of events that can be scheduled in 
\ the event table.
\ },
\ {{EVENT-TABLE}})
100000 CONSTANT  MAXEVENT

\ worddoc({MEMORY},{ARRAY},{array},{ n1 n2 --- },{},
\ {A defining word that generates an array with a lenght forthvar({n1}) 
\ and a stride (element size) of forthvar({n2}) .
\ Execution semantics of the defined word is forthsample({ n --- addr})
\ where forthcode({n}) is the index and forthvar({addr}) is 
\ the place where array item forthvar({n}) is to be 
\ },
\ {{TICK}})
: ARRAY <BUILDS DUP , * ALLOT DOES> >R R @ * R> + CELL+ ;

\ worddoc({EVENT},{EVENT[]},{event_schedule},{ n --- addr},{},
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


\ worddoc({EVENT},{FILLED},{filled},{ --- addr},{},
\ {A variable indicating the last index of an 
\ forthcode({EVENT[]}) that belongs to the filled part of the buffer.
\ The emptied nor the filled part can be empty. 
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{EMPTIED}})
1 VARIABLE FILLED

\ worddoc({EVENT},{EMPTIED},{emptied},{ --- addr},{},
\ {A variable indicating the last index of an 
\ forthcode({EVENT[]}) that belongs to the emptied part of the buffer.
\ The emptied nor the filled part can be empty. 
\ For this reason the last event of the filled part is not used.
\ In principle this is a circular buffer, but it may not be used like
\ it yet.
\ },
\ {{EVENT[]},{FILLED}})
0 VARIABLE EMPTIED

\ worddoc({EVENT},{!EVENT},{init_event},{ --- },{},
\ {Initialise the event table.},
\ {{EVENT},{EMPTIED},{FILLED}})
: !EVENT 0 EMPTIED !  1 FILLED ! ;

\ worddoc({EVENT},{#EMPTY},{amount_empty},{ --- n},{},
\ {Gives the lenght of the emptied part of the event table.}
\ {{EVENT[]},{EMPTIED},{FILLED}})
: #EMPTY   EMPTIED @   FILLED @ -  
    DUP 0< IF MAXEVENT + THEN ;

\ worddoc({EVENT},{#FULL},{amount_full},{ --- n},{},
\ {Gives the lenght of the filled part of the event table,
\ this part must not be empty, such that if the two
\ pointers are equal the table is totally full.
\ },
\ {{EVENT[]},{EMPTIED},{FILLED}})
: #FULL   FILLED @   EMPTIED @ -  
    DUP 0< IF MAXEVENT + THEN ;

1 ?TEST
." EXPECT 1 " MAXEVENT 1 - . ." :" 123 #FULL . #EMPTY . 

: FIND-EVENT DROP DROP 1 ;
: INSERT-EVENT DROP DROP DROP 1 FILLED +! ;
\ worddoc({EVENT},{SET-EVENT},{set_event},{ d --- n},{},
\ {Put an event forthvar({d}) into the event table.
\ This may fail, if the event table is full.
\ forthvar({n}) is the position where it was put,
\ and -1 means ``operation has failed''.},
\ {{EVENT[]},{EMPTIED},{FILLED}})
: SET-EVENT #EMPTY IF 
    2DUP FIND-EVENT >R
    R INSERT-EVENT
    R> ELSE -1 THEN ;

\ worddoc({EVENT},{GET-EVENT},{get_event},{ --- n},{},
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
." EXPECT 1 1 1 123 :" 123  10. SET-EVENT . 20. SET-EVENT . 30. SET-EVENT . .
0. 0 EVENT[] 2!         0. 1 EVENT[] 2!         0. 2 EVENT[] 2!
." EXPECT 0 1 2 -1 :"   GET-EVENT . GET-EVENT . GET-EVENT . GET-EVENT .



