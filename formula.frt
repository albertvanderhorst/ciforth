\               Formula music translation program.
\ $Id$}
\ Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License}

: \ 0 WORD ;
: ?TEST DROP ;

\ worddoc( {ISO},{CHAR},{--- c},{ISO},
\ {Leave forthvar({c}) the first non blank char in the 
\ input stream.},
\ {{}})
: CHAR 
TIB @ IN @ +   
BEGIN DUP C@ BL = WHILE 1+ REPEAT 
DUP C@ SWAP
1+ TIB @ - IN ! 
;

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

