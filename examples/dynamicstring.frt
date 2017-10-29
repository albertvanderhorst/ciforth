\ $Id$
\ Copyright (2014): Albert van der Horst {by GNU Public License}

WANT ALLOCATE
" FORGET -dynamicstrings- " 'EVALUATE CATCH DROP
DATA -dynamicstrings-

\ Move STRING to heap, leave STRING
: $>h   DUP CELL+ ALLOCATE THROW DUP >R $! R> $@ ;

!CSP
"AAP" $>h ." Expect AAP : " TYPE  CR
?CSP
\ Move STRING to heap, leave STRINGADDRESS
: $>ha  DUP CELL+ ALLOCATE THROW DUP >R $! R> ;
"AAP" $>ha ." Expect AAP : " $@ TYPE CR
?CSP

\ Append STRING to STRINGADDRESS on heap, leave
\ STRINGADDRESS.
: $+>ha   OVER OVER @ + CELL+ RESIZE THROW DUP >R $+! R> ;

"MONEKY" $>ha "NOOT " ROT $+>ha ." Expect MONEKYNOOT : " $@ TYPE CR
?CSP
\ Leave the magic CELL that is in front of files got with ``GET-FILE''
: MAGIC  "FiLe" DROP @ ;
MAGIC DSP@ 4 ." Expect FiLe : " TYPE DROP CR
?CSP
\ For testing: an ISO word, that might not be present.
: ALIGN  1- 1 CELLS 1- OR 1+ ;

." Expect 24 : " 23 ALIGN . CR
." Expect 24 : " 24 ALIGN . CR
?CSP

\ For STRING or FILECONTENT recover the space that was
\ allocated in memory.
\ or a file.
: _recover DROP DUP 1 CELLS - @ MAGIC =
   IF -1 ELSE -2 THEN CELLS + DP ! ;

" Expect TRUE -1 : "
HERE
"AAP" _recover
ROT ROT TYPE  HERE = .
?CSP

CURRENT @   'ONLY >WID CURRENT !
    \ The same as " , but strings are allocated.
    : "   POSTPONE " STATE @ 0= IF 2DUP $>h 2SWAP _recover THEN ;
    PREFIX IMMEDIATE

CURRENT !
!CSP    \ : also uses !CSP

DATA JAN _ , _ ,
" Expect TRUE -1 : "
HERE
"AAP"   JAN 2!
ROT ROT TYPE HERE = .
." Expect AAP : " JAN 2@ TYPE CR

?CSP
: MIES   "AAP" TYPE ;
." Expect AAP : " MIES CR
?CSP

: GET-FILE   GET-FILE 2DUP $>h 2SWAP _recover ;
"howto.txt" GET-FILE

." EXPECT a lot " DROP 100 TYPE CR
?CSP

DATA JAN
"AAP" , ,
"NOOT" , ,
"MIES" , ,

: JAN[] 2 * CELLS JAN + ;

0 JAN[] 2@ TYPE CR
1 JAN[] 2@ TYPE CR
2 JAN[] 2@ TYPE CR
