#!./lina -s

\ Usage: sortworddoc.frt [-r] <file1> <file2>
\ file1 must contain worddoc records in a format
\  worddocxxx(field1a, field2a, ....)
\  worddocyyy(field1b, field2b, ....)
\ The records are sorted on field1, then on field2 and output to file2.
\ If -r is present the sorting is first on field2, then field1.

WANT QSORT PAIR[] 2, COMPARE

\ Use the search mechanism of screen1. Prepare data area for `POSITION .
0 _pad !  ^J _pad $C+   "worddoc" _pad $+!
\ Split and store from  string  its firstdoc, leave  remainingdocs  .
: split
   2DUP POSITION DUP IF 1+ >R   OVER R@ OVER -  2,  + R> SWAP OVER -
   ELSE DROP 2, 0 0 THEN ;

1 ARG[] "-r" COMPARE 0= CONSTANT must-swap    must-swap IF SHIFT-ARGS THEN

1 ARG[] GET-FILE
\ Split whole file to `items .
CREATE items   BEGIN split DUP 0=  UNTIL 2DROP
HERE items - 2 CELLS / CONSTANT #items
: item[]  2* CELLS items + 2@ ;

\ For  item#  return its  first  field  . Before ( must be ignored.
: first item[] &( $/ 2DROP &, $/ 2SWAP 2DROP ;
\ For  item#  return its second  field  .
: second item[] &( $/ 2DROP &, $/ 2DROP ;

\ The sorting for html want the priority swapped.
must-swap IF
    'first 'second 2 CELLS EXCHANGE
THEN
\ Compare items with  index1  and  index2  : "index1  is  earlier"
: my-<   2DUP >R first R> first COMPARE DUP IF NIP NIP ELSE
   DROP >R second R> second COMPARE THEN 0< ;

\ Swap items with  index1  and  index2  .
: my-<-->   2* SWAP 2* items PAIR[]   2 CELLS EXCHANGE ;

0 #items 1- 'my-< 'my-<--> QSORT   \ sort `items

8 BASE !   2 ARG[] 664 CREATE-FILE THROW  CONSTANT fd  DECIMAL
#items 0 DO I item[] fd WRITE-FILE THROW LOOP
fd CLOSE-FILE THROW
