\ Copyright (2003): Albert van der Horst by GNU Public License
\ $Id$
\  An implementation of the ISO Forth READ-LINE & WRITE-LINE words

\ General idea: the stack diagram of READ-LINE is terribly involved. So
\ what we do here is split it over three levels. The lowest level is
\ involved with the i/o proper and system-dependant thingies. The middle
\ level does the actual work but doesn't care for exceptions. The
\ highest level catches any exceptions and converts to proper ISO
\ interface. The exception / error 6 ( I/O OPERATION OUT OF RANGE) is
\ used, but caught immediately in behalf of end-of-file handling.

\ Has an environmental dependancy upon an addressable stack and
\ big-endianess.

\ ---------------- READ-LINE ------------------------------

\ From HANDLE get : a CHAR. Errors are thrown , 6=eof.
: GETCHAR >R 0 DSP@ 1 R@ READ-FILE THROW 0=  6 AND THROW
    DUP ^M = IF DROP R@ RECURSE THEN
    RDROP ;

\ For CHAR : "it IS a line end".
: eol? ^J = ;

\ To BUFFER read at most COUNT characters from HANDLE.
\ Leave first free ADDRESS in buffer. Errors are thrown, 6=eof.
: (READ-LINE) SWAP BEGIN >R >R   R@ GETCHAR   2DUP SWAP C!
    eol? IF R> R> DROP 0 ELSE 1+ R> R> 1- THEN
    DUP  WHILE REPEAT  2DROP ;

\ To BUFFER read a line of at most COUNT char's from HANDLE.
\ Leave actual COUNT, "chars REMAIN", ERROR.
: READ-LINE ROT DUP >R ROT ROT
    '(READ-LINE) CATCH
     DUP 6 = IF 2DROP 2DROP 0 0 0 ELSE
     DUP IF 0 0 ROT ELSE
     DROP   R@ - -1 0 THEN THEN
     RDROP ;

\ ---------------- WRITE-LINE ------------------------------

\ Linux file ending.
CREATE CR$ 1 , ^J C,
\ Linux default file permission ( 755)
8 BASE ! 755 CONSTANT PERMISSIONS DECIMAL

\ CP/M MSDOS file ending.
\ CREATE CR$ 2 , ^M C,  ^J C,
\ MSDOS default file create properties ( nothing)
\ 0 CONSTANT PERMISSIONS


\ Output CHAR to HANDLE. Errors are thrown.
: PUTCHAR >R  DSP@ 1 R> WRITE-FILE THROW DROP ;

\ Write a line from BUFFER COUNT characters to HANDLE.
\ Errors are thrown.
: (WRITE-LINE) >R  R@ WRITE-FILE THROW  CR$ $@ R> WRITE-FILE THROW ;

\ Write a line from BUFFER COUNT characters to HANDLE.
\ Leave actual ERROR.
: WRITE-LINE '(WRITE-LINE) CATCH DUP IF >R 2DROP DROP R> THEN ;


\ Generate a test file, warning multi-line string.
"" "NOOT" PUT-FILE
"NOOT" 1 OPEN-FILE THROW CONSTANT NOOT

"12345" NOOT  WRITE-LINE
"678"   NOOT  WRITE-LINE
""      NOOT  WRITE-LINE
"9ABCD" NOOT  WRITE-LINE
NOOT  CLOSE-FILE

\ Generate a test file, warning multi-line string.
"12345
678

9ABCD
" "AAP" PUT-FILE

"AAP" 0 OPEN-FILE THROW CONSTANT AAP


." Expect 0 -1 5 | : " PAD 10 AAP READ-LINE . . . &| EMIT CR
." Expect 0 -1 3 678| : " PAD 5 AAP READ-LINE . . . PAD 3 TYPE &| EMIT CR
." Expect 0 -1 0 | : " PAD 5 AAP READ-LINE . . . &| EMIT CR
." Expect 0 -1 3 9AB| : " PAD 3 AAP READ-LINE . . . PAD 3 TYPE &| EMIT CR
." Expect 0 -1 2 3 CD| : " PAD 3 AAP READ-LINE . . . PAD 2 TYPE &| EMIT CR
." Expect 0 0 0 | : " PAD 3 AAP READ-LINE . . . &| EMIT CR

EXIT

\ ----------------------------------------------------------

: (READ-LINE) LOCAL HANDLE LOCAL COUNT
    BEGIN
        HANDLE GETCHAR LOCAL THIS-CHAR
        -1 +TO COUNT
    THIS-CHAR eol? 0=    COUNT 0>   AND WHILE
        THIS-CHAR OVER C!   1+
    REPEAT ;


char *paren_read_line( char *buffer, int count, int handle)
{
    for ( i=count, pc=buffer; i-- && !eol( *pc = getchar(handle) ) ) pc++;
    return pc;
}

char *paren_read_line( char *pc, int count, int handle)
{
    for ( /* */ ; count-- && !eol( *pc = getchar(handle) ) ) pc++;
    return pc;
}
