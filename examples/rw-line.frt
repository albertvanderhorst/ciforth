
WANT READ-LINE
WANT WRITE-LINE


\
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
