\ Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
\ $Id$
\ Convert a text file to a .lab file.

WANT WRITE-LINE
WANT ARG[]

\ TYPE on the error channel
: (etype) 2 WRITE-FILE DROP ;
'(etype) 'ETYPE 3 CELLS MOVE

: ECR CR$ $@ ETYPE ;
: E. S>D 0 (D.R) ETYPE ;

\ Current input line.
VARIABLE LINE   0 LINE !

\ Print error MESSAGE and exit.
: ERROR
    "Error at input line " ETYPE   LINE @ E. ECR
    "   " ETYPE ( m) ETYPE ECR
    1 EXIT-CODE !   BYE ;

\ Checks a line STRING, but leaves IT unchanged.
\ Warn for lines where the last character is used up,
\ such that use separating lines in blocks is difficult.
\ This is whenever the line count is 64 chars.
: CHECK DUP 64 = IF
    "Input line " ETYPE   LINE @ E.
    " has non-blank in last position" ETYPE ECR 2DUP ETYPE ECR
THEN ;


VARIABLE INPUT        VARIABLE OUTPUT   1 OUTPUT !
\ Check and open files passed as arguments, leaving in above variables.
: GET-HANDLES
    3 ARGC <> IF "Usage: fromblk fromfile tofile" ERROR THEN
    1 ARG[] 0 OPEN-FILE THROW INPUT !
    2 ARG[] "" 2OVER PUT-FILE 1 OPEN-FILE THROW OUTPUT !
;

\ Close the files passed as arguments, using above variables.
: CLOSE-HANDLES INPUT @ CLOSE-FILE THROW OUTPUT @ CLOSE-FILE THROW ;

\ Read a block line, return as a STRING (at ``PAD'').
: INPUT-LINE   PAD   DUP 64 INPUT @ READ-FILE THROW ;

\ Output STRING as a line, but warn if too long (64 char)
: OUTPUT-LINE   -TRAILING 2DUP CHECK   OUTPUT @ WRITE-LINE THROW ;

\ Convert arg 1 to arg 2 , from a block file to a text file.
: MAIN    GET-HANDLES
    BEGIN   INPUT-LINE DUP WHILE   1 LINE +!   OUTPUT-LINE REPEAT
    CLOSE-HANDLES
;
