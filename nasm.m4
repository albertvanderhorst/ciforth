dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
define({_HEADER_ASM},{;
; NASM version of FIG FORTH created by M4 from generic listing.
;})dnl
dnl
dnl Directives ignored by NASM.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
define({END},;)dnl Turn END into comment.
define({_OFFSET})dnl Ignore this design error altogether.
define({PAGE},;)dnl Turn PAGE into comment.
define({PROC},;)dnl Turn PROC into comment.
define({TITLE},;)dnl Turn TITLE into comment.
define({_CELL_PTR},{ CELL_M4})dnl Make it specify SIZE only.
dnl
dnl NASM doesnot allow to shift the program counter with ORG
define({_NEW_ORG},{RESB    $1-($-$$)})dnl
dnl
define({_RESB},{RESB    $1})dnl
dnl
dnl Specification of the length of an operand,
dnl needed to get exactly the same code as MASM.
define({_BYTE},BYTE)dnl Operand has length BYTE.
dnl Fill in where MASM introduces a superfluous NOP
define({_EXTRANOP},NOP)dnl Superfluous nops introduced by MASM
dnl
dnl More complicated tricks to get rid of MASM.
define({_ENDP},;)dnl Each ENDP is started with _ENDP in generic listing.
dnl
dnl Handling large blocks of comment
define({_COMMENT},{%if 0})dnl
define({_ENDCOMMENT},{%endif})dnl
define({_BYTE_PTR},{BYTE $1})dnl
define({SET_16_BIT_MODE},{BITS   16})dnl
define({SET_32_BIT_MODE},{BITS   32})dnl
define({_ALIGN},{ALIGN    $1})dnl   
define({DSS},{DB})dnl
