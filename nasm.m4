dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Once and for all. 
changequote({,})dnl
dnl
define({_HEADER_ASM},{;
; NASM version of FIG FORTH created by M4 from generic listing.
;})dnl
dnl
dnl Directives ignored by NASM.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
define({END},;)dnl Turn END into comment.
define({OFFSET})dnl Ignore this design error altogether. 
define({PAGE},;)dnl Turn PAGE into comment.
define({PROC},;)dnl Turn PROC into comment.
define({TITLE},;)dnl Turn TITLE into comment.
define({_WORD_PTR},)dnl Ignore 'WORD PTR' altogether. 
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
define({_COMMENT}, %if 0 
)dnl
define({_ENDCOMMENT}, %endif
)dnl
