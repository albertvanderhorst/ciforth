dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
dnl Once and for all. 
changequote({,})dnl
dnl
dnl Directives ignored by NASM.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
define({END},;)dnl Turn END into comment.
define({OFFSET})dnl Ignore this design error altogether.
define({PAGE},;)dnl Turn PAGE into comment.
define({PROC},;)dnl Turn PROC into comment.
define({TITLE},;)dnl Turn TITLE into comment.
dnl
dnl More complicated tricks to get rid of MASM.
define({_ENDP},;)dnl Each ENDP is started with _ENDP in generic listing.
dnl
dnl Handling large blocks of comment
define({_COMMENT}, %if 0 
)dnl
define({_ENDCOMMENT}, %endif
)dnl
