dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
define({_HEADER_ASM},{;
; MASM/TASM version of FIG FORTH created by M4 from generic listing.
;})dnl
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl 
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_WORD_PTR},WORD PTR)dnl Sometimes really needed even after introducing [].
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
dnl
dnl Handling large blocks of comment
define({_COMMENT},COMMENT ~
)dnl
define({_ENDCOMMENT},~
)dnl

