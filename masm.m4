dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
changequote({,})dnl
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl 
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_WRD_PTR},WRD PTR)dnl Sometimes really needed even after introducing [].
dnl
dnl Handling large blocks of comment
define({_COMMENT},COMMENT ~
)dnl
define({_ENDCOMMENT},~
)dnl

