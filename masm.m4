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
define({_OFFSET},OFFSET)dnl 
dnl
dnl
dnl Second and later uses of ORG shift the program counter 
define({_NEW_ORG},
        ORG    $1)dnl
dnl Handling large blocks of comment
define({_COMMENT},COMMENT ~
)dnl
define({_ENDCOMMENT},~
)dnl
define({_0BRANCH},dnl
DW      ZBRAN
        DW      $1-$)dnl
define({_BRANCH},dnl
DW      BRAN
        DW      $1-$)dnl
define({_LOOP},dnl
DW      XLOOP
        DW      $1-$)dnl
define({_BYTE_PTR},BYTE PTR $1)dnl

