dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
define({_HEADER_ASM},{;
; MASM/TASM version of ciforth created by ``m4'' from generic listing.
; This source can be assembled using the ubiquitous microsoft MASM assembler.
; Alternatively with the Borland TASM assembler.
;})dnl
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_CELL_PTR},{WORD PTR})dnl Sometimes really needed even after introducing [].
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({_OFFSET},OFFSET)dnl
define({RELATIVE_WRT_ORIG}, {$1 - ORIG})
dnl
dnl MASM has an unreasonable quirk to reserve memory
define({_RESB},
        DB      $1 DUP (?))dnl
dnl
dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)dnl
dnl
dnl MASM uses operators as keywords
define({_OR_},{OR})
dnl
dnl Handling large blocks of comment
define({_COMMENT},COMMENT ~
)dnl
define({_ENDCOMMENT},~
)dnl
define({_BYTE_PTR},{BYTE PTR $1})dnl
define({_ALIGN},{ALIGN    $1})dnl
define({DSS},{DB})dnl
define({M4_EM},0FFFEH)
