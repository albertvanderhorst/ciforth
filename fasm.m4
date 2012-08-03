dnl $Id$
dnl Copyright(2011): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Macro's to adapt the source to Flat Assembler
divert(-1)dnl
define({TITLE},;)dnl Turn TITLE into comment.
define({PAGE},;)dnl Turn PAGE into comment.
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in _HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
define({_dbquoted},"{{$1}}")dnl
define({_sgquoted},'{{$1}}')dnl
define({_quoted},{ifelse( -1, index({$1},{"}),{_dbquoted},{_sgquoted})}({{$1}}))
dnl
define({_C},{{;}})
define({_HEADER_ASM},{;
; FASM version of ciforth created by ``m4'' from generic listing.
; This source can be assembled using the Flat Assembler,
;  available from the Net.
        FORMAT  PE console  ; Instead of telling the linker.
;
        INCLUDE 'INCLUDE/WIN32A.INC'      ; ASCII windows definitions.
;})dnl
define({SET_32_BIT_MODE},{ use32 })dnl
define({_TEXT},{.text})
define({_SECTION_},{       SECTION '$1'})
define({_SECTION_NOBITS_},{})
define({_SECTION_END_},)
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - ORIG})
dnl
dnl FASM reserves memory in a sensible way.
define({_RESB}, {RB  ($1) })dnl
dnl
dnl Assembly Pointer
define({_AP_}, {$})dnl
dnl
dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)dnl
dnl Introduced in behalf of MASM
define({_OR_},{OR})
dnl
dnl Pointer handling
define({_BYTE_PTR},{BYTE PTR $1})dnl
define({_CALL_FAR_INDIRECT},{CALL DWORD PTR [$1]})dnl Perfectly unreasonable!
define({_FAR_ADDRESS},{[$1:$2]})dnl
define({_CELL_PTR},{WORD})dnl Sometimes really needed even after introducing [].
define({_OFFSET},)dnl
define({LONG},{DWORD})dnl
dnl
dnl Handling large blocks of comment
dnl This just doesn't work, because fasm syntax checks the content.
dnl define({_COMMENT},{        if 0
dnl })dnl
dnl define({_ENDCOMMENT},{       end if
dnl })dnl
dnl Last possibility of block comment, suppress it in output.
define({_COMMENT},{_SUPPRESSED(})
define({_ENDCOMMENT},{)})
define({_ENDOFPROGRAM},{
        ENTRY  $1
})dnl
define({_ALIGN},{ALIGN    $1})dnl
define({DSS},{DB})dnl
define({EQU},{=})dnl
define({EXTRN},{;})dnl
define({_DX16},{{DX}})dnl
dnl Work around for nasm. Here needed?
define({A32},{DB 0x67
        })
define({_FASM_}, _yes )
divert{}dnl
