dnl $Id$
dnl Copyright(2011): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Macro's to adapt the source to Flat Assembler
divert(-1)
define({TITLE},;)dnl Turn TITLE into comment.
define({PAGE},;)dnl Turn PAGE into comment.
define({GLOBAL},{;})dnl Start at first point in executable segment
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in _HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
define({_dbquoted},"{{$1}}")
define({_sgquoted},'{{$1}}')
define({_quoted},{ifelse( -1, index({$1},{"}),{_dbquoted},{_sgquoted})}({{$1}}))

define({_C},{{;}})
dnl octal
define({_O},{{$1O}})
define({_HEADER_ASM},{;
; FASM version of ciforth created by ``m4'' from generic listing.
; This source can be assembled using the Flat Assembler,
;  available from the Net.
_DLL_(
{;      fasm forth.asm forth
        FORMAT  PE console  ; Instead of telling the linker.
;
        INCLUDE 'include/win32a.inc'      ; ASCII windows definitions.
define({_SECTION_NOBITS_},{})
})_C{}_END_({ _DLL_})
_LINUX_N_(
{;      fasm forth.asm forth
_BITS32_({define({ELF_FORMAT},{ELF})})
_BITS64_({define({ELF_FORMAT},{ELF64})})
        FORMAT  ELF_FORMAT EXECUTABLE ; Instead of telling the linker.
define({_SECTION_NOBITS_},{       SEGMENT executable readable writable})
;
})_C{}_END_({ _LINUX_N_})
;})
define({SET_32_BIT_MODE},{ use32 })
define({SET_64_BIT_MODE},{ use64 })
define({_TEXT},{.text})
define({_SECTION_},       {       SEGMENT executable readable writable})
define({_SECTION_END_},)
dnl Get rid of unknown MASM specifier.
define({_BYTE},)

define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - ORIG})

dnl FASM reserves memory in a sensible way.
define({_RESB}, {RB  ($1) })

dnl Assembly Pointer
define({_AP_}, {$})

dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)
dnl Introduced in behalf of MASM
define({_OR_},{OR})

dnl Pointer handling
define({_BYTE_PTR},{BYTE PTR $1})
define({_CALL_FAR_INDIRECT},{CALL DWORD PTR [$1]})dnl Perfectly unreasonable!
define({_FAR_ADDRESS},{[$1:$2]})
define({_CELL_PTR},{WORD})dnl Sometimes really needed even after introducing [].
define({_OFFSET},)
define({LONG},{DWORD})
define({QUAD},{QWORD})

dnl Handling large blocks of comment
dnl This just doesn't work, because fasm syntax checks the content.
dnl define({_COMMENT},{        if 0
dnl })
dnl define({_ENDCOMMENT},{       end if
dnl })
dnl Last possibility of block comment, suppress it in output.
define({_COMMENT},{_SUPPRESSED(})
define({_ENDCOMMENT},{)})
define({_ENDOFPROGRAM},{
_DLL_({
        ENTRY  $1
})_C{}_END_({ _DLL_})
})
define({_ALIGN},{ALIGN    M4_CELLWIDTH})
define({DSS},{DB})
define({EQU},{=})
define({EXTRN},{;})
dnl in behalf of ports
define({_DX16},{{DX}})
define({_AX16},{{AX}})
define({_AX32},{{EAX}})
dnl Work around for nasm. Here needed?
define({A32},{DB 0x67
        })
define({_FASM_}, _yes )
divert{}
