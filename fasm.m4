dnl $Id$
dnl Copyright(2017): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Macro's to adapt the source to Flat Assembler
divert(-1)
dnl Directives ignored by FASM.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
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
; Default for fasm: one section, which is called segment (!)
_DLL_(
{;      fasm forth.asm forth
        ; fasm generates executable, no separate linking.
       _BITS32_({FORMAT  PE console})_BITS64_({ FORMAT  PE64 console})
;
        INCLUDE _BITS32_({'include/win32a.inc'})_BITS64_({'include/win64a.inc'})      ; ASCII windows definitions.
define({_BSS_},{})dnl
define({_TEXT_},       { section '.text' code executable readable writable})dnl
})_C{}_END_({ _DLL_})
_HOSTED_OSX_({
; This version can be assembled on an OS X system (Apple):
;   fasm -f elf xina.asm -o xina.o
;   convert to macho format
;   ld xina.o -segprot __TEXT rwx rwx -segprot __DATA rwx rwx -o xina
;   However as per 2016 dec 21 , it doesn't run.
        FORMAT  ELF     ; No macho, go via ELF object format.
define({_TEXT_},{ section '.text' executable  })dnl
define({_BSS_},{ section '.bss' writable  })dnl
})
_LINUX_N_(
{;      fasm forth.asm forth
_BITS32_({define({ELF_FORMAT},{ELF})})
_BITS64_({define({ELF_FORMAT},{ELF64})})
        ; fam generates executable, no separate linking.
        FORMAT  ELF_FORMAT EXECUTABLE
;
define({_TEXT_},       {       SEGMENT executable readable writable})dnl
define({_BSS_},{})dnl
})_C{}_END_({ _LINUX_N_})
;})
define({SET_16_BIT_MODE},{ use16 })
define({SET_32_BIT_MODE},{ use32 })
define({SET_64_BIT_MODE},{ use64 })
dnl Get rid of unknown MASM specifier.
define({_BYTE},)

define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - _ORIG})

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
define({_BYTE_PTR},{BYTE $1})
define({_CALL_FAR_INDIRECT},{CALL DWORD PTR [$1]})dnl Perfectly unreasonable!
define({_FAR_ADDRESS},{[$1:$2]})
define({_CELL_PTR},{WORD})dnl Sometimes really needed even after introducing [].
define({_OFFSET},)
define({LONG},{DWORD})
define({QUAD},{QWORD})

dnl Handling large blocks of comment
dnl This just doesn't work, because fasm syntax checks the content.
define({_COMMENTED},{patsubst({$1},{^},{;})})
dnl Alternative if patsubst not available.
dnl define({_COMMENTED},{_SUPPRESSED})
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
dnl STRANGE ODDS AND ENDS
dnl apparently display is a reserved word.
define({DISPLAY},{MY_DISPLAY})
divert{}
