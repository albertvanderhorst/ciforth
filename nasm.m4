dnl $Id$
dnl Copyright(2017): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in _HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
define({_dbquoted},"{{$1}}")
define({_sgquoted},'{{$1}}')
define({_quoted},{ifelse( -1, index({$1},{"}),{_dbquoted},{_sgquoted})}({{$1}}))
dnl comment
define({_C},{{;}})
dnl octal
define({_O},{{$1Q}})
define({_HEADER_ASM},{;
; This is a NASM version of ciforth created by ``m4'' from the generic listing.
; It can be assembled using ``nasm'' obtainable via :
; Source: ftp://ftp.us.kernel.org/pub/software/devel/nasm/source/
; URL: http://www.cryogen.com/Nasm/
_HOSTED_OSX_({
; This version can be assembled on an OS X system (Apple):
;   nasm -f macho xina.asm -o xina.o
;   ld xina.o -segprot __TEXT rwx rwx -segprot __DATA rwx rwx -o xina
;And to get the smallest possible executable(optional):
;   strip xina
define({_TEXT_},{ section .text })
define({_BSS_},{ section .bss })
})
_LINUX_N_({
; This version can be assembled on a Linux system by
 _BITS32_({ ;   nasm forth.asm -felf -g -o forth.o
;   ld -melf_i386 -N forth.o -o forth
; Tested with nasm version 2.10.01.
})
 _BITS64_({ ;   nasm forth.asm -felf64 -o forth.o
;   ld -melf_x86_64  -N forth.o -o forth
; Tested with nasm version 2.10.01.
})
;And to get the smallest possible executable(optional):
;   strip forth -s -R .data
})
_LINUX_C_({
; This source is to be linked with c-code. You are referred to the
; information in the generic system for building the executable.
; In general be warned that GNU(-Linux) is hard on those linking c
;  with assembler code.
})
_PC_({
; This version can be assembled on a Linux system in behalf of a
; _HOSTED_MSDOS_({MS-DOS}) _BOOTED_({standalone (booting)}) version by
;   nasm -fbin ciforth.asm -o ciforth.com})
; For assembling on other systems where nasm is available see the
; documentation of nasm.
_BOOTED_({; See again the generic system manual for more information how to install
; booting versions.
})
})

dnl Directives ignored by NASM.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
define({END},;)dnl Turn END into comment.
define({PAGE},;)dnl Turn PAGE into comment.
define({PROC},;)dnl Turn PROC into comment.
define({TITLE},;)dnl Turn TITLE into comment.

dnl NASM doesnot allow to shift the program counter with ORG
define({_NEW_ORG},{RESB    $1-(_AP_-$$)})

dnl Assembly Pointer
define({_AP_}, {$})

dnl NASM uses c-operators such as | and &
define({_OR_},{|})

define({_RESB},{RESB    $1})

dnl Specification of the length of an operand,
dnl needed to get exactly the same code as MASM.
define({_BYTE},BYTE)dnl Operand has length BYTE.
dnl Fill in where MASM introduces a superfluous NOP
define({_EXTRANOP},NOP)dnl Superfluous nops introduced by MASM

dnl More complicated tricks to get rid of MASM.
define({_ENDP},;)dnl Each ENDP is started with _ENDP in generic listing.

define({RELATIVE_WRT_ORIG}, {$1 - $$})

dnl Pointer handling
define({_BYTE_PTR},{BYTE $1})
define({_CALL_FAR_INDIRECT},{CALL FAR [$1]})
define({_FAR_ADDRESS},{[$1:$2]})
define({_CELL_PTR},{ CELL_M4})dnl Make it specify SIZE only.
define({_OFFSET},)dnl Ignore this design error altogether.
define({QUAD},{QWORD})

dnl Handling large blocks of comment
define({_COMMENTED},{%if 0}$1{%endif})
define({SET_16_BIT_MODE},{BITS   16})
define({SET_32_BIT_MODE},{BITS   32})
define({SET_64_BIT_MODE},{BITS   64})
define({_ALIGN},{ALIGN    M4_CELLWIDTH})
define({DSS},{DB})
dnl A nobits section takes no place in the object file.
define({_TEXT_},{section .text})
define({_BSS_},{section .bss})
define({_ENDOFPROGRAM},{
        END $1
})
dnl A quirk, make sure in port instruction, DX is not turned into EDX
dnl in behalf of ports
define({_DX16},{{DX}})
define({_AX16},{{AX}})
define({_AX32},{{EAX}})
dnl Work around because of poor performance of .NET assembler.
define({_OR_},{|})
divert{}
