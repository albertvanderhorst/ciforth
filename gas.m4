dnl $Id: gas.m4,v 5.16 2017/11/10 18:33:43 albert Exp $
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)

dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in _HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
define({_quoted},{"patsubst({{$1}},{\([\"]\)},{\\\1})"})
define({_HEADER_ASM},{#
# Gnu as version of ciforth created by ``m4'' from generic listing.
# This source can be assembled using versions better than 2.13.
# This file can be assembled and linked in the following way:
#       as _BITS64_({-64}) lina.s
#       ld -s -N a.out -o lina
# The -s (strip) may not be necessary.

.Intel_syntax prefix
})
define({_C},{{#}})
define({_O},{{0$1}})
define({ASSUME},#)dnl Turn ASSUME into comment.
define({CSEG},#)dnl Turn CSEG into comment.
define({END},#)dnl Turn END into comment.
define({TITLE},#)dnl Turn TITLE into comment.
define({PAGE},#)dnl Turn PAGE into comment.
define({GLOBAL},.global)
dnl Get rid of unknown MASM specifier.
define({_BYTE},)

define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - _ORIG})

define({_RESB},       {.SPACE   $1})

dnl Assembly Pointer
define({_AP_}, {.})

dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)

dnl MASM uses operators as keywords. We want bitwise or here!
define({_OR_},{|})

dnl Pointer handling
define({_BYTE_PTR},{BYTE PTR $1})
define({_CALL_FAR_INDIRECT},{CALL CWORD PTR [$1]})dnl Perfectly unreasonable!
define({_FAR_ADDRESS},{{$1:[$2]}})
define({_CELL_PTR},{CWORD PTR})dnl Sometimes really needed even after introducing [].
define({SET_32_BIT_MODE},)
define({SET_64_BIT_MODE},)
define({_OFFSET},OFFSET)

dnl Handling large blocks of comment
define({_COMMENTED}, { /* }$1{ */ })

dnl A nobits section takes no place in the object file.
define({_TEXT_},{.text})
define({_BSS_},{.bss})
define({_ENDOFPROGRAM},{
        END $1
})
dnl leave this to sed for the moment.
define({_ALIGN},{.balign    M4_CELLWIDTH{,}0x00})
define({DSS},{.ASCII})
dnl string register
define({EDI},{{%EDI}})
define({ESI},{{%ESI}})
define({RDI},{{%RDI}})
define({RSI},{{%RSI}})
dnl Stacks and base registers
define({EBP},{{%EBP}})
define({ESP},{{%ESP}})
define({RBP},{{%RBP}})
define({RSP},{{%RSP}})
dnl Normal registers
define({EAX},{{%EAX}})
define({EBX},{{%EBX}})
define({ECX},{{%ECX}})
define({EDX},{{%EDX}})
define({RAX},{{%RAX}})
define({RBX},{{%RBX}})
define({RCX},{{%RCX}})
define({RDX},{{%RDX}})
dnl 64 bit registers
define({R8},{{%R8}})
define({R9},{{%R9}})
define({R10},{{%R10}})
define({R11},{{%R11}})
define({R12},{{%R12}})
define({R13},{{%R13}})
define({R14},{{%R14}})
define({R15},{{%R15}})
dnl in behalf of ports
define({_DX16},{{%DX}})
define({_AX16},{{%AX}})
define({_AX32},{{%EAX}})
dnl Segment registers
define({CS},{{%CS}})
define({DS},{{%DS}})
define({ES},{{%ES}})
define({FS},{{%FS}})
define({GS},{{%GS}})
define({SS},{{%SS}})
dnl Half registers
define({AL},{{%AL}})
define({AH},{{%AH}})
define({BL},{{%BL}})
define({BH},{{%BH}})
define({CL},{{%CL}})
define({CH},{{%CH}})
define({DL},{{%DL}})
define({DH},{{%DH}})
define({DQ},{.quad})
define({DD},{.long})
define({DW},{.word})
define({DB},{.byte})
define({EQU},{=})
divert{}
