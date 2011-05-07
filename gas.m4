dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)dnl
dnl
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
})dnl
define({_C},{{#}})
define({_O},{{0$1}})
define({ASSUME},#)dnl Turn ASSUME into comment.
define({CSEG},#)dnl Turn CSEG into comment.
define({END},#)dnl Turn END into comment.
define({TITLE},#)dnl Turn TITLE into comment.
define({PAGE},#)dnl Turn PAGE into comment.
define({GLOBAL},.global)dnl
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - ORIG})
dnl
define({_RESB},       {.SPACE   $1})dnl
dnl
dnl Assembly Pointer
define({_AP_}, {.})dnl
dnl
dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)dnl
dnl
dnl MASM uses operators as keywords. We want bitwise or here!
define({_OR_},{|})
dnl
dnl Pointer handling
define({_BYTE_PTR},{BYTE PTR $1})dnl
define({_CALL_FAR_INDIRECT},{CALL CWORD PTR [$1]})dnl Perfectly unreasonable!
define({_CELL_PTR},{CWORD PTR})dnl Sometimes really needed even after introducing [].
define({SET_32_BIT_MODE},)dnl
define({SET_64_BIT_MODE},)dnl
define({_OFFSET},OFFSET)dnl
dnl
dnl Handling large blocks of comment
define({_COMMENT}, { /* })dnl
define({_ENDCOMMENT}, { */ })dnl
dnl
dnl A nobits section takes no place in the object file.
define({_SECTION_},{.text})
define({_SECTION_NOBITS_},{.bss})
dnl leave this to sed for the moment.
define({_ALIGN},{.balign    M4_CELLWIDTH{,}0x00})dnl
define({DSS},{.ASCII})dnl
dnl string register
define({EDI},{{%EDI}})dnl
define({ESI},{{%ESI}})dnl
define({RDI},{{%RDI}})dnl
define({RSI},{{%RSI}})dnl
dnl Stacks and base registers
define({EBP},{{%EBP}})dnl
define({ESP},{{%ESP}})dnl
define({RBP},{{%RBP}})dnl
define({RSP},{{%RSP}})dnl
dnl Normal registers
define({EAX},{{%EAX}})dnl
define({EBX},{{%EBX}})dnl
define({ECX},{{%ECX}})dnl
define({EDX},{{%EDX}})dnl
define({RAX},{{%RAX}})dnl
define({RBX},{{%RBX}})dnl
define({RCX},{{%RCX}})dnl
define({RDX},{{%RDX}})dnl
define({_DX16},{{%DX}})dnl  Used for in and outports
dnl Segment registers
define({CS},{{%CS}})dnl
define({DS},{{%DS}})dnl
define({ES},{{%ES}})dnl
define({FS},{{%FS}})dnl
define({GS},{{%GS}})dnl
define({SS},{{%SS}})dnl
dnl Half registers
define({AL},{{%AL}})dnl
define({AH},{{%AH}})dnl
define({BL},{{%BL}})dnl
define({BH},{{%BH}})dnl
define({CL},{{%CL}})dnl
define({CH},{{%CH}})dnl
define({DL},{{%DL}})dnl
define({DH},{{%DH}})dnl
define({DQ},{.quad})
define({DD},{.long})
define({DW},{.word})
define({DB},{.byte})
define({EQU},{=})
divert{}dnl
