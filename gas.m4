dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)dnl
dnl
define({_HEADER_ASM},{;
; ``GNU assembler'' (gas) version of ciforth created by M4 from generic listing.
; The next line may not be true as we are still in a constuction phase:
; This version can be assembled on a Linux system by
;   as forth.asm -o forth.o
;   ld forth.o -o forth
; We could use ".Intel_syntax noprefix" , but in fact there is so
; much crap that we need ``sed'' anyway. The % prefixes, and the
; the destination-source are in fact no big deal.
;})dnl
dnl
dnl Directives ignored by gas.
define({ASSUME},;)dnl Turn ASSUME into comment.
define({CSEG},;)dnl Turn CSEG into comment.
define({END},;)dnl Turn END into comment.
define({_OFFSET},{{$}})dnl Immediate data.
define({PAGE},;)dnl Turn PAGE into comment.
define({PROC},;)dnl Turn PROC into comment.
define({TITLE},;)dnl Turn TITLE into comment.
define({_CELL_PTR},{ CELL_M4})dnl Make it specify SIZE only.
dnl
dnl gas doesnot allow to shift the program counter with ORG
define({_NEW_ORG},{RESB    $1-(_AP_-$$)})dnl
dnl
dnl Assembly Pointer
define({_AP_}, {.})dnl
dnl
dnl gas uses c-operators such as | and &
define({_OR_},{|})
dnl
define({_RESB},       {.SPACE   $1})dnl
dnl
dnl Specification of the length of an operand,
dnl needed to get exactly the same code as MASM.
define({_BYTE},BYTE)dnl Operand has length BYTE.
dnl Fill in where MASM introduces a superfluous NOP
define({_EXTRANOP},NOP)dnl Superfluous nops introduced by MASM
dnl
dnl More complicated tricks to get rid of MASM.
define({_ENDP},;)dnl Each ENDP is started with _ENDP in generic listing.
dnl
dnl Handling large blocks of comment
define({_COMMENT}, { /* })dnl
define({_ENDCOMMENT}, { */ })dnl
define({_BYTE_PTR},{$1})dnl
define({_CELL_PTR},{$1})dnl
define({SET_32_BIT_MODE},)dnl
dnl The padding value is specified to get the same code as in nasm.
define({_ALIGN},{{.balign    4,0x90}})dnl
define({DSS},{.ASCII})dnl
dnl
define({LODSD},{{LODSL}})dnl
define({extern},{{.extern}})dnl
define({global},{{.global}})dnl
dnl Index registers
define({EDI},{{%EDI}})dnl
define({ESI},{{%ESI}})dnl
dnl Stacks and base registers
define({EBP},{{%EBP}})dnl
dnl Normal registers
define({EAX},{{%EAX}})dnl
define({EBX},{{%EBX}})dnl
define({ECX},{{%ECX}})dnl
define({EDX},{{%EDX}})dnl
dnl Segment registers
define({CS},{{%CS}})dnl
define({DS},{{%DS}})dnl
define({ES},{{%ES}})dnl
define({SS},{{%SS}})dnl
dnl Don't define the half registers, require context often!
dnl
define({SHORT},{})dnl
define({INT},{{INT} $1})dnl
dnl A nobits section takes no place in the object file.
define({_SECTION_},{.section $1,"awx"})
define({_SECTION_NOBITS_},{.section $1,"awx",@nobits})
dnl leave this to sed for the moment.
dnl define({DD},{.long})
dnl define({DW},{.word})
dnl define({DB},{.byte})
divert{}dnl
