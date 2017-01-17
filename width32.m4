dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2017): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl None of the following makes sense unless 32 bit mode
SET_32_BIT_MODE         _C Assembler directive
divert(-1)
define({CELL_M4},LONG)dnl
define({CWORD},{DWORD})dnl
define({SPO},{ESP})dnl
define({RPO},{EBP})dnl
define({HIP},{ESI})dnl
define({WOR},{EAX})dnl
define({DC},{DD})dnl
define({AX},{EAX})dnl
define({BX},{EBX})dnl
define({CX},{ECX})dnl
define({DX},{EDX})dnl
define({DI},{EDI})dnl
define({SI},{ESI})dnl
define({BP},{EBP})dnl
define({SP},{ESP})dnl
define({LODS},{LODSD})dnl Only instruction to be changed.
define({WORD},{LONG})dnl
define({_NEXT}, {_NEXT_MACRO})
define({_PUSH}, {_PUSH_MACRO})
define({_2PUSH}, {_2PUSH_MACRO})
divert{}dnl
