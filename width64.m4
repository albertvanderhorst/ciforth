dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl None of the following makes sense unless 64 bit mode
SET_64_BIT_MODE         _C Assembler directive
divert(-1)
define({CELL_M4},{QUAD})dnl
define({CWORD},{QWORD})dnl
define({SPO},{RSP})dnl
define({RPO},{RBP})dnl
define({HIP},{RSI})dnl
define({WOR},{RAX})dnl
define({DC},{DQ})dnl
define({AX},{RAX})dnl
define({BX},{RBX})dnl
define({CX},{RCX})dnl
define({DX},{RDX})dnl
define({DI},{RDI})dnl
define({SI},{RSI})dnl
define({BP},{RBP})dnl
define({SP},{RSP})dnl
define({LODS},{LODSQ})dnl Only instruction to be changed.
define({WORD},{QUAD})dnl
define({_NEXT}, {_NEXT_MACRO})
define({_PUSH}, {_PUSH_MACRO})
define({_2PUSH}, {_2PUSH_MACRO})
divert{}dnl
