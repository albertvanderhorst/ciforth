dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
SET_64_BIT_MODE         _C Assembler directive
define({CELL_M4},{QUAD})dnl
define({CWORD},{QWORD})dnl
define({M4_CELLWIDTH},8)dnl
define({SPO},{RSP})dnl
define({RPO},{RBP})dnl
define({HIP},{RSI})dnl
define({WOR},{RAX})dnl
define({_NEXT}, {_NEXT_MACRO})
define({_PUSH}, {_PUSH_MACRO})
define({_2PUSH}, {_2PUSH_MACRO})
