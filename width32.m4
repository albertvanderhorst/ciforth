dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl FULLY EXPERIMENTAL HAS NEVER BEEN LOGGED IN.
SET_32_BIT_MODE         ; Assembler directive
define({BLOCKS_IN_USE_MASK},{80000000H})dnl
define({CELL},LONG)dnl
define({CELLWIDTH},4)dnl
define({CELLP},{LIT, CW, PLUS})dnl
define({LCELL},{LIT, CW})dnl
define({DC},{DD})dnl
define({AX},{EAX})dnl
define({BX},{EBX})dnl
define({CX},{ECX})dnl
define({DX},{EDX})dnl
define({DI},{EDI})dnl
define({SI},{ESI})dnl
define({BP},{EBP})dnl
define({STACKPOINTER},{ESP})dnl
define({LODSW},{LODSD})dnl
