dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
include(header.m4)
dnl architectural consequences
dnl Look at this as a knowledge base:
dnl A(define(B),y) means if we have A, then we need B too.
dnl A(B(define(C),y)) means if we have A and B then we need C too.
dnl A(define(C),y) combined with B(define(C),y) means 
dnl if we have A or we have B, then we need C too.

_BOOTFD__1_({define({_PC_1_},_yes)})
_BOOTFD__1_({define({_RWFD_1_},_yes)})
_BOOTHD__1_({define({_PC_1_},_yes)})
_BOOTHD__1_({define({_RWHD_1_},_yes)})
_HOSTED_MSDOS_1_({define({_PC_1_}, _yes )})
_PC_1_({define({_DIRECTMAPPED_1_}, _yes )})
dnl switch back and forth between protected and real mode.
_PC_1_(_PROTECTED_1_({define({_SWITCH_1_},_yes)}))
_BOOTFD__1_({define({_BOOTED_1_},_yes)})
_BOOTHD__1_({define({_BOOTED_1_},_yes)})
_SWITCH_1_({include(protect.m4)})
_SWITCH_1_({define({_LOW_BUF_1_},_yes)})
_SWITCH_1_({define({_HIGH_BUF_1_},_no)})
_BOOTED_1_({define({_ABSOLUTELOAD_1_},_yes)})
_SWITCH_1_({define({_ABSOLUTELOAD_1_},_yes)})
_REAL_1_({define({_BITS16_1_},_yes)})
_HOSTED_LINUX_1_({define( {_EQULAYOUT_1_}, _no )})
_HOSTED_LINUX_1_({define( {_BITS32_1_}, _yes )})
dnl immediate consequences
_BITS32_1_({define({M4_CELLWIDTH},4)})
_BITS16_1_({define({M4_CELLWIDTH},2)})
_BITS16_1_({define({M4_INITDP},{TEXTEND})})
_BITS32_1_({define({_NEXT},{_NEXT32})})
_BITS32_1_({define({_PUSH},{_PUSH32})})
_BITS32_1_({define({_2PUSH},{_2PUSH32})})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
