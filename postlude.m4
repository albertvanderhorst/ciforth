dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
include(header.m4)
dnl Work around a deficiency in nasm : an ORG requires a numeric argument
define({M4_BIOSBOOT},{07C00H})
dnl architectural consequences
_BOOTFD__1_({define({_SWITCH_1_},_yes)})
_BOOTHD__1_({define({_SWITCH_1_},_yes)})
_BOOTFD__1_({define({_BOOTED_1_},_yes)})
_BOOTHD__1_({define({_BOOTED_1_},_yes)})
_REAL_1_({define({_SWITCH_1_},_no)})
_SWITCH_1_({include(protect.m4)})
_SWITCH_1_({define({_LOW_BUF_1_},_yes)})
_SWITCH_1_({define({_HIGH_BUF_1_},_no)})
_REAL_1_({define({_BITS16_1_},_yes)})
_HOSTED_LINUX_1_({define( {_EQULAYOUT_1_}, _no )})
_HOSTED_LINUX_1_({define( {_BITS32_1_}, _yes )})
dnl immediate consequences
_BITS32_1_({define({M4_CELLWIDTH},4)})
_BITS16_1_({define({M4_CELLWIDTH},2)})
_BITS32_1_({define({_NEXT},{_NEXT32})})
_BITS32_1_({define({_PUSH},{_PUSH32})})
_BITS32_1_({define({_2PUSH},{_2PUSH32})})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
