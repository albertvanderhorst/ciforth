dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
include(header.m4)
dnl architectural consequences
_BOOTFD__1_({define({_NATIVE_1_},_yes)})
_BOOTHD__1_({define({_NATIVE_1_},_yes)})
_REAL_1_({define({_NATIVE_1_},_no)})
dnl For the moment NATIVE is identical to having to switch modes
dnl This need not remain so in the future.
_NATIVE_1_({include(protect.m4)})
_NATIVE_1_({define({_LOW_BUF_1_},_yes)})
_NATIVE_1_({define({_HIGH_BUF_1_},_no)})
_REAL_1_({define({_BITS16_1_},_yes)})
_HOSTED_LINUX_1_({define( {_EQULAYOUT_1_}, _no )})
dnl immediate consequences
_BITS32_1_({define({M4_CELLWIDTH},4)})
_BITS16_1_({define({M4_CELLWIDTH},2)})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
include(constant.m4)
divert{}dnl
