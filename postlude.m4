dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
include(header.m4)
dnl architectural consequences
dnl Look at this as a knowledge base:
dnl A(define(B),y) means if we have A, then we need B too.
dnl A({B({define(C),y})}) means if we have A and B then we need C too.
dnl A(define(C),y) combined with B(define(C),y) means 
dnl if we have A or we have B, then we need C too.

_BITS32_({define({_PROTECTED_},_yes)})
_REAL_({define({_BITS16_},_yes)})
_BOOTFD_({define({_RWFD_},_yes)})
_BOOTHD_({define({_RWHD_},_yes)})
_BOOTFD_({define({_BOOTED_},_yes)})
_BOOTHD_({define({_BOOTED_},_yes)})
_BOOTED_({define({_PC_},_yes)})
_HOSTED_MSDOS_({define({_PC_}, _yes )})
_LINUX_N_({ define( {_HOSTED_LINUX_}, _yes )})       
_LINUX_C_({ define( {_HOSTED_LINUX_}, _yes )})       
_HOSTED_LINUX_({ define( {_BITS32_}, _yes )})
_PC_({define({_DIRECTMAPPED_}, _yes )})
_PC_({define({_NORMAL_BYE_}, _yes )})
dnl switch back and forth between protected and real mode.
_PC_({_PROTECTED_({define({_SWITCH_},_yes)})})
_SWITCH_({include(protect.m4)})
_SWITCH_({define({_LOW_BUF_},_yes)})
_SWITCH_({define({_HIGH_BUF_},_no)})
_BOOTED_({define({_ABSOLUTELOAD_},_yes)})
_BOOTED_({define({_USEBIOS_},_yes)})
_SWITCH_({define({_ABSOLUTELOAD_},_yes)})
_ABSOLUTELOAD_({define({_NORMAL_BYE_},_no)})
_HOSTED_LINUX_({define( {_EQULAYOUT_}, _no )})
_MODERN_({define({_RWFILE_},_yes)})
dnl Detectable error situations. Terminate.
_RWFD_({ _RWHD_({errprint({Mass storage cannot be at the same time from floppy and hard disk.
})m4exit(1000)})})
_BOOTED_({_HOSTED_LINUX_({errprint({Cannot boot into a Linux hosted Forth. 
})m4exit(1001)})})
_BOOTED_({ _HOSTED_MSDOS_({errprint({Cannot boot into a MSDOS hosted Forth. 
})m4exit(1002)})})
_BITS16_({ _BITS32_({errprint({32 and 16 bits at the same time? Choose one option!
})m4exit(1003)})})
_LINUX_C_({ _LINUX_N_( {errprint({C library and native calls at the same time? Choose one option!
})m4exit(1004)})})
_CLASSIC_({ _USEBIOS_( {errprint({Classic and BIOS I/O model are in conflict. Choose one option!
})m4exit(1005)})})
_CLASSIC_({ _MODERN_( {errprint({Classic and Modern I/O model are in conflict. Choose one option!
})m4exit(1006)})})
_USEBIOS_({ _MODERN_( {errprint({BIOS and Modern I/O model are in conflict. Choose one option!
})m4exit(1007)})})
dnl immediate consequences
_BITS32_({define({M4_CELLWIDTH},4)})
_BITS16_({define({M4_CELLWIDTH},2)})
_BITS16_({define({M4_INITDP},{TEXTEND})})
_BITS32_({define({_NEXT},{_NEXT32})})
_BITS32_({define({_PUSH},{_PUSH32})})
_BITS32_({define({_2PUSH},{_2PUSH32})})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
