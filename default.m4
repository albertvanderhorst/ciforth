dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
dnl This file contains the default version and mechanism for configuration.
dnl YOU SHOULD NEVER NEED TO CHANGE THIS FILE. 
dnl The choices are put in a configuration file, such as alone.m4
dnl What is put here sets defaults.
dnl Make sure the choices are compatible, in particular :
dnl a bootable system must rely on BIOS not MSDOS
dnl a 32 bit system cannot be real mode
dnl (And of course activating 16 and 32 bit at the same time is really out.)
dnl
dnl Change the quotes once and for all. 
dnl This means a separate pass (we do not want this!)
dnl Note that there is no attempt to quote the paremeters.
dnl If you try to make Forth words with { } your coat is sold.
changequote({,})dnl
dnl We want usage like this
dnl for define({_USEDOS______________} _yes)
dnl we would have this input
dnl ; ________________________________ _USEDOS______________________________( {
dnl anything but curls
dnl }; _END__________________________________________________________________
dnl And have output like this
dnl ; ________________________________ _USEDOS______________________________( 
dnl (OMMITTED)           
dnl ; _END__________________________________________________________________
dnl for define({_USEDOS______________} _yes)
dnl This file should be included and later some definitions redefined.
dnl _1_ is later to be replace by 40 _.
define({_no}, 
{;CODE SUPPRESSED:})dnl
define({_END___1__},  
_______1__ {$1})dnl
define({_yes},{${}1})
dnl
dnl     DO NOT TOUCH THESE. INVISIBLE TO NORMAL CONFIGURATORS
define( {_EQULAYOUT_1_}, _yes )dnl       
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Run the forth in real mode.
define( {_REAL_1_}, _no({$0}) )dnl       
dnl
dnl Run the forth in protected mode.
define( {_PROTECTED_1_}, _no({$0}) )dnl       
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Run the forth in 16 bits mode, (must be in accordance with above).
define( {_BITS16_1_}, _no({$0}) )dnl       
dnl
dnl Run the forth in 32bits (so protected) mode.
define( {_BITS32_1_}, _no({$0}) )dnl       
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Use BIOS for I/O. No redirection but possible stand alone.
define( {_USEBIOS_1_}, _no({$0}) )dnl       
dnl
dnl Use DOS for I/O. Possibility of redirection.
define( {_USEDOS_1_}, _no({$0}) )dnl
dnl
dnl Use DOS for I/O. Possibility of redirection. 
dnl File based, no obsolete MSDOS features.
define( {_MODERN_1_}, _no({$0}) )dnl
dnl
dnl Use LINUX for I/O. Possibility of redirection. 
dnl File based.
define( {_LINUXIO_1_}, _no({$0}) )dnl
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Boot sector present for use on PC. _USEBIOS_ required.
dnl May run under MSDOS as well. Boot from floppy.
define( {_BOOTFD__1_}, _no({$0}) )dnl       
dnl
dnl Boot sector present for use on PC. _USEBIOS_ required.
dnl May run under MSDOS as well. Boot from hard disk.
define( {_BOOTHD__1_}, _no({$0}) )dnl       
dnl
dnl Use BIOS for I/O. No redirection but possible stand alone.
define( {_HOSTED__1_}, _no({$0}) )dnl       
dnl
dnl Use LINUX for I/O. Redirection possible.
define( {_LINUXHOSTED_1_}, _no({$0}) )dnl       
dnl
dnl    FEATURES THAT STAND ON THEIR OWN, MOSTLY INDEPENDANT
dnl
dnl Keep old FIG features like WIDTH ENCLOSE
define( {_OLDFIG__1_}, _no({$0}) )dnl       
dnl
dnl Put all ANSI CORE definitions in, even if they could be loaded
define( {_CORE____1_}, _no({$0}) )dnl
dnl
dnl If undefined, a really minimalistic system results.
define( {_MAXIM___1_}, _no({$0}) )dnl
dnl
dnl To strip even further use NODOUBLE instead of DOUBLE (Not ANSI!)
define( {_DOUBLE__1_}, _no({$0}) )dnl
define( {_NODOUBLE_1_}, _no({$0}) )dnl
dnl
dnl Put interrupts on overflows in. (Not ANSI!)
dnl In practice you limit your memory to under $7f00.0000.0000.0000
dnl (even less for <64 bit systems) 
define( {_SAFECALC_1_}, _no({$0}) )dnl


