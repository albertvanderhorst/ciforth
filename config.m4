dnl
dnl We want usage like this
dnl for define(`_USEDOS______________' _yes)
dnl we would have this input
dnl ; ________________________________ _USEDOS______________________________( {
dnl anything but curls
dnl }; _END__________________________________________________________________
dnl And have output like this
dnl ; ________________________________ _USEDOS______________________________( 
dnl (OMMITTED)           
dnl ; _END__________________________________________________________________
dnl for define(`_USEDOS______________' _yes)
dnl This file should be included from masm.m4 linux.m4

dnl Change the quotes once and for all. 
dnl This means a separate pass (we do not want this!)
dnl mark here your preferences
dnl Note that there is no attempt to quote the paremeters.
dnl If you try to make Forth words with { } your coat is sold.
dnl
dnl Use DOS for I/O. Possibility of redirection.
define( `_USEDOS___', _no )dnl
dnl
dnl Use BIOS for I/O. No redirection but possible stand alone.
define( `_USEBIOS__', $1 )dnl       
dnl
dnl Keep old FIG features like WIDTH ENCLOSE
define( `_OLDFIG___', $1 )dnl       
dnl
dnl Put all ANSI CORE definitions in, even if they could be loaded
define( `_CORE_____', $1 )dnl
dnl
dnl If undefined, a really minimalistic system results.
define( `_MAXIM____', )dnl
dnl
dnl To strip even further use NODOUBLE instead of DOUBLE (Not ANSI!)
define( `_DOUBLE___', $1 )dnl
define( `_NODOUBLE_', )dnl
dnl
dnl Put interrupts on overflows in. (Not ANSI!)
dnl In practice you limit your memory to under $7f00.0000.0000.0000
dnl (even less for <64 bit systems) 
define( `_SAFECALC_', )dnl

dnl Once and for all. This is not intended.
changequote({,})dnl

