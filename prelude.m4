dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)dnl
dnl
dnl This file contains the default version and mechanism for configuration
dnl for the generic figforth system.
dnl TO CHANGE THIS FILE FOR THE PURPOSE OF GENERATION
dnl A DIFFERENT VERSION OF FIGFORTH RESTRICT YOURSELF TO THE USE CHOICE'S PART
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
changecom{}dnl no braindamaged comment with #
dnl Indicate that a large part of conditional assembly ends here
define({_END_}, )dnl obsolete version
define({_END_}, )dnl preferred version
dnl _yes and  _no are expanded during definition time and generate
dnl define({aap},$1) or define({aap},)
define({_yes},{${}1})
define({_no},)dnl
dnl
dnl     DO NOT TOUCH THESE. INVISIBLE TO NORMAL CONFIGURATORS
dnl     Memory layout is defined using equ's
define( {_EQULAYOUT_}, _yes )dnl
dnl    Block buffer are allocated somewhere high
define( {_HIGH_BUF_}, _yes )dnl
dnl    Block buffer are allocated in the dictionary
define( {_LOW_BUF_}, _no)dnl
dnl    Booting directly into forth, from floppy or hard disk.
define({_BOOTED_},_no)dnl
dnl    Started by an operating system. 
define({_HOSTED_},_no)dnl
dnl    The 32 bit mode uses no paging for access to memory.
define({_DIRECTMAPPED_},_no)dnl
dnl    The code must be load at an absolute address.
define({_ABSOLUTELOAD_},_no)dnl
dnl Work around a deficiency in nasm : an ORG requires a numeric argument
define({M4_BIOSBOOT},{07C00H})
dnl Have code to switch ourselves to protected mode, e.g. after booting.
dnl Move forth up such thar ORG agrees with LOADADDRESS.
define( {_SWITCH_}, _no)dnl
dnl Have a normal return to MSDOS (without jumping to a CS-corrector)
define({_NORMAL_BYE_}, _no)
dnl Work on a PC, as a PC. Not Linux.
define({_PC_}, _no)
dnl Normally no code needed switch between modes.
define({JMPHERE_FROM_REAL},{})dnl
define({JMPHERE_FROM_PROT},{})dnl
dnl This is an experimental version, restricted to 255 blocks.
define({_FEWBLOCKS_}, _yes)
dnl Blocks are allocated in a file.
define({_BLOCKSINFILE_}, _no)
dnl
dnl ############## USER CHOICES #############################################
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Run the forth in real mode.
define( {_REAL_}, _no)dnl
dnl
dnl Run the forth in protected mode, in fact mucho invisible.
define( {_PROTECTED_}, _no)dnl
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl
dnl Run the forth in 16 bits mode
define( {_BITS16_}, _no)dnl
dnl
dnl Run the forth in 32bits (so protected) mode
dnl (must be in accordance with above).
define( {_BITS32_}, _no)dnl
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl    Combine those marked A only with A's from the next group,
dnl    same for B's
dnl
dnl A Use BIOS for I/O. No redirection but possible stand alone.
define( {_USEBIOS_}, _no)dnl
dnl A1 In addition choose either
dnl A1 Hard disk I/O
define({_RWHD_},_no)
dnl A1 Floppy disk I/O
define({_RWFD_},_no)
dnl A1 Blocks in files
define({_RWFILE_},_no)
dnl
dnl A Use DOS for I/O. Possibility of redirection.
dnl Rely partly on DOS for I/O. The original mishmesh, using
dnl the physical floppy lay out and some functions now declared obsolete.
define( {_CLASSIC_}, _no)dnl
dnl
dnl A File based, no obsolete MSDOS features.
define( {_MODERN_}, _no)dnl
dnl
dnl B Use LINUX for I/O. Possibility of redirection.
dnl File based.
define( {_LINUX_C_}, _no)dnl
dnl
dnl B Use LINUX for I/O. Possibility of redirection.
dnl File based.  No c involved.
define( {_LINUX_N_}, _no)dnl
dnl
dnl    CHOOSE ONE OF THE FOLLOWING
dnl     See remarks of previous group.
dnl
dnl A Boot sector present for use on PC. _USEBIOS_ & RWFD required.
dnl May run under MSDOS as well. Boot from floppy.
define( {_BOOTFD_}, _no)dnl
dnl
dnl A Boot sector present for use on PC. _USEBIOS_ & RWHD required.
dnl May run under MSDOS as well. Boot from hard disk.
define( {_BOOTHD_}, _no)dnl
dnl
dnl A Rely on MSDOS to start the program.
define( {_HOSTED_MSDOS_}, _no)dnl
dnl
dnl B Rely on LINUX to start the program.
define( {_HOSTED_LINUX_}, _no)dnl
dnl
dnl    FEATURES THAT STAND ON THEIR OWN, MOSTLY INDEPENDANT
dnl
dnl Keep the old debugging facility with place for breakpoints
define( {_OLDDEBUG_}, _no)dnl
dnl
dnl Keep the new debugging facility that allow to print IP (register SI)
define( {_NEWDEBUG_}, _no)dnl
dnl
dnl The end of memory, typically good for 16 bit. Redefine for 32 bit.
define({M4_EM},10000H)
dnl
dnl If M4_LOADADDRESS and M4_ORG are the same, FORTH address 0 is physical address 0.
dnl
dnl Applicable if 'SWITCH = _yes'
dnl The physical address where the code is loaded, i.e. agrees with ORG.
dnl For 'BOOTED = _yes' : this is naturally M4_BIOSBOOT
dnl For 'HOSTED_MSDOS = _yes' : it is made true by moving code.
define({M4_LOADADDRESS},M4_BIOSBOOT)dnl No quotes, must be numeric!
dnl
dnl Applicable if 'SWITCH = _yes'
dnl The position with respect to the start of the Forth code segment
dnl where the code is to be compiled.
define({M4_ORG},M4_BIOSBOOT)dnl No quotes, must be numeric!
dnl
dnl Applicable if 'SWITCH = _yes'
dnl The physical address where the switch segment starts.
dnl If this is equal to M4_LOADADDRESS , while M4_ORG is zero :
dnl   This maximizes the amount of code able to communicate with the BIOS
define({M4_SWITCHORG},M4_BIOSBOOT)dnl
dnl
dnl Retain information in the documentation that is redundant but illustrative.
define({_VERBOSE_},_yes)
dnl
dnl    FEATURES THAT NEED SELDOM CHANGES
dnl
dnl Where the dictionary starts for 32 bit systems.
dnl (Is automatically overwritten for 16 bits systems.
define({M4_INITDP},{110000H})

dnl ############## USER CHOICES END #########################################

dnl ############## STILL ON WISH LIST ################## IGNORE ##############
dnl
dnl The fields in the dictionary headers are aligned to a cell boundary.
dnl STILL ON WISH LIST
define( {_ALIGNED_}, _no)dnl
dnl
dnl Keep old FIG features like WIDTH ENCLOSE
dnl STILL ON WISH LIST
define( {_OLDFIG_}, _no)dnl
dnl
dnl Put all ANSI CORE definitions in, even if they could be loaded
dnl STILL ON WISH LIST
define( {_CORE_}, _no)dnl
dnl
dnl If undefined, a really minimalistic system results.
dnl STILL ON WISH LIST
define( {_MAXIM_}, _no)dnl
dnl
dnl To strip even further use NODOUBLE instead of DOUBLE (Not ANSI!)
dnl STILL ON WISH LIST
define( {_DOUBLE_}, _no)dnl
define( {_NODOUBLE_}, _no)dnl
dnl
dnl Put interrupts on overflows in. (Not ANSI!)
dnl In practice you limit your memory to under $7f00.0000.0000.0000
dnl (even less for <64 bit systems)
dnl STILL ON WISH LIST
define( {_SAFECALC_}, _no)dnl
dnl OTHER MISCELLANEOUS

dnl ############## GENERATION OF DOCUMENTATION ##############################
dnl The m4 system separates the doc and code onto different channels
dnl such that the output is code, separation message, documentation.

define({_SUPPRESSED}, )dnl
dnl Switch the system to generate assembler source
define( {_GENERATE_CODE}, 
{divert(1)dnl}
)

dnl Switch the system to generate documentation
define( {_GENERATE_DOC}, 
{divert(3)dnl}
)

dnl Switch the system to split off testinfo
dnl the real tests are further down the pipeline
define( {_GENERATE_TEST}, 
{divert(5)dnl}
)

dnl Redefine ``worddoc'' to make sure it lands in the documentation part
define({worddoc},
{_GENERATE_DOC{}dnl}
{{worddoc({$1},{$2},{$3},{$4},{$5},{$6},{$7})}}
{_GENERATE_TEST{}dnl}
{{wordtest({$2},{$8})}}
{_GENERATE_CODE{}dnl}
)dnl

define({worddocsafe},
{_GENERATE_DOC{}dnl}
{{worddocsafe({$1},{$2},{$3},{$4},{$5},{$6},{$7})}}
{_GENERATE_TEST{}dnl}
{{wordtest({$3},{$8})}}
{_GENERATE_CODE{}dnl}
)dnl

dnl Same for ``worddocchapter'' 
define({worddocchapter},
{_GENERATE_DOC{}dnl}
{{worddocchapter({$1},{ },{$3},{$4},{$5},{$6},{$7})}}
{{worddocchapterend({$1},{~~~~~~~~~~},{CONTENT},{},{},{},{})}}
{_GENERATE_CODE{}dnl}
)dnl

dnl m4 generates code on channel 1
divert(2)dnl
; Split here for documentation
dnl m4 generates documentation on 3
divert(4)dnl 
; Split here for test
dnl m4 generates tests on 5
divert(-1){}dnl

