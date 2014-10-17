CIFORTH CUSTOMIZATION FOR OSX version 5.x.x.

************************************************************************
*  This is not a release, but an experimental version                  *
*  for the early adapters.                                             *
*                                                                      *
*                                                                      *
*  1. You have to build the executable yourself, using the             *
*     prescript contained in the nasm source.                          *
*     You'll have to install nasm first.                               *
*                                                                      *
*  2. Now you can test with the batch file testxina                    *
*                                                                      *
*     This should not give output. You or I may be at fault.           *
*                                                                      *
************************************************************************

COPYRIGHT (c) 2000-2011 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.
Apart from the GPL you have rights explicitly granted in the
document wina.pdf.

DESCRIPTION
Forth is a tool for evolutionary programming. This is the binary
distribution of xina 5.x.x, a beta release. Without notice this
description also applies to versions 4.0.x . xina is the OSX
native version of ciforth (common Intel Forth), an interpret
environment and compiler for Forth. Native is to be understood that
only system calls are used, no static link libraries or other
facilities.
It is (large and by) compliant with the ISO Forth standard,
the CORE wordset is fully implemented. The small, classic, indirect
threaded kernel contains the essential, i.a. file access and
exceptions. Its power is multiplied by an extensive source library,
that add i.a. a decompiler and integrated 386 assembler. It is fully
self contained; if you want to understand a compiler in all details,
this is your best, if not only, choice. ciforth sports a very high
documentation to binary ratio.

Unpack in the directory where you want to use it by :
    tar xf xina.tar

Install nasm, the well known net wide assembler.

Assemble and link using the instructions in the source.

You can not yet compile a program `monkey.frt' to an executable
`monkey.exe':
    xina -c monkey.frt

RELEASE CONTENT
Don't panic! ciforth is just two files.
The rest is documentation (examples and source).

COPYING         Copyright notice
READMExina.txt    This file
xina.asm        Source, for the ``nasm'' assembler.
NOT YET:        Forth interpreter compiler
xina.html       Usable reference documentation
xina.pdf        Documentation in Portable Data Format
forth.lab       Source library

The latest version can be fetched from
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html
This contains also the generic system that is recommended over
`xina.asm' if you want to make extensive change.

DOCUMENTATION I
The regular documentation is your choice of PostScript, PDF with
the same content.

DOCUMENTATION II
Viewing the file ``xina.HTML'' with a html viewer,
gives a reference extract but with more cross links.
This is only intended to be used while using the system, it
contains no information about installation or tutorial material.

INSTALLATION
TBS. This is a preliminary release.
Installation will remain manual:
A simple manual installation is superior to an automatic installation
that would never work across all the targeted platforms.
This Forth worked on windows version I never used or even heard of!
And of course even the simplest manual installation weeds out the wimps.
(Forth is not for wimps.)