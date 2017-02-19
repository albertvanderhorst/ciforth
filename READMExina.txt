CIFORTH CUSTOMIZATION FOR OSX version 5.2.x.

COPYRIGHT (c) 2000-2017 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.
Apart from the GPL you have rights explicitly granted in the
document wina.pdf.

DESCRIPTION
Forth is a tool for evolutionary programming. This is the binary
distribution of xina release 5.2.x.
non-beta
xina is the OSX
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
xina is compatible with the a release 5.2.x for linux and windows dll.
For the moment however it is only a 32 bit version.

Unpack in the directory where you want to use it by :
    tar xf xina.tar

Now you can use wina interactively by :
    xina

You can compile a program `hellow.frt' to an executable `hellow.exe':
    xina -c hellow.frt


RELEASE CONTENT
Don't panic! ciforth is just two files.
The rest is documentation (examples and source).

COPYING         Copyright notice
READMExina.txt  This file
xina.asm        Source, for the ``nasm'' assembler.
xina            Forth interpreter compiler
xina.html       Usable reference documentation
xina.pdf        Documentation in Portable Data Format
hellow.frt      Compilation example : hello world.
forth.lab       Source library

The latest version and Linux and MS-Windows versions can be fetched from
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html
or
    https://github.com/albertvanderhorst/ciforth
(subdirectory releases).
This contains also the generic system that is recommended over
`xina.asm' if you want to make extensive change.

DOCUMENTATION I
The regular documentation is your choice of PostScript,
PDF with the same content.

DOCUMENTATION II
Viewing the file ``xina.html'' with a html viewer,
gives a reference extract but with more cross links.
This is only intended to be used while using the system, it
contains no information about installation or tutorial material.

INSTALLATION
Installation will remain manual:
A simple manual installation is superior to an automatic installation
that would never work across all the targeted platforms.
And of course even the simplest manual installation weeds out the wimps.
(Forth is not for wimps.)

You can use xina from the current directory. You can install it in the
path, e.g. in your own ~/bin directory using the -i option, that is
described in the manual. If you install it system wide, don't forget
to make it executable for the world and the library readable for the
world.

ASSEMBLING XINA
There is no need to assemble xina yourself, but you can if
you want to.

Install nasm, the well known net wide assembler.

Assemble and link using the instructions in the source.
