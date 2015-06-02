CIFORTH CUSTOMIZATION FOR MS-Windows, DLL. version 5.2.x.

COPYRIGHT (c) 2000-2015 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.
Apart from the GPL you have rights explicitly granted in the
document wina.pdf.

DESCRIPTION
Forth is a tool for evolutionary programming. This is the binary
distribution of wina release 5.2.x.
wina is the Windows
native version of ciforth (common Intel Forth), an interpret
environment and compiler for Forth. Native is to be understood that
only kernel32.dll facilities are used, no other dll's, no other
facilities, and no registry.
There is a major difference with
releases 4.x.x : the interface with the operating system
is no longer DOS but via dll's. In particular this means that you
can import all libraries that are sufficiently documented at a
low level, in particular kernel.dll, and that long filenames are
no longer a problem.
wina is (large and by) compliant with the ISO Forth standard,
the CORE wordset is fully implemented. The small, classic, indirect
threaded kernel contains the essential, i.a. file access and
exceptions. Its power is multiplied by an extensive source library,
that add i.a. a decompiler and integrated 386 assembler. It is fully
self contained; if you want to understand a compiler in all details,
this is your best, if not only, choice. ciforth sports a very high
documentation to binary ratio.

Unpack in the directory where you want to use it by :
    pkunzip wna5d94beta.zip

Now you can use wina interactively by :
    wina

You can compile a program `hellow.frt' to an executable `hellow.exe':
    wina -c hellow.frt

The Forth word defined latest in `hellow.frt' is the entry point.
The library contains facilities to access arguments passed to
a program.

RELEASE CONTENT
Don't panic! ciforth is just two files, binary and library.
The rest is documentation (plus examples and source).

 COPYING         Copyright notice
 READMEwina.txt    This file
 wina##          The Forth interpreter
 forth.lab       Source library, a text file
 wina.fas        Source, for the ``fasm'' assembler
 wina.exe        Forth interpreter compiler
 wina.texinfo    Documentation master source
 wina.pdf        Documentation in Portable Data Format
 wina.ps         Documentation in PostScript
 wina.html       Browsable, reference documentation only
 hellow.frt      Compilation example : hello world.
 wc.script       Script example: word count 1)

1) Adapt the name of the interpreter.

The latest version can be fetched from
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html
This contains also the generic system that is recommended over
`wina.s' if you want to make extensive change.

DOCUMENTATION I
The regular documentation is your choice of PostScript, PDF with
the same content.

DOCUMENTATION II
Viewing the file ``wina.html'' with a html viewer,
gives a reference extract but with more cross links.
This is only intended to be used while using the system, it
contains no information about installation or tutorial material.

INSTALLATION
After unpacking wina is ready to use, because the library sits in
the current directory as configured.

Installation to a systemwide directory is easy :
wina -i C:\tools\wina.exe C:\tools\wina.lab %COMSPEC%
If C:\tools is in the PATH , wina now is usable from anywhere.

On a XP system it is recommended  to do :
wina -i C:\WINDOWS\system32\wina.exe c:\WINDOWS\system32\wina.lab c:\WINDOWS\system32\cmd.exe
The executable contains its own configuration information.
All this is explained in detail in the documentation.

If commands like DIR don't work (typically file not found errors),
the third parameter, which is the command interpreter, is wrong.
It has to agree with the variable ComSpec that can be inspected
with the SET command.

A simple manual installation is superior to an automatic installation
that would never work across all the targeted platforms.
This Forth worked on windows version I never used or even heard of!
And of course even the simplest manual installation weeds out the wimps.
(Forth is not for wimps.)

NOTE
The source file wina.fas can be assembled by fasm on a Linux or Apple
system, maybe even on MS-Windows. Instructions can be found in the source.
