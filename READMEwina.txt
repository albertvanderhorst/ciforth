CIFORTH CUSTOMIZATION FOR MS-Windows, DLL. version 5.4.x

COPYRIGHT (c) 2000-2024 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.
Apart from the GPL you have rights explicitly granted in the
document wina.pdf.

RELEASE CONTENT
Don't panic! ciforth is just two files, binary and library.
The rest is documentation (plus examples and source).
In the following ## means 64 or 32.

 COPYING         Copyright notice
 READMEwina.txt    This file
 wina##          The Forth interpreter
 forth.lab       Source library, a text file
 wina##.fas      Source, for the ``fasm'' assembler
 wina##.exe      Forth interpreter compiler
 wina##.texinfo  Documentation master source
 wina##.pdf      Documentation in Portable Data Format
 wina##.ps       Documentation in PostScript
 wina##.html     Browsable, reference documentation only
 hellow.frt      Compilation example : hello world.
 wc.script       Script example: word count 1)

1) Adapt the name of the interpreter.

UNPACK AND RUN
Unpack in the directory where you want to use it by :
    pkunzip wina64-5.5.#.zip

Now you can use wina interactively by :
    wina32
or
    wina64
or
   wina
after installation.

The executables run at once, if they are in the working directory.
After proper installation you can run wina and script's
everywhere.
You can compile a program `hellow.frt' to an executable `hellow.exe':
    wina64 -c hellow.frt
The Forth word defined latest in `hellow.frt' is the entry point.
The library contains facilities to access arguments passed to
a program.

DOCUMENTATION I
The regular documentation is your choice of PostScript, PDF with
the same content.

DOCUMENTATION II
Viewing the file ``wina##.html'' with a html viewer,
gives a reference extract but with more cross links.
This is only intended to be used while using the system, it
contains no information about installation or tutorial material.

INSTALLATION
(This is an excerpt of the information in wina##.pdf.)
After unpacking wina is ready to use, because the library is configured
as sitting in the current directory, no path.

Installation to a system wide directory is easy :
wina## -i C:\tools\wina##.exe C:\tools\forth.lab %COMSPEC%
If C:\tools is in the PATH , wina## now is usable from anywhere.

On a XP system it is recommended  to do :
wina## -i C:\WINDOWS\system32\wina##.exe c:\WINDOWS\system32\wina.lab c:\WINDOWS\system32\cmd.exe
The executable contains its own configuration information.
All this is explained in detail in the documentation.

The dictionary is fixed to 4 Megabyte. The -g (grow) options is not
yet available for Microsoft.

If commands like DIR don't work (typically file not found errors),
the third parameter, which is the command interpreter, is wrong.
It has to agree with the variable ComSpec that can be inspected
with the SET command.

A simple manual installation is superior to an automatic installation
that would never work across all the targeted platforms.
This Forth worked on windows version I never used or even heard of!
And of course even the simplest manual installation weeds out the wimps.
(Forth is not for wimps.)

CHECK
If you run lina with -v you can check the version of the
executable and the library. They must agree.

MODIFICATION
The source file wina.fas can be assembled by fasm on MS-Windows 32-bit
or 64-bit system, or fasmw. Instructions can be found in the source.
You have to change just one file, ci86.wina##.fas.

On a a Linux system fasm can be used, provided that besides the Linux
fasm version, the MS-Windows version of fasm must be decompressed
to borrow its include subdirectory.
The generic system can create the assembler file in gas, nasm or masm
format. Linking can be a problem.

The latest version and OSX and Linux versions can be fetched via
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html

For making extensive changes, such as modifying the header fields or
making a version booting from a hard disk or adapting to a different
assembler, I recommend the compiler factory over modifying the
assembler file. It is present in
    https://github.com/albertvanderhorst/ciforth

PROMOTION
Forth is a tool for evolutionary programming. This is the binary
distribution of wina release 5.#
wina is the Windows native version of ciforth (common Intel Forth), an
interpret environment and compiler for Forth. Native is to be
understood that only kernel32.dll facilities are used, no other dll's,
no other facilities, and no registry.
There is a major difference with releases 4.x.x : the interface with
the operating system is no longer DOS but via dll's. In particular
this means that you can import all libraries that are sufficiently
documented at a low level, in particular kernel.dll, and that long
filenames are no longer a problem.
wina is (large and by) compliant with the ISO Forth standard,
the CORE wordset is fully implemented. The small, classic, indirect
threaded kernel contains the essential, i.a. file access and
exceptions. Its power is multiplied by an extensive source library,
that add i.a. a decompiler and integrated 386 assembler. It is fully
self contained; if you want to understand a compiler in all details,
this is your best, if not only, choice. ciforth sports a very high
documentation to binary ratio.
The Forth is 64 or 32 bits, indicated by ## in the following.

$Id: READMEwina.txt,v 5.5 2024/04/21 16:22:40 albert Exp $
