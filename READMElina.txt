CIFORTH CUSTOMIZATION FOR LINUX

COPYRIGHT (c) 2000-2024 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.

RELEASE CONTENT
Don't panic! ciforth is just two files, binary and library.
The rest is documentation (plus examples and source).
## maybe 32 or 64.

 COPYING         Copyright notice
 READMElina.txt  This file
 lina32 / lina64 The Forth interpreter compiler
 forth.lab       Source library, a text file
 lina##.texinfo  Documentation master source
 lina##.ps       Documentation in PostScript format
 lina##.pdf      Documentation in Portable Data Format
 lina##.info     Documentation in info format
 lina##.html     Browsable, reference documentation only
 lina##.1        man page: options and overview
 ci86.lina##.fas Source in fasm format
      or
 ci86.lina##.s   Source in gas format (alternative)
     or
 ci86.lina##.asm Source in asm format (alternative)
 hellow.frt      Compilation example : hello world.
 mywc            Script example, old style.
 wc.script       Script example, new style.


UNPACK AND RUN
Unpack in the directory where you want to use it by :
    tar xfz ci86.lina##-#.#.#.gz

Now you can use lina by :
    lina##
or
    lina## -e
or
    lina
after symbolic linking (see installation).

Print the manual (150 pages) by :
    lpr ci86.lina##.ps
or view it:
    gv ci86.lina##.ps
or view by :
    info -f ./ci86.lina##.info

View the same information in ci86.lina##.pdf with an appropriate tool

Viewing the file ``ci86.lina##.html'' with a html viewer,
gives a reference that is concise but has more cross links.

The executables, script's and hellow.frt run at once,
provided the current directory (.) is in your PATH.
After proper installation you can run lina and script's
everywhere.

INSTALLATION
For system wide installation (64-bit) the following is recommended:
    su
    ./lina64 -g 60 lina64+
    ./lina64+ -i /usr/bin/lina64 /usr/lib/forth.lab
    ln -s /usr/bin/lina64 /usr/bin/lina
    chmod 755 /usr/bin/lina64
    chmod 644 /usr/lib/forth.lab

    mkdir /usr/share/doc/lina || true
    cp ci86.lina64.* /usr/share/doc/lina
    cp lina.1      /usr/share/man/man1/lina.1
    cp ci86.lina64.info   /usr/share/info/lina.info

The above increases Forth's dictionary space with 60 Mb
(or beyond 4 Gbyte e.g. 8000 Mbyte. )
Installing lina32 is similar.
See also the -i option in the manual.

Once installed you can use lina :
    Compile the example program by : lina -c hellow.frt
    Try the script by : wc.script *.s *.txt *.html

Source package in debian format
If you are installing from a deb file, all files are placed in the
appropriate directories.

CHECK
If you run lina with -v you can check the version of the
executable and the library. They must agree.

MODIFICATION
You can rebuild lina from the assembler file, instruction are in its
source.
The latest version and OSX and MS-windows versions can be fetched from
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html
You have to change just one file, ci86.lina##.fas.

For making extensive changes, such as modifying the header fields or
making a version booting from a hard disk or adapting to a different
assembler, I recommend the compiler factory over modifying the
assembler file. It is present in
    https://github.com/albertvanderhorst/ciforth

PROMOTION
Forth is a tool for evolutionary programming. This is the binary
distribution of lina 5.# . The following applies to all versions 5.#
of lina  in 32 or 64 bits. Contrary to what you expect from me,
this README is common to lina32 and lina64, because there
are virtually no differences except cell size.
The default name is lina; this is the 64 bit version. The name
lina64 is only used if there is a lina32 version around.

lina is the Linux native (= c-less) version of ciforth (common Intel
Forth), an interpret environment and compiler for Forth. It is (large
and by) compliant with the ISO Forth standard; the CORE wordset is
fully implemented. The small, classic, indirect threaded kernel
contains the essential, i.a. file access and exceptions. Its power is
multiplied by an extensive source library, that add i.a. a decompiler
and integrated 386 assembler. It is fully self contained; if you want
to understand a compiler in all details, this is your best, if not
only, choice. ciforth sports the highest documentation to binary
proportion in the EDP industry, barring m4 on Coherent.

$Id: READMElina.txt,v 5.9 2024/04/21 16:22:40 albert Exp $
