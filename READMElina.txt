CIFORTH CUSTOMIZATION FOR LINUX

COPYRIGHT (c) 2000-2022 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.

Forth is a tool for evolutionary programming. This is the binary
distribution of lina 5.# . The following applies to all versions 5.#
of lina  in 32 or 64 bits. Contrary to what you expect from me,
this documentation is common to lina32 and lina64, because there
are virtually no differences except cell size.

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

RELEASE CONTENT
Don't panic! ciforth is just two files, binary and library.
The rest is documentation (plus examples and source).

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

The script examples only runs once you have installed /usr/bin/lina.

You can rebuild lina from the assembler file, instruction are in this
source.
The latest version and OSX and MS-windows versions can be fetched from
    http://home.hccnet.nl/a.w.m.van.der.horst/ciforth.html
You have to change just one file, ci86.lina.fas.

For making extensive changes, such as modifying the header fields or
making a version booting from a hard disk or adapting to a different
assembler, I recomment the compiler factory over modifying the
assembler file. It is present in
    https://github.com/albertvanderhorst/ciforth

Unpack in the directory where you want to use it by :
    tar xfz ci86.lina##-#.#.#.gz

Now you can use lina by :
    lina##
or
    lina## -e
or
    lina
After linking with so:
ln -s /usr/bin/lina /usr/bin/lina64

Print the manual (150 pages) by :
    lpr ci86.lina##.ps
or view it:
    gv ci86.lina##.ps
or view by :
    info -f ./ci86.lina##.info

View the same information in ci86.lina##.pdf with an appropriate tool

Viewing the file ``ci86.lina##.html'' with a html viewer,
gives a reference that is concise but has more cross links.


For system wide installation (32-bit) the following is recommended:
    su
    ./lina32 -g 60 lina32+
    ./lina32+ -i /usr/bin/lina32 /usr/lib/forth.lab
    ln -s /usr/bin/lina32 /usr/bin/lina
    chmod 755 /usr/bin/lina32
    chmod 644 /usr/lib/forth.lab

    mkdir /usr/share/doc/lina || true
    cp ci86.lina32.* /usr/share/doc/lina
    cp lina32.1      /usr/share/man/man1

The above increases Forth's dictionary space from 1 to 61 Mb.
Installing lina64 is similar, but now you may want to grow
by 8000 or 128000 Mbyte.
See also the -i option in the manual.

Once installed you can use lina :
    Compile the example program by : lina -c hellow.frt
    Try the script by : wc.script *.s *.txt *.html

Source package in debian format
If you are installing from a deb file, all files are placed in the
appropriate directories. This solves the problem how to add the
.info file to the system, as this is difficult to do by hand.

NOTE ON "info"
The "info" system for program documentation actively discourages
adding documentation to a system, by hiding how a .info file
can be installed. The nice folks of Debian had added a separate
program install-info , but as of 2015 that has been replaced by
ginstall-info that reinstates the former unworkable situation
with info files.
In a pinch you can do
   info -f <absolute-path>

$Id: READMElina.txt,v 5.7 2022/03/12 12:42:15 albert Exp $
