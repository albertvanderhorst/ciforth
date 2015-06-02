CIFORTH CUSTOMIZATION FOR LINUX

COPYRIGHT (c) 2000-2015 Albert van der Horst , THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.

Forth is a tool for evolutionary programming. This is the binary
distribution of lina 5.# . The following applies to all versions 5.#
of lina  in 32 or 64 bits.

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

The following applies to the 64 bit version, replace lina64
by lina32 everywhere. The symbolic link to lina may point to
either version.

Unpack in the directory where you want to use it by :
    tar fxz lina-5####.gz

Now you can use lina by :
    lina64
or
    lina64 -e

Print the manual (150 pages) by :
    lpr ci86.lina64.ps
or view it:
    gv ci86.lina64.ps

View the same information in ci86.lina64.pdf with an appropriate tool
or view by :
    info -f ci86.lina64.info

Viewing the file ``ci86.lina64.html'' with a html viewer,
gives a reference that is concise but has more cross links.

For customized installation see the -i option in the manual.

For system wide installation the following is recommended:
    su
    ./lina64 -g 60 lina64+
    ./lina64+ -i /usr/bin/lina64 /usr/lib/forth.lab
    ln -s /usr/bin/lina64 /usr/bin/lina
    chmod 755 /usr/bin/lina64
    chmod 644 /usr/lib/forth.lab

    mkdir /usr/share/doc/lina || true
    cp ci86.lina64.* /usr/share/doc/lina
    cp lina64.1      /usr/share/man/man1

(Growing lina by 60 Mbyte is primaryly useful for lina32,
lina64 has already an 8000 Mbyte dictionary space.

On info :
The "info" system for program documentation actively discourages
adding documentation to a system, by hiding how a .info file
can be installed. The nice folks of Debian had added a separate
program install-info , but as of 2015 that has been replaced by
a piece of junk that is much harder to use than reverse engineering
where to place an info file. 
that you are out of luck. 
Your only option is to do
   info -f <absolute-path>

$Id$
