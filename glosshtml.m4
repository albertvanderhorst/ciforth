include(prelude.m4)dnl
include(postlude.m4)dnl
dnl Define a word definition on channel 9
dnl        the corresponding menu item on channel 6
dnl        and a definition for the second pass on channel 0
define({forthcode}, <A HREF="#$1">$1</A>)
define({worddoc},{
divert(9)dnl
<P>
<HR><A NAME="$2"></A>
<H1>
$2</H1>
<P>

STACKEFFECT: $4
<P>

DESCRIPTION: 
<P>
$6
<P>
divert(6)dnl
<LI>
<A HREF="#$2">$2</A></LI>
divert(9)dnl
})dnl
define({worddocsafe},{
divert(9)dnl
<P>
<HR><A NAME="$2"></A>
<H1>
$2</H1>
<P>

STACKEFFECT: $4
<P>

DESCRIPTION: 
<P>
$6
<P>
divert(6)dnl
<LI>
<A HREF="#$2">$2</A></LI>
divert(9)dnl
})dnl
define({worddocsafe2},{
divert(9)dnl
<P>
<HR><A NAME="$3"></A>
<H1>
$2</H1>
<P>

STACKEFFECT: $4
<P>

DESCRIPTION: 
<P>
$6
<P>
divert(6)dnl
<LI>
<A HREF="#$3">$2</A></LI>
divert(9)dnl
})dnl
<HTML>
<HEAD>
dnl<META HTTP-EQUIV="Content-Type" CONTENT="text/html"; charset="iso-8859-1">
   <TITLE>Fig-Forth 3.0 Manual</TITLE>
</HEAD>
<BODY>

<H1>
MENU </H1>

divert(5)dnl
Choose the word you want to know about:
<P>
divert(0)dnl
dnl Normal description comes after the definitions but before the menu's
dnl and contents.
