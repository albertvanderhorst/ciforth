dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
define({forall}, {ifelse(len({$2}),0,,
{$1}({$2}) {forall({$1},{$3},{$4},{$5},{$6},{$7},{$8},{$9},{$10})}
)})dnl
dnl Define a word definition on channel 9
dnl        the corresponding menu item on channel 6
dnl        and a definition for the second pass on channel 0
define({forthvar}, <I>$1</I>)
define({forthsamp}, <B>$1</B>)
define({forthexample},<P><B>$1</B><P>)
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
ifelse(len({$7}),0,,{
<P>

SEE ALSO: forall({forthcode},$7)})
divert(6)dnl
<LI>
<A HREF="#$2">$2</A></LI>
divert(9)dnl
})dnl
<HTML>
<HEAD>
dnl<META HTTP-EQUIV="Content-Type" CONTENT="text/html"; charset="iso-8859-1">
   <TITLE>Fig-Forth 3.0 Manual</TITLE>
</HEAD>
<BODY>

<H1>
GLOSSARY</H1>

divert(5)dnl
</PRE>
Choose the word you want to know about:
<P>
divert(0)dnl
dnl Normal description comes after the definitions but before the menu's
dnl and contents.
<PRE>

