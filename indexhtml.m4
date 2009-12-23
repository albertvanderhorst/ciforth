dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
define(divert,)
divert(-1)
changequote({,})dnl
dnl Define a word index on channel 2
dnl        the wordset index on channel 4
dnl        and a definition for the second pass on channel 0
define({forthvar}, {<B>$1</B>})
define({forthdefi}, {<I>$1</I>})
define({forthdefin}, {<I>$1</I>})
define({forthsamp}, {<B>$1</B>})
define({forthemph}, {<B>$1</B>})
define({forthkey}, {<B>$1</B>})
define({forthexample},{<P><B>$1</B><P>})
define({forthcode}, {<A HREF="#$1">$1</A>})
define({forthxref}, {<A HREF="#$1">See also $1</A>})
dnl ----------------------- worddoc ------------------------------------
define({worddoc},
{divert(2)dnl
forthcode({$2}) &nbsp; &nbsp; })dnl
dnl ----------------------- worddocsafe  ------------------------------------
define({worddocsafe},
{divert(2)dnl
forthcode({$2}) &nbsp; &nbsp; })dnl
dnl ----------------------- worddocchapter -----------------------------
define({worddocchapter},
{divert(4)dnl
<A HREF="#$1INDEX">$1</A> &nbsp; &nbsp; }) dnl
dnl ----------------------- worddocchapterend  -----------------------------
define({worddocchapterend},{dnl})dnl
include(namescooked.m4)
divert(0)dnl
<HTML>
<HEAD>
   <META http-equiv="content-type" content="text/html; charset=ISO-8859-1">
   <TITLE>thisforth ci86gnrversion Manual</TITLE>
</HEAD>
<BODY>

{<style type="text/css">
h1   { color: green; background: #CCFFCC; font-family: verdana; font: 24px; font-weight: bold; }
h2   { color: red; background: #CCFFCC; font-family: verdana; font: 20px; }
a    { color: blue; background: #CCFFCC; font-family: verdana; font: 18px; font-weight: bold; }
body { background: #CCFFCC; }
</style>}

<H1>
   thisforth Version [ ci86gnrversion ] GLOSSARY </H1>

divert(1)dnl
<P>
<HR><A NAME=GLOSSARY_INDEX></A>
<H1>
GLOSSARY INDEX</H1>
<P>
Here you can select the glossary entry of a
word. All words in the basic set are documented.
<P>
divert(3)dnl
<P>
<HR><A NAME=WORDSET_INDEX></A>
<H1>
GLOSSARY INDEX BY WORDSET</H1>
Here you can select the description of a
Wordsets: small sets of words that work together.
These are intended to give more insight in how to use the
words in cooperation.
<P>
Note: the first reference under `see also' of a word is the wordset to
which it belongs.
<P>
divert(0)dnl
dnl Normal description comes after the definitions but before the menu's
dnl and contents.
