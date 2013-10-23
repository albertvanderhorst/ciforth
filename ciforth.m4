dnl $Id$
dnl Copyright(2013): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)dnl

Change the quotes once and for all.
This means a separate pass (we do not want this!)
Note that there is no attempt to quote the paremeters.
If you try to make Forth words with { } your coat is sold.

changequote({,})dnl
undefine({changequote})

Also this file makes some stupid features of m4 harmless.

We want to use shift as a normal word.
undefine({shift})dnl

No #. dnl is quite enough comment, thank you.
changecom
undefine({changecom})

Comment this in for deperado debugging.

dnl  debugfile(ci86.errm4)dnl
divert{}dnl
