dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
divert(-1)
define({quote},{{{$1}}})
define({forthvar},{@var{}quote($1)})
define({forthsamp},{@samp{}quote($1)})
define({forthcode},{@code{}quote($1)})
define({forthdefi},{@dfn{}quote($1)})
define({forthunderline},{$1})
define({forthxref},{@xref{}quote($1)})
define({forthpxref},{@pxref{}quote($1)})
define({forthfile},{{@file}})
define({forthitemize},{{@itemize}})
define({forthenditemize},{{@end itemize}})
define({forthenumerate},{{@enumerate}})
define({forthendenumerate},{{@end enumerate}})
define({forthquotation},{{@quotation}})
define({forthendquotation},{{@end quotation}})
define({forthitem},{{@item}})
define({forthbullet},{{@bullet}})
define({note},{$2})
dnl define({a},{{@a}}) define({enda},{{@end a}})
define({forthexample},
{@example 
@cartouche $1 
@end cartouche 
@end example})dnl

dnl Macros later we want some nice formatting
define({figforth},fig-Forth)
define({ciforth},ciForth)
define({DFW},D.F.W.)
divert(0)dnl
