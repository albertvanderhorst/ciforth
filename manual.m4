dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
define({quote},{{{$1}}})
define({forthvar},{@var{}quote($1)})
define({forthsamp},{@samp{}quote($1)})
define({forthcode},{@code{}quote($1)})
define({forthexample},
{@example
@cartouche
{$1}
@end cartouche
@end example})dnl

