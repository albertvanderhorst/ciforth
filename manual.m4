dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
divert(-1)
dnl The result must be @var{xxx} , the amount of braces around varies.
dnl This was a misinterpretation, forth variables are not meta
dnl variables, they are samples. Plus that stack diagrams are "samples"
dnl define({forthvar},{{@samp{$1}}})
define({forthvar},{{@var{$1}}})
define({forthfile},{@file{{$1}}})
define({forthsamp},{@samp{{$1}}})
define({forthmacro},{@samp{{$1}}})
define({forthprog},{@code{{$1}}})
define({forthcodeni},{@code{{$1}}})
define({forthcode},{@code{{$1}}
@findex $1
})
define({forthdefi},{@dfn{{$1}}
@cindex $1
})
define({forthunderline},{$1})
define({forthxref},{@xref{{$1}}})
define({forthpxref},{@pxref{{$1}}})
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
dnl be careful, an example always occupies whole lines.
define({forthexample},
{@example
@cartouche $1
@end cartouche
@end example})dnl
divert(0)dnl

