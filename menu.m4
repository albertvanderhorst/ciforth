dnl $Id$
changequote({,})dnl
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{* $1 ::})dnl
@menu
divert(2)dnl
@end menu
divert(1)dnl So the actual menu appears between the @menu commands
