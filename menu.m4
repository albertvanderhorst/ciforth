dnl $Id: menu.m4,v 5.2 2005/06/16 15:54:17 albert Exp $
changequote({,})dnl
define({worddoc},{dnl})dnl ignore those
define({worddocsafe},{dnl})dnl ignore those
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{* $1 ::})dnl
@menu
divert(2)dnl
@end menu
divert(1)dnl So the actual menu appears between the @menu commands
