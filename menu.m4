dnl $Id$
changequote({,})dnl
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{* $1 ::})dnl
@menu
include(wordset.mig)dnl
@endmenu
