dnl $Id$
changequote({,})dnl
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{
divert(1)
{define({_prev$1},_previous})
{define({_next}{}_previous,$1})
{define({_previous},$1})
divert(2)
{define({wordsetnode_$1},{
@node $1, _next$1(),_prev$1(), Glossary
@section $1})}
})
divert(1)
{divert(-1)}
{define({_previous},)}
include(wordset.mig)
divert(1)
{define({_next}{}_previous,)}
divert(0)
undivert
{divert(0)}


