dnl $Id$
changequote({,})dnl
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{
divert(1)
{define({prev$1},previous})
{define({next}{}previous,$1})
{define({previous},$1})
divert(2)
{define({wordsetnode_$1},{
@node $1, next$1(),prev$1(), Glossary
@section $1})}
})
divert(1)
{divert(-1)}
{define({previous},)}
include(wordset.mig)
divert(1)
{define({next}{}previous,)}
divert(0)
undivert
{divert(0)}


