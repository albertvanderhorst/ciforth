dnl $Id$
changequote({,})dnl
define({worddocchapterend},{dnl})dnl ignore those
define({worddocchapter},{
divert(1)
{define({prev$1},previous})
{define({next}{}previous,$1})
{define({previous},$1})
divert(2)
{define({wordsetnode_$1},{{
@node $1, next$1(),prev$1(), Glossary
@subsection $1}})}
})
divert(-1)
include(wordset.mig)
divert(0)
undivert
include(gloss.mi)


