dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
changequote({,})
dnl Define a word definition on channel 9 
dnl   the corresponding menu item on channel 6 
dnl   and the content including a definition for the second pass on channel 1
define({worddoccommon},{
divert(9)dnl
@node $8, next$3(), prev(), MyTop()
@subsection $8
@pindex $2

Name: $2 

ifelse($4,,
{No stackeffect},
{Stackeffect: $4})

Attributes: $5

Description: $6

    {define({prev},{$8})dnl}
    divert(1)dnl
{{{$8}})dnl
define({next$3},}dnl
    divert(6)dnl
* $2  :ifelse($9,,:,$9)
divert(9)dnl
})dnl
define({worddocchapter},
dnl Another macro call, resolved at yet a higher level
wordsetnode_$1() 
    {$2}
    {$3}
    {$4}
{{define({MyTop},{{$1}})dnl}}
dnl Leave the first half of the first definition (unused) 
{divert(1)dnl}
    {{
define({{_dummy_}},dnl}}
dnl Initialise menu channel 
dnl Normal description comes after the definitions but before the menu's 
dnl and contents.
{divert(6)dnl}
{{dnl
define({prev},{{$1}})dnl}}
{
@menu}
)
define({worddoc},
{worddoccommon({$1},{$2},{$3},{$4},{$5},{$6},{$7},{$2},)}
)dnl
define({worddocsafe},
{worddoccommon({$1},{$2},{$3},{$4},{$5},{$6},{$7},{$3},{$3})}
)dnl
define({worddocchapterend},
dnl Complete the last half of the last definition empty 
{divert(1)dnl}
    {{)dnl}}
dnl Close menu channel 
{divert(6)dnl}
{{@end menu}}
{divert}
{undivert}
)
{define({MyTop},{Glossary})dnl}


