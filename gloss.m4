include(prelude.m4)dnl
include(postlude.m4)dnl 
define({prev},{{MyTop()}})dnl
dnl Define a word definition on channel 9 
dnl        the corresponding menu item on channel 6 
dnl        and a definition for the second pass on channel 0 
define({worddoc},{
divert(9)dnl
@node $1, next$2(), prev(), MyTop()
@subsection $1

NAME: $1 

STACKEFFECT: $3

DESCRIPTION: $5

    define({prev},{$1})dnl
    divert(0)dnl
        {{$1})dnl
        define({next$2},}
divert(6)dnl
* $1  ::  
divert(9)dnl
})dnl
define({worddocsafe},{
divert(9)dnl
@node $2, next$2(), prev(), MyTop()
@subsection $2

NAME: $1 

STACKEFFECT: $3

DESCRIPTION: $5

    define({prev},{$2})dnl
    divert(0)dnl
        {{$2})dnl
        define({next$2},}
    divert(6)dnl
* $1  : $2  
divert(9)dnl
})dnl
dnl Leave the first half of the first definition (unused) 
divert(0)dnl
    {define({dummy},}
dnl Complete the last half of the last definition empty 
divert(2)dnl
    {)dnl}
dnl Initialise menu channel 
divert(6)dnl
@menu
dnl Close menu channel one higher such that it becomes at the end 
divert(7)dnl
@end menu
dnl Normal description comes after the definitions but before the menu's 
dnl and contents.
divert(2)dnl


