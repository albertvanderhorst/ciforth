dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
dnl Split each wordtest() struct into tests (odd) and testresults (even)
dnl on different diversion.
divert(-1)dnl
changequote({,})dnl
define({_T_},{-1})
define({_TO_},{1})
define({forall2}, {ifelse(len({$2}),0,{dnl},
{$1}({{$2}},{{$3}}){forall2({$1},{$4},{$5},{$6},{$7},{$8},{$9},{$10}dnl
,{$11},{$12},{$13},{$14},{$15},{$16},{$17},{$18},{$19},{$20} )} )})dnl
dnl Define a words test test on channel 2
dnl   the corresponding outcome on channel 4
define({onetest},
{divert(2)dnl
$1 CR
divert(4)dnl
$2
})dnl
define({wordtest},
{divert(2)dnl
." testing $1"
HEX : XX ; CR
divert(4)dnl
testing $1
forall2({onetest},$2)
S[ ]
divert(2)dnl
.S CR
FORGET XX  })
divert(1)dnl
CR
divert(3)dnl
BYE
Split here for test
divert(2)dnl
'NOOP 'OK 3 CELLS MOVE
divert(5)dnl
divert{}dnl
