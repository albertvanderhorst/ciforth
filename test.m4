dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
dnl Split each wordtest() struct into tests (odd) and testresults (even)
dnl on different diversion.
divert(-1)dnl
changequote({,})dnl
define({forall2}, {ifelse(len({$2}),0,{dnl},
{$1}({{$2}},{{$3}}){forall2({$1},{$4},{$5},{$6},{$7},{$8},{$9},{$10})} )})dnl
dnl Define a words test test on channel 2
dnl   the corresponding outcome on channel 4
define({onetest},
{divert(2)dnl
$1 CR QUIT 
divert(4)dnl
$2
})dnl
define({wordtest},
{divert(2)dnl
." testing $1" QUIT
HEX : X ; CR QUIT
divert(4)dnl
testing $1
forall2({onetest},$2)
divert(2)dnl
FORGET X  QUIT})
divert(1)dnl
CR QUIT                                                                
divert(3)dnl
BYE
Split here for test
divert(5)dnl
divert{}dnl
