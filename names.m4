dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl $Id$
divert(-1)
include(VERSION)
dnl Macros later we want some nice formatting
define({figforth},fig-Forth)
dnl define({ciforth},fig-Forth)
define({DFW},{{D.F.W.}})
define({_BITS_},_BITS16_({16})_BITS32_({32}))
define({_SEGMENT_},_PROTECTED_({selector}) _REAL_({descriptor}))
define({thisfilename},{ciforth})
divert(0)dnl
