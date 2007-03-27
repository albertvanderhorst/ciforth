dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
include(default.m4)
define( {_HOSTED_}, _yes )dnl
define( {_MODERN_}, _yes )dnl
define( {_REAL_}, _yes )dnl
define( {_BITS16_}, _yes )dnl
define(M4_CELLWIDTH,2)dnl
define(M4_EM,0x10000)dnl
dnl This stuf must go into postlude. Too disturbing for Joe user.
include(header.m4)
_PROTECTED_({include(protect.m4)})
divert
