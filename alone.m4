dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
include(default.m4)
define( {_USEBIOS_1_}, _yes )dnl
define( {_BOOTFD__1_}, _yes )dnl       
define( {_PROTECTED_1_}, _yes )dnl       
define( {_BITS16_1_}, _yes )dnl       
define(M4_CELLWIDTH,2)dnl       
define(M4_EM,10000H)dnl       
dnl This stuf must go into postlude. Too disturbing for Joe user.
include(header.m4)
_PROTECTED_1_({include(protect.m4)})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
