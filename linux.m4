dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
include(default.m4)
define( {_LINUXIO_}, _yes )dnl
define( {_LINUXHOSTED_}, _yes )dnl       
define({M4_ORIG},8050000H) 
define({M4_EM},(M4_ORIG+10000H)) 
define({M4_EM},(10000H)) 
dnl define( {_PROTECTED_}, _no )dnl       
define( {_EQULAYOUT_}, _no )dnl       
define( {_BITS32_}, _yes )dnl       
define(M4_CELLWIDTH,4)dnl       
dnl This stuf must go into postlude. Too disturbing for Joe user.
include(header.m4)
_PROTECTED_({include(protect.m4)})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
