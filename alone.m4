dnl $Id: alone.m4,v 5.2 2005/06/16 15:53:57 albert Exp $
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
include(default.m4)
define( {_USEBIOS_}, _yes )dnl
define( {_BOOTFD_}, _yes )dnl       
define( {_PROTECTED_}, _yes )dnl       
define( {_BITS16_}, _yes )dnl       
define(M4_CELLWIDTH,2)dnl       
define(M4_EM,0x10000)dnl       
dnl This stuf must go into postlude. Too disturbing for Joe user.
include(header.m4)
_PROTECTED_({include(protect.m4)})
dnl LEAVE THIS! THE BOOT CODE IS ALWAYS 16 BITS.
include(width16.m4)
divert{}dnl
