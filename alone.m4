dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
include(default.m4)
define( {_USEBIOS_1_}, _yes )dnl
define( {_BOOTFD__1_}, _yes )dnl       
define( {_PROTECTED_1_}, _yes )dnl       
define( {OPTIONAL_SWITCH_TO_32MODE}, )dnl       
dnl define( {OPTIONAL_SWITCH_TO_32MODE}, {include(width32.m4)})dnl       
include(header.m4)
include(protect.m4)
include(width16.m4)
divert{}dnl


