dnl  $Id$  M4 file to handle siwtching from to preotected mode
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
define({TO_PROTECTED},{
         CLI
	 MOV EAX,CR0
	 INC AL  
	 MOV CR0,EAX            ;set protected mode
})dnl
define({TO_REAL},{
	 MOV EAX,CR0
	 DEC AL  
	 MOV CR0,EAX            ;set real mode
         STI
})dnl
define({JMPHERE_FROM_32},{
	JMPFAR
	DD      $+6
	DW      $1
})dnl
define({JMPHERE_FROM_16},{
	JMPFAR
	DW      $+4
	DW      $1
})dnl


