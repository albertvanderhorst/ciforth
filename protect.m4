dnl  $Id$  M4 file to handle siwtching from to preotected mode
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
define({CS_PROT},{CS_16})
define({DS_PROT},{DS_16})
define({IDENTIFY_PROT},{IDENTIFY_16})
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
        MOV     AX,REALSEGMENT
        MOV     SS,AX
        MOV     ES,AX
        MOV     SS,AX
})dnl
define({JMPHERE_FROM_PROT},{
	JMPFAR
	DC      $+2+CELLWIDTH
        DW      CODESEGMENT
	TO_REAL
})dnl
define({JMPHERE_FROM_REAL},{
	TO_PROTECTED    
	JMPFAR
	DW      $+4
        DW      REALSEGMENT
;        DW      CS_PROT    must become of course
        MOV     AX,DS_PROT
        MOV     DS,AX
        MOV     ES,AX
        MOV     SS,AX
})dnl     

