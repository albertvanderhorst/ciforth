dnl  $Id$  M4 file to handle siwtching from to preotected mode
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
divert(-1)
define({CS_PROT},{CS_16})
define({DS_PROT},{DS_16})
define({IDENTIFY_PROT},{IDENTIFY_16})
dnl overwrite definitions for 32 bit mode
_BITS32_1_( {define({CS_PROT},{CS_32})})
_BITS32_1_( {define({DS_PROT},{DS_32})})
_BITS32_1_( {define({IDENTIFY_PROT},{IDENTIFY_32})})
define({TO_PROTECTED},{
         CLI
	 MOV EAX,CR0
	 INC AL  
	 MOV CR0,EAX            ;set protected mode
	JMPFAR
	DW      $+4
        DW      CS_PROT    
})dnl
define({TO_REAL},{
	JMPFAR
	DC      $+2+CW
        DW      CS_COMMON
	MOV EAX,CR0
	DEC AL  
	MOV CR0,EAX            ;set real mode
})dnl
define({JMPHERE_FROM_PROT},{
	TO_REAL
dnl The curly brackets prevent AX to be expanded to EAX
        BITS 16
        MOV     {AX},REALSEGMENT
        MOV     DS,{AX}
        MOV     ES,{AX}
        MOV     SS,{AX}
        STI
        ;_BITS32_1_({
        BITS 32})
})dnl
define({JMPHERE_FROM_REAL},{
	TO_PROTECTED    
        MOV     AX,DS_PROT
        MOV     DS,AX
        MOV     ES,AX
        MOV     SS,AX
})dnl     
divert{}dnl

