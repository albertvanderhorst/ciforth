; $Log $
; $Id$  
  
 ORG  07c00h             ; This means we'll have to trim the
 			;    binary with dd or something. Well worth it.
 			;    After   as   and   objcopy -O binary   do
 			;    dd < a.out > whatever bs=512 skip=62
  
A_GDT             EQU     27C00H
A_IDT             EQU     2FC00H
 BITS 16
_start:
  
 cli	;
  
 mov si        ,gdt_entries  ; copy initial GDT descriptors to  x2000
 mov di        ,0008h  ;       skip and thus allocate entry 0
 mov    ax, A_GDT/10H
 mov ES,AX
 mov bx,di 
 add bx,0100h  
for_128_duals:                  ; move 256 bytes
 	mov  ax,[si] 
 	mov  [ES:Di],ax
 	add di,BYTE 2 
 	add si,BYTE 2 
 	cmp di,bx 
 jnz for_128_duals
 				;  assuming ds=0 
 				;  This must be true while booting, allthough nowhere
                                ;  guaranteed, but otherwise booting software would be difficult
                                ; to write.
  
 lgdt [initial_gdtr]              ; This is one reason for the .org 07c00
  
 mov ecx , cr0 
 inc cx
 mov cr0 ,ecx  ; protection is on, but we need to get into
 				; 32 bit segments. You have to branch to
 				; a new cs, mov won't do cs.
  
 				; hand assembled jmp sd1:cs32
 DB   066h                    ; we're still IN 16 bit
 DB   0eah
 DD   cs32
 DW   008h                    ; 8 is GDT index 1, our code seg. selector
  
cs32:
  
 BITS 32                 ; since we just jumped to a 32 bit segment
  
 mov eax,                010h  ; selector 010h is index 2, our 32 bit data
 mov ds,ax 
 mov ss,                 ax  ; This might help with the 32/16/32 stunts.
  
 mov esp,              09000h  ; first 64k of second meg is our stack
  
 ;
 ;# BUILD Interrupt Descriptor Table at A_IDT physical
 ;      256 interrupt gate descriptors that call sequential IIT
 ;      calls, which all call INTCALLER.
  
 mov ax,07003h 
 mov edi,A_IDT 
per_IDT_descriptor:                     ; Our first (example) entry is...
 	mov [edi],ax               ; 03 70       starting at A_IDT 
 	inc edi                        ; offset
 	inc edi
 	mov WORD [edi],008h 
 	inc edi                        ; 03 70         08 00
 	inc edi                        ;               pmode code sel.
 	mov LONG [edi],000008e00h  
 	inc edi                        ; 03 70 20 00   00 8e 00 00
 	inc edi                        ;               present    intrrgate
 	inc edi
 	inc edi
 	add eax, BYTE 8 
 	cmp  eax,07803h
 jnz per_IDT_descriptor
  
 lidt [initial_idtr]
  
 ;
 ;# BUILD Int Indirection Table at 07000h <-> 07800.h This is a bunch of
 ;      calls that INTCALLER can pop the return address of to find out
 ;      what int is happening.
  
 mov	ebx,0ffh  
 mov 	edi, 07000h , 
 mov	 eax,           (INTCALLER - 07008h )  ; first relative offset
 						; byte following
while_B:
 		; dest is cursor into IIT
 		; eax is current call offset
 		; ebx is an entry counter, and is extraneous
  
 	mov LONG    [edi],0e8909090h     ; e8 opcode for CALL rel32
 					; prefixed with NOPs. Note endianism.
 	add edi, BYTE 4   
 	mov    [edi] ,  eax           ; append 4 byte relative offset
 	add edi,BYTE 4 
 	sub eax, BYTE 8                        ; relative offset from next IIT call to
 					; INTCALLER will be 8 less.
 	dec ebx
 jnz while_B
  
 sti	;
  
HANG: nop  ;
  
 	mov   eax, [0b8060h] 
 	inc eax
 	inc eax
 	mov [ 0b8060h] ,eax 
 	int 040h 
  
 jmp short HANG ;#;#;#;#;#;
 ;#;#;#;#;#;#;#;#;#;#;#;#;#;#;#;#;
  
INTCALLER:              ; System code, so to speak.
  
 			; we are in the system 32-bit segments
  
 mov [06300h], eax 
 pop eax                        ; gotta pop the I.I.T. return value to calculate
 			;   which int we are.
  
  mov   eax,[0b8060h + 320] 
  inc eax
  inc eax
  mov [0b8060h +320],eax 
  
 DB   0eah                    ; far jmp to a suitable-for-real-mode cs
 DD small_code_segment
 DW   020H
small_code_segment:             ;h we are a 16-bit machine with no useful
 				;       segments.
  
 BITS 16
  
 mov  eax                       ,018h ; upgrade to an 8086
  
 mov  ds,ax 
 mov  ss,ax 
  
 mov ecx,cr0  
 dec cx
 mov  cr0                ,ecx ; turn off protection, losing the segments
 				;       again.
  
 DB   0eah 
 DW   rereal
 DW   00h                     ; far jmp to an 8086 cs, NOT a dscriptor
  
rereal:
  
 mov  ax               ,0b800h ; real mode demo code. This, I think, could be
 mov ds                  ,ax  ;   an 8086 int caller, which is
 mov ax,[6400h]
 sub ax                    ,BYTE 1 ; Left as an Excercize to the Reader.
 mov [1620],ax                  ;   HINT:  pushf
 mov [06400h],ax  
  
 mov ecx,cr0 
 inc ecx
 mov cr0,ecx 
  
 DB   0eah 
 DW   recs32
 DW   008h                    ; 8 is GDT index 1, our code seg. selector
  
recs32:
  
 BITS 32
  
 mov ax,010h 
  
 mov ds,eax  
 mov  ss,eax 
  
  mov   eax,[0b8060h + 640] 
  inc eax
  inc eax
  mov [0b8060h +640],eax 
  
 iret
  
 ;#;#;#;#;#;#;#; initialized DATA allocations      ;#;#;#;#;#;#;#;#;#;#
  
gdt_entries:
 	; The GDT copy routine skip-allocates index 0.
 	; We initialize from index 1 on. Index 1 = selector 8.
 	; This template is for typical segments, not gates and such.
  
         DW   0FFFFh  ; low dual of limit
 	DW   0       ; low dual of base
 	DB   0       ; bits 16<->23 of base
  
 				; The DFbyte, see also the DFnybble
 				;
 				; bit   on=     description       bit of 64
 				; 0     01h     accessed                40
 				; 1     02h     read/write              41
 				; 2     04h     expandup/conform        42
 				; 3     08h     executable?             43
 				; 4     010h    typical                 44
 				; 5     020h    DPL lo                  45
 				; 6     040h    DPL hi                  46
 				; 7     080h    present                 47
 	DB   09Ah    ; = present typical executable readable
  
 				; bit   on=     description       bit of 64
                                 ; 0    01h     limit hi nybble         48
                                 ; 1     02h             "               49
                                 ; 2     04h             "               50
                                 ; 3     08h             "               51
 				; The DFnybble(TM)
 				; 4     010h    AVL                     52
                                 ; 5     020h    0                       53
                                 ; 6     040h    Big/D-efault/X          54
 				; D-bit true = 32 bit addy/operands in pmode
                                 ; 7     080h    4k Granularity          55
 	DB   0CFh    ; = *4k Big   f-nybble
 	DB   000h    ; hi byte of base
  
 ; index 2, 32 bit data, selector val = 010,h 4 gig limit, 0 base
         DW   0FFFFh 
 	DW   0
 	DB   0
 	DB   092h    ; present typical writable
 	DB   0cfh    ; *4k  BIG  f-nybble
 	DB   000h
  
 ; index 3, 16 bit data, selector val = 018,h 64k limit, 0 base
         DW   0FFFFh  ; limit
         DW   0
         DB   0
         DB   092h    ; present typical writable
         DB   00h     ; (bytegrain) 0-max-nybble-of-limit
         DB   0
  
 ; index 4, 16 bit code, selector val = 020,h 64k lim/gran, 0000 base
 ; this is base 0 real-ish, for INTCALLER , ffff limit
  
         DW   0FFFFh  ; limit
         DW   0       ; this is physical
 	DB   00h     ;     this OR previous dual = 0000
 	DB   09ah    ; present typical executable readable
 	DB   00h     ; (smallgrain) (notBIG) 0-max-nybble-of-limit
         DB   0
  
 ;,.....................................................
  
 ;# 6 byte limits/pointers for IDTR & GDTR
  
initial_idtr:
 	DW  0800h            ; 2k, 256 * 8
 	DD A_IDT           ; gdt physical address
  
initial_gdtr:
 	DW  0400h            ; gdt limit
 	DD A_GDT             ; gdt physical address
  
 RESB 01feh-($-$$)
 DW  0AA55h   ; Claim to be a PEEEEEEE.CEE bootsector.
  
STAGE_II:
 ; get a floppy load on, put your OS here, and jmp here.
  
 ; Copyright 2000 Rick Hohensee
 ; References/keys: Intel 386 manuals, John Fine, GNU as.info, gazos
 ;      Ralf Brown's, Linux
 ; Janet_Reno.s
