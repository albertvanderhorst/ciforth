

;              ciforth : a generic I86 ISO FORTH by HCC FIG

; $Id$
; Copyright (2000): Albert van der Horst by GNU Public License
;
;HCC FIG Holland : Hobby Computer Club, Forth Interest Group  Holland
        ;  66,106
 ;   ciforth $Revision$
;
; For the generic system (to generate ciforth in an other configuration than this one):
;     http://home.hccnet.nl/a.w.m.van.der.horst/ci86gnr.html
;
; If this is a configured assembly file, it should be accompanied with configured
; documentation (texinfo, ps, html.)
; WITHOUT THE DOCUMENTATION: GIVE UP! GET THE REAL THING!
; You have a configured system, if there are NO curly brackets on the next line.
;
;
; Configuration of this particular version:
; 32 -bits protected mode
; standalone
; Normally ciforth doesn't observe ISO >IN.
; Contains :
; (there may be no items here.)
;        Security words
;         Loadable words, i.e. all of ISO CORE, more than is needed
;           for a self contained kernel.
; A field in the header to point to source
;
 ;
; This is a NASM version of ciforth created by ``m4'' from the generic listing.
; It can be assembled using ``nasm'' obtainable via :
; Source: ftp://ftp.us.kernel.org/pub/software/devel/nasm/source/
; URL: http://www.cryogen.com/Nasm/

; This version can be assembled on a Linux system in behalf of a
;  standalone (booting) version by
;   nasm -fbin ciforth.asm -o ciforth.com
; For assembling on other systems where nasm is available see the
; documentation of nasm.
; See again the generic system manual for more information how to install
; booting versions.

%if 0
        A generic version of ISO FORTH for IBM type standard PC's by
                Albert van der Horst

                in cooperation with
                HCC Forth user group
                The Netherlands
                www.forth.hccnet.nl

              based on
              FIG-FORTH
   implemented by:  Charlie Krajewski
                    205 ( BIG ) Blue Rd.
                    Middletown, CT  06457

  The listing has been made possible by the
  prior work of:
               Thomas Newman, Hayward, Ca.

 : other_acknowledgements
         John_Cassidy
         Kim_Harris
         George_Flammer
         Robert_D._Villwock ;

 : for tools
         Richard M. Stallman
         Linus Torvalds

No one who programs with FORTH can afford to be without:
  "Starting Forth  by Leo Brodie" and "Thinking Forth by Leo Brodie".
   Both out of print.

This Forth is a descendant in the 300+ (RCS)- generations from fig-Forth.

For nostalgic reasons the following comment has never been removed:
   Although there is much to be said for typing in your own
   listing and getting it running, there is much to be said
   not typing in your own listing.  If you feel that 100+
   pages of plinking is nutty, contact me for availability
   of a disc with source & executable files.  Obtainable at
   a bargain basement price, prepare yourself for bargain
   basement support.

All publications of the FORTH Interest Group are public domain.
They may be further distributed by the inclusion of this
credit notice:
               This publication has been made available by:

               FORTH Interest Group
               P.O. Box 1105
               San Carlos, Ca.  94070
[I feel obliged to keep this last one in (AH). Note that although it is
based on fig-Forth no stone is left unturned.]
%endif
        ;
; ########################################################################################
;                       PREPARATION (no code)
; ########################################################################################
FIGREL  EQU     4       ; FIG RELEASE #
FIGREV  EQU     0       ; FIG REVISION #
USRVER  EQU     0      ; USER VERSION NUMBER, a digit now
;
;      VERY ELEMENTARY .
CW      EQU     4    ; Size of a cell in Forth, not in the bootcode.
ERRORSCREEN EQU     48    ; Screen where the error messages start.
;
;      MEMORY LAYOUT.
; Normally this is specified at the m4 configuration level.
; For a configured system these values can be changed at this single place.
NBUF    EQU     8    ; No. of buffers, or screens
KBBUF   EQU     1024      ; Data bytes per disk buffer
US      EQU     40H*CW  ; User variable space
EM      EQU     400000H     ; Where the memory ends w.r.t. ORIG.
EMP     EQU     (EM-1)/1000H+1 ; Number of pages.
RTS     EQU     10000H    ; Return stack & terminal input buffer
;

;
;      ASCII CHARACTER EQUIVALENTS
;
ABL     EQU     ' '     ; SPACE
ACR     EQU     0DH     ; CR
ASO     EQU     '['     ; SQUARE BRACKET OPEN
ASC     EQU     ']'     ; SQUARE BRACKET CLOSE
ADOT    EQU     '.'     ; PERIOD
ALF      EQU     0AH     ; LINE FEED, USED INTERNALLY AS
                        ; LINE ENDER
AFF      EQU     0CH     ; FORM FEED
BELL    EQU     07H     ; ^G
BSIN    EQU     08H     ; INPUT DELETE CHARACTER
BSOUT   EQU     08H     ; OUTPUT BACKSPACE ( ^H )
;
;      HEADER RELATED EQUATES
B_DUMMY   EQU     01H     ; dea is dummy, from vocabulary link
B_INVIS   EQU     02H     ; dea is invisible, "smudged".
B_IMMED   EQU     04H     ; dea is a immediate.
B_DENOT   EQU     08H     ; dea is a denotation.
C_HOFFSET EQU     0       ; Offsets of code field in cells, w.r.t. dea
D_HOFFSET EQU     1       ; Same for data field
F_HOFFSET EQU     2       ; Same for flag field
L_HOFFSET EQU     3       ; Same for link field
N_HOFFSET EQU     4       ; Same for name field
S_HOFFSET EQU     5       ; Same for source field
PH_OFFSET EQU     6   ; Past header field: Start of data area.
BD_OFFSET EQU     6+1 ; Start of BODY for CREATEd word.
;

;

STRUSA  EQU     EM-US         ; User area at end
;

STRTIB  EQU     STRUSA-RTS      ; Start return stack area
                                ; Under this : data stack
INITR0  EQU     STRUSA         ; Grows down
INITS0  EQU     STRTIB          ; Grows down
 ;

;


DRIVE   EQU     0       ; Use floppy A for blocks.
%if 0
DRIVE   EQU     80H     ; Use drive C for blocks.
%endif
;

BPS     EQU     512             ;Bytes/sector, common to all of MSDOS
SPB     EQU     KBBUF/BPS
;
;


; PHYSICAL DISK PARAMETERS
;
; Disk parameters:
; FD drive 3"
TRKS    EQU     80    ;Number of tracks
SPT     EQU     18    ;Sectors/track
HEADS   EQU     2     ;Number of heads
NFAT    EQU     2     ; Number of FATS
SECROOT EQU     0EH   ; Sectors for root directory entry.
SECFAT  EQU     9     ; Sectors per FAT
MEDIA   EQU    0F0H   ; Descriptor byte. Used for selecting between A: and C:.

%if 0
; FD drive 5"
TRKS    EQU     80      ;Number of tracks
SPT     EQU     15      ;Sectors/track
HEADS   EQU     2       ;Number of heads
NFAT    EQU     2       ; Number of FATS
SECROOT EQU     ?       ; Sectors for root directory entry.
SECFAT  EQU     7       ; Sectors per FAT
MEDIA   EQU    0F0H     ; Descriptor byte. Used for selecting between A: and C:.
%endif

%if 0
; Hard drive
;This works supposedly with all reasonably modern drives.
TRKS    EQU     1       ;Number of tracks, dummy
SPT     EQU     63      ;Sectors/track
HEADS   EQU     255     ;Number of heads
NFAT    EQU     2       ; Number of FATS
SECROOT EQU     0       ; Sectors for root directory entry, dummy.
SECFAT  EQU     0       ; Sectors per FAT, dummy
MEDIA   EQU    0F8H     ; Descriptor byte. Used for selecting between A: and C:.
%endif

; Bios specific equates.
BOOTADDRESS     EQU     07C00H ; PC jumps to 0:7C00 to boot
        ; Skip boot sector,fats and root dir and first sector of file.
 ;

; The disk needs not to be recognized by MSDOS
; Usable for generating a bootable floppy simple.
; Always used for hard disk.
SECSTRT EQU     1
;
; END  OF PHYSICAL DISK PARAMETERS
;
;

;

; Segments   @ Valid in real mode % Valid in protected mode
; Names starting in A_ are linear, physical (32 bit), absolute addresses
RSTSIZE     EQU     10000H  ; For real mode stack.
GDTSIZE         EQU     8000H   ; For GDT-table.
IDTSIZE         EQU     0800H   ; For IDT-table.
A_FORTH0     EQU     07C00H - 07C00H ;  Physical address of Forth's CS:0 = SS:0 = ES:0 .

A_SWITCH        EQU     07C00H
A_RST       EQU     A_SWITCH + 10000H
A_GDT             EQU     A_RST + RSTSIZE
A_IDT             EQU     A_GDT + GDTSIZE
A_LOWDP         EQU     A_IDT + IDTSIZE

; @ Real mode place for the stack.
; This is such that after switching to real mode an isolated
; Stack is available
SS_RST      EQU     A_RST/10H ;
SWITCHSEGMENT     EQU     A_SWITCH/10H ; @ DS and CS for real code
; Add this to go from GDT_CS addresses to GDT_SWITCH addresses.
M4_SWITCHOFFSET EQU  ( A_FORTH0 - 07C00H)

; The GDT_.. are offsets in the GDT table. They can be arbitrarily chosen
; as far as the GDT goes as long as they are a multiple of 08H
; Switching sometimes restricts these to a particular value.
GDT_SWITCH       EQU    SWITCHSEGMENT  ; % Switching segment, must be same for switching to work!
GDT_CS EQU 10H  ; % The protected mode code segment
GDT_SS         EQU     SS_RST ; % The protected mode data segment
GDT_DS         EQU     SS_RST ; % The protected mode data segment
GDT_SEGMENT    EQU     A_GDT/10H       ; @ General descriptor table.
IDT_SEGMENT    EQU     A_IDT/10H       ; @ Interrupt descriptor table.

IDENTIFY_16 EQU 008FH   ; Identification of 16 bit data/code segment, byte 6
IDENTIFY_32 EQU 00CFH   ; Identification of 32 bit data/code segment, byte 6
IDENTIFY_INT EQU 8E00H  ; Identification of an interrupt descriptor, byte 5
IDENTIFY_XR  EQU 9A00H  ; Identification of a code segment, execute read, byte 5
IDENTIFY_RW  EQU 9200H  ; Identification of a data segment, read write, byte 5

GDTLEN EQU GDTSIZE-1      ; Intel peculiarity.
IDTLEN EQU IDTSIZE-1      ; Intel peculiarity.
BOOTOFFSET EQU 0
;

;

create  EQU     3C00H
open    EQU     3D00H
close   EQU     3E00H
read    EQU     3F00H
write   EQU     4000H
delete  EQU     4100H
lseek   EQU     4200H
;

;

; ########################################################################################
;                      BOOTCODE    (optional, always real mode)
; ########################################################################################

; All bootcode must be relocatable and its memory references absolute.
; Not for the sake of booting, but to allow MSDOS to start the program too.

        ;    SEGMENT PARA PUBLIC 'CODE'
        ; CS:;,DS:;,SS:;,ES:;
     ORG     07C00H



ORIG:
        JMP     SHORT BOOT
        NOP
        ; MSDOS programmers reference (thru 6, 3.9)
        DB    "DFW--EXP"
LBPS    DW         BPS
        DB         1
RESSECTORS  DW     01H
        DB         NFAT
        DW BPS*SECROOT/32
        DW         HEADS*TRKS*SPT    ; sectors/drive
LMEDIA  DB         MEDIA
        DB         SECFAT, 0H
LSPT    DW         SPT
LHEADS  DW       HEADS
HIDDENSECS    DD        0H
HUGESECS      DD         000H
      ; BIOS parameter block ends here
        DB 000H, 000H, 029H                  ; Required magic.
        DB         004H, 01CH, 040H,  00BH
        DB    "           "
        DB    "FAT12   "

;       Read the sector with number in CX (Counting from 0) to ES:BX.
;       Keep BX, CX
READSECTOR:
        PUSH    CX
        PUSH    BX
        MOV     AX,CX
        MOV     CL,[LSPT]
        DIV     CL
        MOV     CL,AH
        INC     CL      ; Sectors counting from 1!
        XOR     AH,AH   ; Get rid of remainder
        MOV     CH,[LHEADS]
        DIV     CH
        MOV     DH,AH   ; Head number
        MOV     CH,AL   ; Only small disks <256 cylinders
        MOV     DL,DRIVE
        MOV     AX,0201H   ; Read absolute one sector
        INT(13H)                 ;BIOS disk access.
        POP     BX
        POP     CX
        RET

RETRY:
        CALL    DISPLAYW
        XOR     AX,AX   ; Reset
;       MOV     DL,DRIVE
        INT(13H)                 ;BIOS disk access.
        CALL    DISPLAYW
        MOV     AL,' '
        CALL DISPLAY
        MOV     AX,CX
        CALL    DISPLAYW
        MOV     AL,' '
        CALL DISPLAY
BOOT:
        MOV     DL, 80H          ; Hard disk, default.
        MOV     AL, [LMEDIA]
        CMP     AL, 0F8H        ; Fixed disk.
        JZ     ENDIF1A
        MOV     DL, 0H          ; Floppy.
ENDIF1A:

        MOV     AL,'D'
        CALL    DISPLAY
        MOV AX,CS
        AND AX,AX       ; Z = BOOTING ?
        JZ   ENDIF1B
        JMP  NOBOOT
ENDIF1B:
        MOV     AL,'F'
        CALL    DISPLAY
        MOV     AH,00   ; Reset
;       MOV     DL,DRIVE
        INT(13H)                 ;BIOS disk access.
        JB      RETRY

        ; The first file copied to a freshly formatted floppy will
        ; be at SECSTRT (See also genboot.bat)
        MOV     CX,SECSTRT      ; Counting from zero
        MOV     AX,BOOTADDRESS/10H ; Bootsegment
        MOV     ES,AX
        MOV     BX,BPS
BEGIN1: CALL    READSECTOR
        INC     CX
        ADD     BX,BPS
        JB      RETRY
        CMP     BX,TEXTEND - $$
        JB      BEGIN1

        MOV     AL,'W'
        CALL    DISPLAY
        CALL    DISPLAYCR

        MOV     AL, [LHEADS]     ;Copy from boot sector to Forth.
        MOV     [LFHEADS],AL
        MOV     AL, [LSPT]
        MOV     [LFSPT],AL,
        MOV     AL, 0
        MOV     [LFDRIVE], DL

        JMP     ENDBOOT
 ;

;

; Debug code, could be dispensed with in an ideal world.
DISPLAYCR:
        MOV     AL,ACR
        CALL    DISPLAY
        MOV     AL,ALF
        JMP DISPLAY

DISPLAYPC:      POP     AX
        PUSH    AX
DISPLAYW:       PUSH    AX              ; Display AX in hex
        MOV     AL,AH
        CALL    DISPLAYHEX
        POP     AX
        ; CALL DISPLAYHEX ; RET
DISPLAYHEX:     PUSH    AX              ; Display AL in hex
        MOV     CL,4
        SAR     AL,CL
        CALL    DISPLAYHD
        POP     AX
        ; CALL DISPLAYHD ; RET
DISPLAYHD:      AND     AL,0FH          ; Display AL as one hex digit
        DAA
        MOV     AH,AL
        MOV     CL,5
        SHR     AH,CL
        ADC     AL,30H
        ; CALL DISPLAY ; RET
DISPLAY:XOR     BH,BH           ; Display AL as an ASCII char
        MOV     AH,0EH
        INT(10H)
        RET
GETKEY: MOV     AH, 01H       ; If CARRY, a key sits in AL.
        INT(16H)
        JNB     ENDIF1
        MOV     AH, 00H       ; Consume the key.
        INT(16H)
ENDIF1:  RET
;
NOBOOT:         ; Skip till here if not booting.


; Apparently we may have to move the code, e.g. if started from MSDOS.
; Prepare return to MSDOS using the original code segment.
        MOV     AX,CS
        MOV     DS,AX
        CALL    HERE1
HERE1:  POP     BX
        MOV     CX,BX
        ADD     BX,RETDOSV-HERE1+1        ; Independant of load address.
        ADD     CX,RETDOS-HERE1
        MOV     [BX],CX
        INC     BX
        INC     BX
        MOV     [BX],AX
        JMP ENDBOOT     ;

; Returns to DOS, provided we started from dos as a .COM.
; Use far jump restoring CS to .COM value.
RETDOS:
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        MOV     SS,AX
        MOV     AH,4CH
        INT     21H    ; Only works if cs is the same as while starting.

;
ENDBOOT:

; ########################################################################################
;                       ADJUST CODE SEGMENT REGISTER (still real mode)
; ########################################################################################
; Required start of .COM program.

; ########################################################################################
;                       MOVE CODE TO ITS PLACE (still real mode)
; ########################################################################################

;   Take care of the situation where booting code is actually started up by
;   MSDOS. This is no problem as long as the code is moved to where it would
;   be if booted. If the code is at its place, nothing really happens here.
;   Furthermore all protected code started by MSDOS must be at an absolute address.
        STD     ; Start at the end going back.
        MOV     CX,TEXTEND-HERE5 ; Amount to move
        CALL  HERE6
HERE6:  POP     AX                 ;  Calculate address of the first byte to move
        ADD     AX,TEXTEND-HERE6-1
        MOV     SI,AX           ; Relocatable address, w.r.t code segment.
        MOV     AX,CS
        MOV     DS,AX
        MOV     AX, A_FORTH0/10H ; Destination segment
        MOV     ES, AX
        MOV     DI, TEXTEND-1
        REPNZ
        MOVSB
        PUSH    ES    ; Corrected code segment
        MOV BX, HERE5
        PUSH BX     ; Correct program counter
        RETF        ; Returning to here5 now
HERE5:
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        MOV     SS,AX
        CLD     ; Reset direction to going up.

;
; ########################################################################################
;                       FILL GDT AND SWITCH TO PROTECTED MODE/32 BITS (optional)
; ########################################################################################

        JMP    PROTECT
GDTLOAD DW     GDTLEN
        DD     A_GDT
IDTLOAD DW     IDTLEN
        DD     A_IDT
PROTECT:
;Erase GDT_SEGMENT
        MOV     AX,GDT_SEGMENT
        MOV     ES,AX
        MOV     DI,0
        MOV     AL,0
        MOV     CX,GDTSIZE
        REP STOSB
;Copy addresses from interrupt area to IDT_SEGMENT
        MOV     AX,IDT_SEGMENT
        MOV     ES,AX
        MOV     CX,IDTSIZE/8
        MOV     SI,0
        MOV     DI,0
REP2:
        LODSD   ;{Interrupt vector.}
        STOSD
        MOV     EAX, IDENTIFY_INT
        STOSD
        DEC     CX
        JNZ     REP2
;Note STOSW uses ES:DI
        MOV     AX,GDT_SEGMENT ; GDT segment
        MOV     ES,AX
        MOV     DI,0
        MOV     AX,GDTLEN
        STOSW
; The switch segment.
; Switch between real and (16-bit) protected mode is done,
; while using this segment (Relocatable code only).
; GDT_SWITCH can to an extent be chosen arbitrarily,
; as long as here we ensure that the real mode address
; is equal to the protected mode address.
; You can only switch while staying at the same physical address
; when you are currently executing in the range GDT_SWITCH:[0:FFFFH]
        MOV     BX,GDT_SWITCH
        MOV     DI,BX
        MOV     AX,0FFFFH
        STOSW
        SHL     BX,4     ;  Turn segment register into IP
        MOV     AX,BX
        STOSW
        MOV     AX,IDENTIFY_XR
        STOSW
        MOV     AX,IDENTIFY_16
        STOSW
;
; GDT_DS/GDT_SS to an extent be chosen arbitrarily,
; The real mode view of GDT_SS is valid, isolated and reserved for real stack.
; DS is reset after switching anyway.
; Accommodate a 24 bit start address, a maximal limit, large pages.
        MOV     DI,GDT_SS ;Identical to GDT_DS
        MOV     AX,0FFFFH
        STOSW
        MOV     EAX,A_FORTH0
        STOSW           ; Only 16 bits
        SHR     EAX,8
        ADD     AX,IDENTIFY_RW
        STOSW
        MOV     AX,IDENTIFY_32
        STOSW
; PREPARE-CS 16/32 BITS
        MOV     DI,GDT_CS
        MOV     AX,0FFFFH
        STOSW
        MOV     EAX,A_FORTH0
        STOSW           ; Only 16 bits
        SHR     EAX,8
        ADD     AX,IDENTIFY_XR
        STOSW
        MOV     AX,IDENTIFY_32
        STOSW

        LGDT    [GDTLOAD]
;       LIDT    [IDTLOAD]
;

;

        JMP     ENDREADJUST
        RESB    01FEH-($-$$)
        ; Signature. Last piece of boot sector.
        DB         055H, 0AAH
ENDREADJUST:
;


; Remember: we are now in the real mode for a protected model.
; Make sure we are in the switch segment, such that we can switch.
        MOV   BX,CS      ; Reality.
        MOV AX, GDT_SWITCH ; Dream.
        PUSH AX     ; Correct code segment
        SUB AX,BX      ; Discrepancy between dream and reality
        MOV CX,10h     ; How much units would that be for the IP?
        CWD
        MUL CX
        CALL  HERE3
HERE3:  POP   BX         ; Reality.
        SUB BX,AX    ;
        ADD BX,THERE4-HERE3
        PUSH BX     ; Corrected program counter
        RETF        ; Returning to THERE4 now




; 32 bit protected mode is no good unless the A20 address line works.
; The following tedious code is copied from the nuni startup code
; for linux.
KB_WAIT:
        IN AL,64H
        AND AL,2
        JNZ KB_WAIT
        RET
THERE4:
        CALL KB_WAIT
        MOV AL,0D1H
        OUT 064H,AL            ; Enable a20
        CALL KB_WAIT
        MOV AL,0DFH
        OUT 060H,AL
 ;
;
; ########################################################################################
;                       PREPARE FOR USING DPMI (OPTIONAL)
; ########################################################################################
;
;

;


;************************
BITS   32         ; Assembler directive

;************************


        CLI     ; Wait for stacks to be setup.

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX
        MOV     EAX,GDT_SS
        MOV     SS,EAX
REP1:

        INC EAX                  ; Wait until a20 works(!)
        MOV [TESTVALUE],EAX
        CMP EAX,[100000H + TESTVALUE] ;  2^20 beyond
        JE REP1
        JMP PASTTEST
TESTVALUE: DD   0
PASTTEST:
 ;
;

; ########################################################################################
;                       FORTH GLUE CODE (optional, except for the jump)
; ########################################################################################

;

;
COLD_ENTRY:
        CLD                     ; DIR = INC

        MOV     AX,DS
        MOV     SS,AX           ;Atomic with next instruction.
        MOV     ESP, LONG[USINI+(CW*(2))]    ;PARAM. STACK
        MOV     EBP, LONG[USINI+(CW*(3))]    ;RETURN STACK
        MOV     ESI, CLD1  ; (IP) <-
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;
CLD1:   DD      COLD    ;  This is a piece of headerless high level code.
;
; ########################################################################################
;                       FORTH ITSELF (entry point : BOOTUP)
; ########################################################################################
;
%if 0
   FORTH REGISTERS
   The names under FORTH are used in the generic source.

   FORTH   8088     FORTH PRESERVATION RULES
   -----   ----     ----- ------------ -----
   HIP   ESI      High level Interpreter Pointer.  Must be preserved
                    across FORTH words.

   WOR   EAX      Working register.  When entering a word
                    via its code field the DEA is passed in WOR.

   SPO   ESP      Parameter stack pointer.  Must be preserved
                    across FORTH words.

   RPO   EBP      Return stack pointer.  Must be preserved across
                    FORTH words.

            EAX      General register.  Used to pass data from
                    FORTH words, see label APUSH or macro _APUSH

            EDX      General register.  Used to pass more data from
                    FORTH words, see label DPUSH or macro _DPUSH

            EBX      General purpose register.

            ECX      General purpose register.

            CS      Segment register. Must be preserved
                    across FORTH words.

            DS      ditto

            SS      ibid

            ES      Temporary segment register only used by
                    a few words. However it MUST remain equal to
                    DS, such that string primitives can be used
                    with impunity.

----------------------------------------------------------
%endif
        ;
%if 0
---------------------------------------------

   COMMENT CONVENTIONS
   ------- -----------

   =       IS EQUAL TO
   <-      ASSIGNMENT

  NAME        =  Address of name
  (NAME)      =  Contents of name

  CFA         =  CODE FIELD ADDRESS : a pointer to executable code
  DFA         =  DATA FIELD ADDRESS : a pointer to
                        data/high level code/ DOES> pointer
  FFA         =  FLAG FIELD ADDRESS: contains flags
  LFA         =  LINK FIELD ADDRESS: a pointer
  NFA         =  NAME FIELD ADDRESS: a pointer to a variable number of chars
  PHA         =  POST HEADER ADDRESS

  S1          =  Parameter stack - 1st cell
  S2          =  Parameter stack - 2nd cell
  R1          =  Return stack    - 1st cell
  R2          =  Return stack    - 2nd cell

  LSB         =  Least significant bit
  MSB         =  Most  significant bit
  LB          =  Low byte
  HB          =  High byte
  LW          =  Low  cell

------------------------------------------------------------
%endif
;
        ;

;
;
;
;
; In 32 bit versions there may be no jumps to NEXT at all
; The label NEXT1 is rarely relevant (for _OLDDEBUG_)
DPUSH:  PUSH    EDX      ; Fall through.
APUSH:  PUSH    EAX
NEXT:
;
        LODSD           ;AX <- (IP)
NEXT1:  MOV     EAX,EAX   ; (WOR) <- (IP)

        JMP      LONG[EAX]    ; TO `CFA'
;
;       Dictionary starts here.

DP0:
; Vocabularies all end in a link to 0.
; Only the word FORTH links to the DENOTATION wordlist,
; that in turn links to 0.


;  *********
;  *   '   *
;  *********
;
    ALIGN     4
N_TICK:   DD      1
        DB      "'"
    ALIGN     4
TICK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    0
        DD    N_TICK
    DD    0

        DD      ITICK
        DD      LITER
        DD      SEMIS
;

;  *********
;  *   &   *
;  *********
;
    ALIGN     4
N_DCHAR:   DD      1
        DB      "&"
    ALIGN     4
DCHAR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    TICK-(CW*(C_HOFFSET))
        DD    N_DCHAR
    DD    0

        DD      INBRS
        DD      SWAP, DROP
        DD      LDUP, QBL
        DD      LIT, 10, QERR
        DD      LITER
        DD      QDELIM
        DD      SEMIS
;

;  *********
;  *   ^   *
;  *********
;
    ALIGN     4
N_DCTL:   DD      1
        DB      "^"
    ALIGN     4
DCTL:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    DCHAR-(CW*(C_HOFFSET))
        DD    N_DCTL
    DD    0

        DD      INBRS
        DD      SWAP, DROP
        DD      LDUP, QBL
        DD      LIT, 10, QERR
        DD      LIT, '@', LSUB
        DD      LITER
        DD      QDELIM
        DD      SEMIS
;

;  *********
;  *   0   *
;  *********
;
    ALIGN     4
N_DEN0:   DD      1
        DB      "0"
    ALIGN     4
DEN0:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DCTL-(CW*(C_HOFFSET))
        DD    N_DEN0
    DD    0

;  *********
;  *   1   *
;  *********
;
    ALIGN     4
N_DEN1:   DD      1
        DB      "1"
    ALIGN     4
DEN1:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN0-(CW*(C_HOFFSET))
        DD    N_DEN1
    DD    0

;  *********
;  *   2   *
;  *********
;
    ALIGN     4
N_DEN2:   DD      1
        DB      "2"
    ALIGN     4
DEN2:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN1-(CW*(C_HOFFSET))
        DD    N_DEN2
    DD    0

;  *********
;  *   3   *
;  *********
;
    ALIGN     4
N_DEN3:   DD      1
        DB      "3"
    ALIGN     4
DEN3:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN2-(CW*(C_HOFFSET))
        DD    N_DEN3
    DD    0

;  *********
;  *   4   *
;  *********
;
    ALIGN     4
N_DEN4:   DD      1
        DB      "4"
    ALIGN     4
DEN4:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN3-(CW*(C_HOFFSET))
        DD    N_DEN4
    DD    0

;  *********
;  *   5   *
;  *********
;
    ALIGN     4
N_DEN5:   DD      1
        DB      "5"
    ALIGN     4
DEN5:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN4-(CW*(C_HOFFSET))
        DD    N_DEN5
    DD    0

;  *********
;  *   6   *
;  *********
;
    ALIGN     4
N_DEN6:   DD      1
        DB      "6"
    ALIGN     4
DEN6:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN5-(CW*(C_HOFFSET))
        DD    N_DEN6
    DD    0

;  *********
;  *   7   *
;  *********
;
    ALIGN     4
N_DEN7:   DD      1
        DB      "7"
    ALIGN     4
DEN7:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN6-(CW*(C_HOFFSET))
        DD    N_DEN7
    DD    0

;  *********
;  *   8   *
;  *********
;
    ALIGN     4
N_DEN8:   DD      1
        DB      "8"
    ALIGN     4
DEN8:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN7-(CW*(C_HOFFSET))
        DD    N_DEN8
    DD    0

;  *********
;  *   9   *
;  *********
;
    ALIGN     4
N_DEN9:   DD      1
        DB      "9"
    ALIGN     4
DEN9:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN8-(CW*(C_HOFFSET))
        DD    N_DEN9
    DD    0

;  *********
;  *   A   *
;  *********
;
    ALIGN     4
N_DENA:   DD      1
        DB      "A"
    ALIGN     4
DENA:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEN9-(CW*(C_HOFFSET))
        DD    N_DENA
    DD    0

;  *********
;  *   B   *
;  *********
;
    ALIGN     4
N_DENB:   DD      1
        DB      "B"
    ALIGN     4
DENB:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DENA-(CW*(C_HOFFSET))
        DD    N_DENB
    DD    0

;  *********
;  *   C   *
;  *********
;
    ALIGN     4
N_DENC:   DD      1
        DB      "C"
    ALIGN     4
DENC:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DENB-(CW*(C_HOFFSET))
        DD    N_DENC
    DD    0

;  *********
;  *   D   *
;  *********
;
    ALIGN     4
N_DEND:   DD      1
        DB      "D"
    ALIGN     4
DEND:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DENC-(CW*(C_HOFFSET))
        DD    N_DEND
    DD    0

;  *********
;  *   E   *
;  *********
;
    ALIGN     4
N_DENE:   DD      1
        DB      "E"
    ALIGN     4
DENE:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DEND-(CW*(C_HOFFSET))
        DD    N_DENE
    DD    0

;  *********
;  *   F   *
;  *********
;
    ALIGN     4
N_DENF:   DD      1
        DB      "F"
    ALIGN     4
DENF:        DD    DOCOL
        DD    LNUMB
        DD    B_IMMED + B_DENOT
        DD    DENE-(CW*(C_HOFFSET))
        DD    N_DENF
    DD    0

;

;  *********
;  *   -   *
;  *********
;
    ALIGN     4
N_DENM:   DD      1
        DB      "-"
    ALIGN     4
DENM:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    DENF-(CW*(C_HOFFSET))
        DD    N_DENM
    DD    0

        DD      PNUMB, DNEGA, SDLITE
        DD      SEMIS
;

;  *********
;  *   +   *
;  *********
;
    ALIGN     4
N_DENP:   DD      1
        DB      "+"
    ALIGN     4
DENP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    DENM-(CW*(C_HOFFSET))
        DD    N_DENP
    DD    0

        DD      PNUMB, SDLITE
        DD      SEMIS
;

;  *********
;  *   "   *
;  *********
;
    ALIGN     4
N_DENQ:   DD      1
        DB      '"'
    ALIGN     4
DENQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED + B_DENOT
        DD    DENP-(CW*(C_HOFFSET))
        DD    N_DENQ
    DD    0

        DD      LIT, SKIP, COMMA        ;  'SKIP , HERE >R 0 ,
        DD      HERE, TOR, ZERO, COMMA
DENQ1:
        DD      LIT, '"', PPARS         ;           BEGIN &" (PARSE)
        DD      INBRS, LDUP, LIT, '"', EQUAL ;           IN[] DUP &" =
        DD      ZBRAN
        DD      DENQ2-$-CW                 ;           WHILE
        DD      TDROP, ONEP             ;           2DROP 1+ R@ $+!
        DD      LDUP, ALLOT, RR, SADD
        DD      BRAN
        DD      DENQ1-$-CW                  ;           REPEAT
DENQ2:
        DD      QBL, ZEQU
        DD      LIT, 10, QERR           ;           ?BLANK 0= 5 ?ERROR
        DD      DROP                    ;                DROP R@ $+!
        DD      LDUP, ALLOT, RR, SADD
 DD LALIGN
        DD      FROMR, SFET, DLITE      ;           R> $@ POSTPONE DLITERAL ;
        DD      SEMIS
;

; The FORTH vocabulary is the only one not to link to zero.
; It links to the DENOTATION vocabulary.
;  *************
;  *   FORTH   *
;  *************
;
    ALIGN     4
N_FORTH:   DD      5
        DB      "FORTH"
    ALIGN     4
FORTH:        DD    DODOE
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DENOTBODY
        DD    N_FORTH
    DD    0

        DD      DOVOC
        DD      0       ; END OF VOCABULARY LIST

    ALIGN     4
        DD    0H
        DD    0
        DD    B_DUMMY
        DD    TASK-(CW*(C_HOFFSET))
        DD    0
    DD    0

;
;

;  ************
;  *   CORE   *
;  ************
;
    ALIGN     4
N_CORE:   DD      4
        DB      "CORE"
    ALIGN     4
CORE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    0
        DD    N_CORE
    DD    0

        DD      ZERO    ; Not (fully) present.
        DD      SEMIS
;

;  ***********
;  *   CPU   *
;  ***********
;
    ALIGN     4
N_LCPU:   DD      3
        DB      "CPU"
    ALIGN     4
LCPU:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CORE-(CW*(C_HOFFSET))
        DD    N_LCPU
    DD    0


       DD      LIT, 0CD1856H, ZERO       ; '80386'
           ; '80386'
;

        DD      SEMIS
;

;  ***************
;  *   VERSION   *
;  ***************
;
    ALIGN     4
N_LVERSION:   DD      7
        DB      "VERSION"
    ALIGN     4
LVERSION:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LCPU-(CW*(C_HOFFSET))
        DD    N_LVERSION
    DD    0

%if 0
;       If this is there it is an official release
        DD      SKIP
        DD      22
IBMPC:  DB      'IBM-PC ciforth'
        DB      FIGREL+40H,ADOT,FIGREV+30H,ADOT,USRVER+30H
        DD      LIT, IBMPC, LIT, 22
%endif
;       If M4_VERSION exists and contains a . it is an official release
        DD      SKIP
         DD      49
SB0: DB      "beta $RCSfile$ $Revision$"
       ALIGN     4
        DD      LIT, SB0
        DD      LIT, 49
        DD      SEMIS
;

;  ************
;  *   NAME   *
;  ************
;
    ALIGN     4
N_LNAME:   DD      4
        DB      "NAME"
    ALIGN     4
LNAME:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LVERSION-(CW*(C_HOFFSET))
        DD    N_LNAME
    DD    0

        DD      SKIP
         DD      7
SB1: DB      "ciforth"
       ALIGN     4
        DD      LIT, SB1
        DD      LIT, 7
        DD      SEMIS
;

;  ****************
;  *   SUPPLIER   *
;  ****************
;
    ALIGN     4
N_SUPPLIER:   DD      8
        DB      "SUPPLIER"
    ALIGN     4
SUPPLIER:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LNAME-(CW*(C_HOFFSET))
        DD    N_SUPPLIER
    DD    0

        DD      SKIP
         DD      20
SB2: DB      "Albert van der Horst"
       ALIGN     4
        DD      LIT, SB2
        DD      LIT, 20
        DD      SEMIS
;
;

;  ******************
;  *   DENOTATION   *
;  ******************
;
    ALIGN     4
N_DENOT:   DD      10
        DB      "DENOTATION"
    ALIGN     4
DENOT:        DD    DODOE
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    FORTH
        DD    N_DENOT
    DD    0

        DD      DOVOC
        DD      FORTH-(CW*(C_HOFFSET))     ; NEXT VOCABULARY
DENOTBODY:

    ALIGN     4
        DD    0H
        DD    0
        DD    B_DUMMY
        DD    DENQ-(CW*(C_HOFFSET))
        DD    0
    DD    0

;

;  *******************
;  *   ENVIRONMENT   *
;  *******************
;
    ALIGN     4
N_ENV:   DD      11
        DB      "ENVIRONMENT"
    ALIGN     4
ENV:        DD    DODOE
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DENOT-(CW*(C_HOFFSET))
        DD    N_ENV
    DD    0

        DD      DOVOC
        DD      DENOT-(CW*(C_HOFFSET))       ; NEXT VOCABULARY

    ALIGN     4
        DD    0H
        DD    0
        DD    B_DUMMY
        DD    SUPPLIER-(CW*(C_HOFFSET))
        DD    0
    DD    0

;

;  ************
;  *   NOOP   *
;  ************
;
    ALIGN     4
N_NOOP:   DD      4
        DB      "NOOP"
    ALIGN     4
NOOP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ENV-(CW*(C_HOFFSET))
        DD    N_NOOP
    DD    0

       LODSD                 ; NEXT
        JMP      LONG[EAX]
;
;
;

;  ***********
;  *   LIT   *
;  ***********
;
    ALIGN     4
N_LIT:   DD      3
        DB      "LIT"
    ALIGN     4
LIT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    NOOP-(CW*(C_HOFFSET))
        DD    N_LIT
    DD    0

        LODSD           ; AX <- LITERAL
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ; TO TOP OF STACK
;

;  ***************
;  *   EXECUTE   *
;  ***************
;
    ALIGN     4
N_EXEC:   DD      7
        DB      "EXECUTE"
    ALIGN     4
EXEC:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LIT-(CW*(C_HOFFSET))
        DD    N_EXEC
    DD    0

        POP     EAX      ; GET XT
        JMP      LONG[EAX + (CW*(C_HOFFSET))]  ;(IP) <- (PFA)
;


;  ***************
;  *   RECURSE   *
;  ***************
;
    ALIGN     4
N_RECURSE:   DD      7
        DB      "RECURSE"
    ALIGN     4
RECURSE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    EXEC-(CW*(C_HOFFSET))
        DD    N_RECURSE
    DD    0

        DD      LATEST, COMMA
        DD      SEMIS
;
;

;  **************
;  *   BRANCH   *
;  **************
;
    ALIGN     4
N_BRAN:   DD      6
        DB      "BRANCH"
    ALIGN     4
BRAN:        DD    (SKIP+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RECURSE-(CW*(C_HOFFSET))
        DD    N_BRAN
    DD    0

;

;  ************
;  *   SKIP   *
;  ************
;
    ALIGN     4
N_SKIP:   DD      4
        DB      "SKIP"
    ALIGN     4
SKIP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BRAN-(CW*(C_HOFFSET))
        DD    N_SKIP
    DD    0

BRAN1:  LODSD

        DEC     EAX
        OR      AL, 4 - 1
        INC     EAX
;
        ADD     ESI,EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   0BRANCH   *
;  ***************
;
    ALIGN     4
N_ZBRAN:   DD      7
        DB      "0BRANCH"
    ALIGN     4
ZBRAN:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SKIP-(CW*(C_HOFFSET))
        DD    N_ZBRAN
    DD    0

        POP     EAX      ; GET STACK VALUE
        OR      EAX,EAX   ; ZERO?
        JZ      BRAN1   ; YES, BRANCH
        LEA     ESI,[ESI+(CW*(1))]
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;
;

;  **************
;  *   (LOOP)   *
;  **************
;
    ALIGN     4
N_XLOOP:   DD      6
        DB      "(LOOP)"
    ALIGN     4
XLOOP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ZBRAN-(CW*(C_HOFFSET))
        DD    N_XLOOP
    DD    0

        MOV     EBX,1    ; INCREMENT
XLOO1:  ADD     [EBP],EBX ; INDEX = INDEX + INCR
        MOV     EAX,[EBP] ; GET NEW INDEX
        SUB     EAX,[EBP+(CW*(1))]        ; COMPARE WITH LIMIT
        XOR     EAX,EBX   ; TEST SIGN
        JS      BRAN1   ; KEEP LOOPING
;
;  END OF `DO' LOOP
        LEA     EBP,[EBP+(CW*(3))]  ; ADJ RETURN STACK
        LEA     ESI,[ESI+(CW*(1))]       ; BYPASS BRANCH OFFSET
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   (+LOOP)   *
;  ***************
;
    ALIGN     4
N_XPLOO:   DD      7
        DB      "(+LOOP)"
    ALIGN     4
XPLOO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    XLOOP-(CW*(C_HOFFSET))
        DD    N_XPLOO
    DD    0

        POP     EBX      ; GET LOOP VALUE
        JMP SHORT     XLOO1
        LODSD                 ; NEXT
        JMP      LONG[EAX]              ;Helpfull for disassembly.
;

;  ************
;  *   (DO)   *
;  ************
;
    ALIGN     4
N_XDO:   DD      4
        DB      "(DO)"
    ALIGN     4
XDO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    XPLOO-(CW*(C_HOFFSET))
        DD    N_XDO
    DD    0

        LODSD
        ADD     EAX,ESI  ;Make absolute
        POP     EDX      ; INITIAL INDEX VALUE
        POP     EBX      ; LIMIT VALUE
        XCHG    EBP,ESP   ; GET RETURN STACK
        PUSH    EAX      ; Target location.
        PUSH    EBX
        PUSH    EDX
        XCHG    EBP,ESP   ; GET PARAMETER STACK
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   (?DO)   *
;  *************
;
    ALIGN     4
N_XQDO:   DD      5
        DB      "(?DO)"
    ALIGN     4
XQDO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    XDO-(CW*(C_HOFFSET))
        DD    N_XQDO
    DD    0

        LODSD
        ADD     EAX,ESI  ;Make absolute
        POP     EDX      ; INITIAL INDEX VALUE
        POP     EBX      ; LIMIT VALUE
        CMP     EDX,EBX
        JZ      QXDO1
        XCHG    EBP,ESP   ; GET RETURN STACK
        PUSH    EAX      ; Target location.
        PUSH    EBX
        PUSH    EDX
        XCHG    EBP,ESP   ; GET PARAMETER STACK
        LODSD                 ; NEXT
        JMP      LONG[EAX]
QXDO1:  MOV     ESI,EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   I   *
;  *********
;
    ALIGN     4
N_IDO:   DD      1
        DB      "I"
    ALIGN     4
IDO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    XQDO-(CW*(C_HOFFSET))
        DD    N_IDO
    DD    0

        MOV     EAX,[EBP] ; GET INDEX VALUE
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ; TO PARAMETER STACK
;

;  *********
;  *   J   *
;  *********
;
    ALIGN     4
N_JDO:   DD      1
        DB      "J"
    ALIGN     4
JDO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    IDO-(CW*(C_HOFFSET))
        DD    N_JDO
    DD    0

        MOV     EAX,[EBP+(CW*(3))] ; GET INDEX VALUE
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ; TO PARAMETER STACK
;

;  **************
;  *   UNLOOP   *
;  **************
;
    ALIGN     4
N_UNLOOP:   DD      6
        DB      "UNLOOP"
    ALIGN     4
UNLOOP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    JDO-(CW*(C_HOFFSET))
        DD    N_UNLOOP
    DD    0

        DD      LIT, RDROP, COMMA
        DD      LIT, RDROP, COMMA
        DD      LIT, RDROP, COMMA
        DD      SEMIS
;

;  ***************
;  *   +ORIGIN   *
;  ***************
;
    ALIGN     4
N_PORIG:   DD      7
        DB      "+ORIGIN"
    ALIGN     4
PORIG:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    UNLOOP-(CW*(C_HOFFSET))
        DD    N_PORIG
    DD    0

        DD      LIT
        DD      USINI
        DD      PLUS
        DD      SEMIS
;
;      Initialisation block for user variables through DOC-LINK
;       <<<<< must be in same order as user variables >>>>>
;
;        DD      WARM_ENTRY FIXME
;        DD      COLD_ENTRY
USINI:  DD      STRUSA  ; User area currently in use, cold value same as next.
        DD      STRUSA  ; INIT (U0) user area of the main task 1
        DD      INITS0  ; INIT (S0)         2
        DD      INITR0  ; INIT (R0)         3
        DD      STRTIB  ; INIT (TIB)        4
        DD      BSIN    ; RUBOUT: get rid of latest char 5
        DD      0       ; AVAILABLE         6
        DD      1       ; INIT (WARNING)     7
        DD      INITDP  ;      INIT (FENCE)  8
DPA:    DD      INITDP  ;      INIT (DP)     9
        DD      ENV-(CW*(C_HOFFSET)) ;       INIT (VOC-LINK) 10
;
;
;


        DD      40H     ; INIT (OFFSET)
;
 ;

;
;
;
        DD      0, 0            ; WHERE             12 13
        DD      0, STRTIB       ;REMAINDER   14 15

; Swap the following with DP to allocate in conventional memory.
         DD     TEXTEND         ; LOW-DP        16
; Leave space to start conventional programs
         DD     80000H          ; LOW-EM        17
;
        RESB    US-($ - USINI)        ; All user can be initialised.
;
;      <<<<< end of data used by cold start >>>>>

;  *************
;  *   DIGIT   *
;  *************
;
    ALIGN     4
N_DIGIT:   DD      5
        DB      "DIGIT"
    ALIGN     4
DIGIT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PORIG-(CW*(C_HOFFSET))
        DD    N_DIGIT
    DD    0

        POP     EDX      ;NUMBER BASE
        POP     EAX      ;ASCII DIGIT
        SUB     AL,'0'
        JB      DIGI2   ;NUMBER ERROR
        CMP     AL,9
        JBE     DIGI1   ;NUMBER = 0 THRU 9
        SUB     AL,7
        CMP     AL,10   ;NUMBER 'A' THRU 'Z'?
        JB      DIGI2   ;NO
DIGI1:  CMP     AL,DL   ; COMPARE NUMBER TO BASE
        JAE     DIGI2   ;NUMBER ERROR
        SUB     EDX,EDX   ;ZERO
        MOV     DL,AL   ;NEW BINARY NUMBER
        MOV     AL,1    ;TRUE FLAG
        NEG     EAX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ;ADD TO STACK
;   NUMBER ERROR
DIGI2:  SUB     EAX,EAX   ;FALSE FLAG
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   (MATCH)   *
;  ***************
;
    ALIGN     4
N_PMATCH:   DD      7
        DB      "(MATCH)"
    ALIGN     4
PMATCH:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DIGIT-(CW*(C_HOFFSET))
        DD    N_PMATCH
    DD    0

        DD      TOR
        DD      RR, TFFA, FETCH
        DD      LIT, B_INVIS | B_DUMMY, LAND ;Get flags.
        DD      ZEQU
        DD      LDUP
        DD      ZBRAN
        DD      MATS2-$-CW
        DD      DROP
        DD      RR, TNFA, FETCH, FETCH
        DD      OVER, LSUB
;
; The following four lines take care of denotations.
        DD      LDUP, ZLESS  ;Ignorable length difference.
        DD      RR, TFFA, FETCH, LIT, B_DENOT, LAND ;Get flag.
        DD      LAND  ;Denotation applicable.
        DD      ZEQU, LAND ;This AND is actually an OR.
;
        DD      ZEQU
        DD      LDUP
        DD      ZBRAN
        DD      MATS2-$-CW
        DD      DROP, OVER
        DD      RR, TNFA, FETCH, SFET
        DD      CORA, ZEQU  ; Compare equals.
MATS2:  DD      FROMR, SWAP
        DD      SEMIS
;

;  **************
;  *   ?BLANK   *
;  **************
;
    ALIGN     4
N_QBL:   DD      6
        DB      "?BLANK"
    ALIGN     4
QBL:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PMATCH-(CW*(C_HOFFSET))
        DD    N_QBL
    DD    0

        DD      LBL, ONEP, LESS
        DD      SEMIS
;

;  ************
;  *   IN[]   *
;  ************
;
    ALIGN     4
N_INBRS:   DD      4
        DB      "IN[]"
    ALIGN     4
INBRS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QBL-(CW*(C_HOFFSET))
        DD    N_INBRS
    DD    0

        DD      SRC, CELLP, TFET
        DD      OVER, EQUAL
        DD      ZBRAN
        DD      INBRS1-$-CW
        DD      ZERO
        DD      BRAN
        DD      INBRS2-$-CW
INBRS1:
        DD      LDUP

        DD      CFET
        DD      ONE, LIN, PSTORE
INBRS2:
        DD      SEMIS
;

;  **************
;  *   (WORD)   *
;  **************
;
    ALIGN     4
N_LPWORD:   DD      6
        DB      "(WORD)"
    ALIGN     4
LPWORD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    INBRS-(CW*(C_HOFFSET))
        DD    N_LPWORD
    DD    0

        DD      X
PWORD0: DD      DROP
        DD      INBRS, QBL
        DD      OVER, SRC, CELLP, FETCH, LSUB ; At end?
        DD      LAND, ZEQU
        DD      ZBRAN
        DD      PWORD0-$-CW

        DD      X
PWORD1: DD      DROP
        DD      INBRS, QBL
        DD      ZBRAN
        DD      PWORD1-$-CW

        DD      OVER, LSUB

        DD      SEMIS
;

;  ***************
;  *   (PARSE)   *
;  ***************
;
    ALIGN     4
N_PPARS:   DD      7
        DB      "(PARSE)"
    ALIGN     4
PPARS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LPWORD-(CW*(C_HOFFSET))
        DD    N_PPARS
    DD    0

        DD      SRC, CELLP, TFET
        DD      OVER, LSUB

        DD      ROT, SSPLIT, TSWAP
        DD      ZEQU
        DD      ZBRAN
        DD      PPARS8-$-CW
        DD      DROP, SRC, CELLP, FETCH
PPARS8:
        DD      LIN, STORE
        DD SEMIS
;

;  ***********
;  *   SRC   *
;  ***********
;
    ALIGN     4
N_SRC:   DD      3
        DB      "SRC"
    ALIGN     4
SRC:        DD    DOUSE
        DD    (CW*(27))
        DD    0H
        DD    PPARS-(CW*(C_HOFFSET))
        DD    N_SRC
    DD    0
      ; And 28 and 29.
;


;  **************
;  *   SOURCE   *
;  **************
;
    ALIGN     4
N_SOURCE:   DD      6
        DB      "SOURCE"
    ALIGN     4
SOURCE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SRC-(CW*(C_HOFFSET))
        DD    N_SOURCE
    DD    0

        DD      SRC, FETCH
        DD      SRC, CELLP, FETCH
        DD      OVER, LSUB
        DD      SEMIS
;

;  ***********
;  *   >IN   *
;  ***********
;
    ALIGN     4
N_IIN:   DD      3
        DB      ">IN"
    ALIGN     4
IIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SOURCE-(CW*(C_HOFFSET))
        DD    N_IIN
    DD    0

        DD      LIN, FETCH
DD      SRC, FETCH, LSUB
        DD      PIIN, STORE
        DD      PIIN
        DD      SEMIS
;
;

;  **********
;  *   CR   *
;  **********
;
    ALIGN     4
N_CR:   DD      2
        DB      "CR"
    ALIGN     4
CR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    IIN-(CW*(C_HOFFSET))
        DD    N_CR
    DD    0

        DD      LIT,ALF
        DD      EMIT
        DD      ZERO, LOUT, STORE
        DD      SEMIS
;

;  *************
;  *   CMOVE   *
;  *************
;
    ALIGN     4
N_LCMOVE:   DD      5
        DB      "CMOVE"
    ALIGN     4
LCMOVE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CR-(CW*(C_HOFFSET))
        DD    N_LCMOVE
    DD    0

        CLD             ;direction
        MOV     EBX,ESI   ;save
        POP     ECX      ;count
        POP     EDI      ;dest
        POP     ESI      ;source
        REP     MOVSB
        MOV     ESI,EBX   ;get back
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   MOVE   *
;  ************
;
    ALIGN     4
N_LMOVE:   DD      4
        DB      "MOVE"
    ALIGN     4
LMOVE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LCMOVE-(CW*(C_HOFFSET))
        DD    N_LMOVE
    DD    0

        MOV     EBX,ESI   ;SAVE
        POP     ECX      ;count
        POP     EDI      ;dest
        POP     ESI      ;source
        CMP     ESI,EDI
        JC    MOVE1
        CLD             ;INC DIRECTION
        JMP SHORT MOVE2
MOVE1:  STD
        ADD     EDI,ECX
        DEC     EDI
        ADD     ESI,ECX
        DEC     ESI
MOVE2:
        REP     MOVSB   ;THAT'S THE MOVE
        CLD             ;INC DIRECTION
        MOV     ESI,EBX   ;GET BACK
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   FARMOVE   *
;  ***************
;
    ALIGN     4
N_FMOVE:   DD      7
        DB      "FARMOVE"
    ALIGN     4
FMOVE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LMOVE-(CW*(C_HOFFSET))
        DD    N_FMOVE
    DD    0

        CLD             ;direction
        MOV     EAX,ESI   ;save
        MOV     EBX,DS    ;save
        POP     ECX      ;count
        POP     EDI      ;dest
        POP     EDX
        AND     EDX,EDX
        JZ      FARMV1
        MOV     ES,EDX
FARMV1:
        POP     ESI      ;source
        POP     EDX
        PUSH    DS
        PUSH    EBX      ;ES in fact.
        AND     EDX,EDX
        JZ      FARMV2
        MOV     DS,EDX
FARMV2:
        REP     MOVSB
        MOV     ESI,EAX   ;restore
        POP     ES
        POP     DS
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   UM*   *
;  ***********
;
    ALIGN     4
N_USTAR:   DD      3
        DB      "UM*"
    ALIGN     4
USTAR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FMOVE-(CW*(C_HOFFSET))
        DD    N_USTAR
    DD    0

        POP     EAX
        POP     EBX
        MUL     EBX      ;UNSIGNED
        XCHG    EAX,EDX   ;AX NOW = MSW
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ;STORE DOUBLE CELL
;

;  **************
;  *   UM/MOD   *
;  **************
;
    ALIGN     4
N_USLAS:   DD      6
        DB      "UM/MOD"
    ALIGN     4
USLAS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    USTAR-(CW*(C_HOFFSET))
        DD    N_USLAS
    DD    0

        POP     EBX      ;DIVISOR
        POP     EDX      ;MSW OF DIVIDEND
        POP     EAX      ;LSW OF DIVIDEND
        DIV     EBX      ;16 BIT DIVIDE
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ;STORE QUOT/REM
;

;  ***********
;  *   AND   *
;  ***********
;
    ALIGN     4
N_LAND:   DD      3
        DB      "AND"
    ALIGN     4
LAND:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    USLAS-(CW*(C_HOFFSET))
        DD    N_LAND
    DD    0

        POP     EAX
        POP     EBX
        AND     EAX,EBX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   OR   *
;  **********
;
    ALIGN     4
N_LOR:   DD      2
        DB      "OR"
    ALIGN     4
LOR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LAND-(CW*(C_HOFFSET))
        DD    N_LOR
    DD    0

        POP     EAX      ; (S1) <- (S1) OR (S2)
        POP     EBX
        OR      EAX,EBX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   XOR   *
;  ***********
;
    ALIGN     4
N_LXOR:   DD      3
        DB      "XOR"
    ALIGN     4
LXOR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LOR-(CW*(C_HOFFSET))
        DD    N_LXOR
    DD    0

        POP     EAX      ; (S1) <- (S1) XOR (S2)
        POP     EBX
        XOR     EAX,EBX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **************
;  *   INVERT   *
;  **************
;
    ALIGN     4
N_INVERT:   DD      6
        DB      "INVERT"
    ALIGN     4
INVERT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LXOR-(CW*(C_HOFFSET))
        DD    N_INVERT
    DD    0

        POP     EAX      ; (S1) <- (S1) XOR (S2)
        NOT     EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   DSP@   *
;  ************
;
    ALIGN     4
N_SPFET:   DD      4
        DB      "DSP@"
    ALIGN     4
SPFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    INVERT-(CW*(C_HOFFSET))
        DD    N_SPFET
    DD    0

        MOV     EAX,ESP   ; (S1) <- (SP)
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   DSP!   *
;  ************
;
    ALIGN     4
N_SPSTO:   DD      4
        DB      "DSP!"
    ALIGN     4
SPSTO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SPFET-(CW*(C_HOFFSET))
        DD    N_SPSTO
    DD    0

        POP     EAX
        MOV     ESP,EAX        ;RESET PARAM STACK POINTER
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;


;  *************
;  *   DEPTH   *
;  *************
;
    ALIGN     4
N_DEPTH:   DD      5
        DB      "DEPTH"
    ALIGN     4
DEPTH:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SPSTO-(CW*(C_HOFFSET))
        DD    N_DEPTH
    DD    0

        DD      SZERO, FETCH
        DD      SPFET
        DD      LSUB
        DD      LIT, CW, SLASH
        DD      ONEM
        DD      SEMIS
;
;

;  ************
;  *   RSP@   *
;  ************
;
    ALIGN     4
N_RPFET:   DD      4
        DB      "RSP@"
    ALIGN     4
RPFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DEPTH-(CW*(C_HOFFSET))
        DD    N_RPFET
    DD    0
      ;(S1) <- (RP)
        PUSH    EBP
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   RSP!   *
;  ************
;
    ALIGN     4
N_RPSTO:   DD      4
        DB      "RSP!"
    ALIGN     4
RPSTO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RPFET-(CW*(C_HOFFSET))
        DD    N_RPSTO
    DD    0

        POP     EBP
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   EXIT   *
;  ************
;
    ALIGN     4
N_EXIT:   DD      4
        DB      "EXIT"
    ALIGN     4
EXIT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RPSTO-(CW*(C_HOFFSET))
        DD    N_EXIT
    DD    0

        MOV     ESI,[EBP] ;(IP) <- (R1)
        LEA     EBP,[EBP+(CW*(1))]
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   CO   *
;  **********
;
    ALIGN     4
N_CO:   DD      2
        DB      "CO"
    ALIGN     4
CO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    EXIT-(CW*(C_HOFFSET))
        DD    N_CO
    DD    0

        XCHG    ESI,[EBP]
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   (;)   *
;  ***********
;
    ALIGN     4
N_SEMIS:   DD      3
        DB      "(;)"
    ALIGN     4
SEMIS:        DD    (EXIT+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CO-(CW*(C_HOFFSET))
        DD    N_SEMIS
    DD    0

;

;  *************
;  *   LEAVE   *
;  *************
;
    ALIGN     4
N_LLEAV:   DD      5
        DB      "LEAVE"
    ALIGN     4
LLEAV:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SEMIS-(CW*(C_HOFFSET))
        DD    N_LLEAV
    DD    0
  ;LIMIT <- INDEX
        DD      RDROP, RDROP, RDROP
        DD      SEMIS
;

;  **********
;  *   >R   *
;  **********
;
    ALIGN     4
N_TOR:   DD      2
        DB      ">R"
    ALIGN     4
TOR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LLEAV-(CW*(C_HOFFSET))
        DD    N_TOR
    DD    0
        ; (R1) <- (S1)
        POP     EBX      ;GET STACK PARAMETER
        LEA     EBP,[EBP - (CW*(1))]    ;MOVE RETURN STACK DOWN
        MOV     [EBP],EBX ;ADD TO RETURN STACK
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   R>   *
;  **********
;
    ALIGN     4
N_FROMR:   DD      2
        DB      "R>"
    ALIGN     4
FROMR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TOR-(CW*(C_HOFFSET))
        DD    N_FROMR
    DD    0
      ;(S1) <- (R1)
        MOV     EAX,[EBP] ; GET RETURN STACK VALUE
        LEA     EBP,[EBP + (CW*(1))]
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   RDROP   *
;  *************
;
    ALIGN     4
N_RDROP:   DD      5
        DB      "RDROP"
    ALIGN     4
RDROP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FROMR-(CW*(C_HOFFSET))
        DD    N_RDROP
    DD    0
      ;(S1) <- (R1)
        LEA     EBP,[EBP+(CW*(1))]
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   R@   *
;  **********
;
    ALIGN     4
N_RR:   DD      2
        DB      "R@"
    ALIGN     4
RR:        DD    (IDO+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RDROP-(CW*(C_HOFFSET))
        DD    N_RR
    DD    0

;

;  **********
;  *   0=   *
;  **********
;
    ALIGN     4
N_ZEQU:   DD      2
        DB      "0="
    ALIGN     4
ZEQU:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RR-(CW*(C_HOFFSET))
        DD    N_ZEQU
    DD    0

        POP     EAX
        NEG     EAX
        CMC
        SBB     EAX,EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   0<   *
;  **********
;
    ALIGN     4
N_ZLESS:   DD      2
        DB      "0<"
    ALIGN     4
ZLESS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ZEQU-(CW*(C_HOFFSET))
        DD    N_ZLESS
    DD    0

        POP     EAX
        OR      EAX,EAX   ;SET FLAGS
        MOV     EAX,0    ;FALSE
        JNS     ZLESS1
        DEC     EAX      ;TRUE
ZLESS1: PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   +   *
;  *********
;
    ALIGN     4
N_PLUS:   DD      1
        DB      "+"
    ALIGN     4
PLUS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ZLESS-(CW*(C_HOFFSET))
        DD    N_PLUS
    DD    0

        POP     EAX      ;(S1) <- (S1) + (S2)
        POP     EBX
        ADD     EAX,EBX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   D+   *
;  **********
;
    ALIGN     4
N_DPLUS:   DD      2
        DB      "D+"
    ALIGN     4
DPLUS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PLUS-(CW*(C_HOFFSET))
        DD    N_DPLUS
    DD    0

        POP     EAX      ; YHW
        POP     EDX      ; YLW
        POP     EBX      ; XHW
        POP     ECX      ; XLW
        ADD     EDX,ECX   ; SLW
        ADC     EAX,EBX   ; SHW
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **************
;  *   NEGATE   *
;  **************
;
    ALIGN     4
N_NEGATE:   DD      6
        DB      "NEGATE"
    ALIGN     4
NEGATE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DPLUS-(CW*(C_HOFFSET))
        DD    N_NEGATE
    DD    0

        POP     EAX
        NEG     EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   DNEGATE   *
;  ***************
;
    ALIGN     4
N_DNEGA:   DD      7
        DB      "DNEGATE"
    ALIGN     4
DNEGA:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    NEGATE-(CW*(C_HOFFSET))
        DD    N_DNEGA
    DD    0

        POP     EBX
        POP     ECX
        SUB     EAX,EAX
        MOV     EDX,EAX
        SUB     EDX,ECX   ; MAKE 2'S COMPLEMENT
        SBB     EAX,EBX   ; HIGH CELL
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
        ;
;

;  ************
;  *   OVER   *
;  ************
;
    ALIGN     4
N_OVER:   DD      4
        DB      "OVER"
    ALIGN     4
OVER:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DNEGA-(CW*(C_HOFFSET))
        DD    N_OVER
    DD    0

        POP     EDX
        POP     EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   DROP   *
;  ************
;
    ALIGN     4
N_DROP:   DD      4
        DB      "DROP"
    ALIGN     4
DROP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    OVER-(CW*(C_HOFFSET))
        DD    N_DROP
    DD    0

        POP     EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   2DROP   *
;  *************
;
    ALIGN     4
N_TDROP:   DD      5
        DB      "2DROP"
    ALIGN     4
TDROP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DROP-(CW*(C_HOFFSET))
        DD    N_TDROP
    DD    0

        POP     EAX
        POP     EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   SWAP   *
;  ************
;
    ALIGN     4
N_SWAP:   DD      4
        DB      "SWAP"
    ALIGN     4
SWAP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TDROP-(CW*(C_HOFFSET))
        DD    N_SWAP
    DD    0

        POP     EDX
        POP     EAX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   DUP   *
;  ***********
;
    ALIGN     4
N_LDUP:   DD      3
        DB      "DUP"
    ALIGN     4
LDUP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SWAP-(CW*(C_HOFFSET))
        DD    N_LDUP
    DD    0

        POP     EAX
        PUSH    EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   2DUP   *
;  ************
;
    ALIGN     4
N_TDUP:   DD      4
        DB      "2DUP"
    ALIGN     4
TDUP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LDUP-(CW*(C_HOFFSET))
        DD    N_TDUP
    DD    0

        POP     EAX
        POP     EDX
        PUSH    EDX
        PUSH    EAX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   2SWAP   *
;  *************
;
    ALIGN     4
N_TSWAP:   DD      5
        DB      "2SWAP"
    ALIGN     4
TSWAP:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TDUP-(CW*(C_HOFFSET))
        DD    N_TSWAP
    DD    0

        POP     EBX
        POP     ECX
        POP     EAX
        POP     EDX
        PUSH     ECX
        PUSH     EBX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   2OVER   *
;  *************
;
    ALIGN     4
N_TOVER:   DD      5
        DB      "2OVER"
    ALIGN     4
TOVER:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TSWAP-(CW*(C_HOFFSET))
        DD    N_TOVER
    DD    0

        POP     EBX
        POP     ECX
        POP     EAX
        POP     EDX
        PUSH     EDX
        PUSH     EAX
        PUSH     ECX
        PUSH     EBX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   +!   *
;  **********
;
    ALIGN     4
N_PSTORE:   DD      2
        DB      "+!"
    ALIGN     4
PSTORE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TOVER-(CW*(C_HOFFSET))
        DD    N_PSTORE
    DD    0

        POP     EBX      ;ADDRESS
        POP     EAX      ;INCREMENT
        ADD     [EBX],EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **************
;  *   TOGGLE   *
;  **************
;
    ALIGN     4
N_TOGGL:   DD      6
        DB      "TOGGLE"
    ALIGN     4
TOGGL:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PSTORE-(CW*(C_HOFFSET))
        DD    N_TOGGL
    DD    0

        POP     EAX      ;BIT PATTERN
        POP     EBX      ;ADDR
        XOR     [EBX],EAX ;
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   @   *
;  *********
;
    ALIGN     4
N_FETCH:   DD      1
        DB      "@"
    ALIGN     4
FETCH:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TOGGL-(CW*(C_HOFFSET))
        DD    N_FETCH
    DD    0

        POP     EBX
        MOV     EAX,[EBX]
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   C@   *
;  **********
;
    ALIGN     4
N_CFET:   DD      2
        DB      "C@"
    ALIGN     4
CFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FETCH-(CW*(C_HOFFSET))
        DD    N_CFET
    DD    0

        POP     EBX
        XOR     EAX,EAX
        MOV     AL,[EBX]
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   2@   *
;  **********
;
    ALIGN     4
N_TFET:   DD      2
        DB      "2@"
    ALIGN     4
TFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CFET-(CW*(C_HOFFSET))
        DD    N_TFET
    DD    0

        POP     EBX      ;ADDR
        MOV     EAX,[EBX] ;MSW
        MOV     EDX,[EBX+(CW*(1))]        ;LSW
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   !   *
;  *********
;
    ALIGN     4
N_STORE:   DD      1
        DB      "!"
    ALIGN     4
STORE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TFET-(CW*(C_HOFFSET))
        DD    N_STORE
    DD    0

        POP     EBX      ;ADDR
        POP     EAX      ;DATA
        MOV     [EBX],EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   C!   *
;  **********
;
    ALIGN     4
N_CSTOR:   DD      2
        DB      "C!"
    ALIGN     4
CSTOR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    STORE-(CW*(C_HOFFSET))
        DD    N_CSTOR
    DD    0

        POP     EBX      ;ADDR
        POP     EAX      ;DATA
        MOV     [EBX],AL
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   2!   *
;  **********
;
    ALIGN     4
N_TSTOR:   DD      2
        DB      "2!"
    ALIGN     4
TSTOR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CSTOR-(CW*(C_HOFFSET))
        DD    N_TSTOR
    DD    0

        POP     EBX      ;ADDR
        POP     EAX      ;MSW
        MOV     [EBX],EAX
        POP     EAX      ;LSW
        MOV     [EBX+(CW*(1))],EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **************
;  *   WITHIN   *
;  **************
;
    ALIGN     4
N_WITHIN:   DD      6
        DB      "WITHIN"
    ALIGN     4
WITHIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TSTOR-(CW*(C_HOFFSET))
        DD    N_WITHIN
    DD    0

        DD      OVER, LSUB, TOR
        DD      LSUB, FROMR
        DD      ULESS
        DD      SEMIS
;


;  **********
;  *   L@   *
;  **********
;
    ALIGN     4
N_LFET:   DD      2
        DB      "L@"
    ALIGN     4
LFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    WITHIN-(CW*(C_HOFFSET))
        DD    N_LFET
    DD    0

        POP     EBX      ;MEM LOC
        POP     ECX      ;SEG REG VAL
        MOV     EDX,DS   ; Leave this for real mode code.
        MOV     DS,ECX
        MOV     EBX,[EBX]
        MOV     DS,EDX
        PUSH    EBX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   L!   *
;  **********
;
    ALIGN     4
N_LSTORE:   DD      2
        DB      "L!"
    ALIGN     4
LSTORE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LFET-(CW*(C_HOFFSET))
        DD    N_LSTORE
    DD    0

        POP     EBX
        POP     ECX
        POP     EDX
        MOV     EAX,DS
        MOV     DS,ECX
        MOV     [EBX],EDX
        MOV     DS,EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;
;

;  *********
;  *   :   *
;  *********
;
    ALIGN     4
N_COLON:   DD      1
        DB      ":"
    ALIGN     4
COLON:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LSTORE-(CW*(C_HOFFSET))
        DD    N_COLON
    DD    0

        DD      SCSP
        DD      LPWORD
        DD      PCREAT
        DD      LATEST, HIDDEN
        DD      RBRAC
        DD      PSCOD
DOCOL:  LEA     EBP,[EBP - (CW*(1))]  ;Push HIP
        MOV     [EBP],ESI ;R1 <- (IP)
         MOV     ESI,[EAX+(CW*(D_HOFFSET - C_HOFFSET))]  ;(IP) <- (PFA)
;        CALL    DISPLAYSI



        MOV      LONG[SPSAVE],ESP
        AND     EAX,EAX
        MOV     ESP,EAX

        JMP     GDT_SWITCH: $+3+CW+M4_SWITCHOFFSET
        MOV EAX,CR0
        DEC AL
        MOV CR0,EAX            ;set real mode
        BITS   16
        MOV     AX,SWITCHSEGMENT
        MOV     DS,AX
        MOV     ES,AX
        MOV     AX,SS_RST ; Make stack valid
        MOV     SS,AX
        STI
        ; Allow interrupts to happen.

        CLI

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX

        MOV     ESP, LONG[SPSAVE]
;
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   ;   *
;  *********
;
    ALIGN     4
N_SEMI:   DD      1
        DB      ";"
    ALIGN     4
SEMI:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    COLON-(CW*(C_HOFFSET))
        DD    N_SEMI
    DD    0

        DD      QCSP
        DD      LIT, SEMIS, COMMA
        DD      LATEST, HIDDEN
        DD      LBRAC
        DD      SEMIS
;

;  ****************
;  *   CONSTANT   *
;  ****************
;
    ALIGN     4
N_LCONST:   DD      8
        DB      "CONSTANT"
    ALIGN     4
LCONST:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SEMI-(CW*(C_HOFFSET))
        DD    N_LCONST
    DD    0

        DD      LPWORD
        DD      PCREAT
        DD      LATEST, TDFA, STORE
        DD      PSCOD
DOCON:  MOV     EAX,[EAX+(CW*((D_HOFFSET-C_HOFFSET)))] ;GET DATA FROM PFA
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ****************
;  *   VARIABLE   *
;  ****************
;
    ALIGN     4
N_VAR:   DD      8
        DB      "VARIABLE"
    ALIGN     4
VAR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LCONST-(CW*(C_HOFFSET))
        DD    N_VAR
    DD    0

        DD      LPWORD
        DD      PCREAT
        DD      ZERO, COMMA
        DD      PSCOD
DOVAR:  MOV     EAX,[EAX+(CW*((D_HOFFSET-C_HOFFSET)))] ;(AX) <- PFA
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   USER   *
;  ************
;
    ALIGN     4
N_USER:   DD      4
        DB      "USER"
    ALIGN     4
USER:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    VAR-(CW*(C_HOFFSET))
        DD    N_USER
    DD    0

        DD      LCONST
        DD      PSCOD
DOUSE:  MOV     EBX,[EAX+(CW*((D_HOFFSET-C_HOFFSET)))] ;PFA
        MOV     EDI, LONG[USINI]
        LEA     EAX,[EBX+EDI]      ;ADDR OF VARIABLE
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;
;

;  *********
;  *   _   *
;  *********
;
    ALIGN     4
N_X:   DD      1
        DB      "_"
    ALIGN     4
X:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    USER-(CW*(C_HOFFSET))
        DD    N_X
    DD    0

        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]    ;Whatever happens to be in EAX, i.e. the dea of ``_''.
;

;  *********
;  *   0   *
;  *********
;
    ALIGN     4
N_ZERO:   DD      1
        DB      "0"
    ALIGN     4
ZERO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    X-(CW*(C_HOFFSET))
        DD    N_ZERO
    DD    0

        XOR     EAX,EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   1   *
;  *********
;
    ALIGN     4
N_ONE:   DD      1
        DB      "1"
    ALIGN     4
ONE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ZERO-(CW*(C_HOFFSET))
        DD    N_ONE
    DD    0

        MOV     EAX,1
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   2   *
;  *********
;
    ALIGN     4
N_TWO:   DD      1
        DB      "2"
    ALIGN     4
TWO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ONE-(CW*(C_HOFFSET))
        DD    N_TWO
    DD    0

        MOV     EAX,2
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   BL   *
;  **********
;
    ALIGN     4
N_LBL:   DD      2
        DB      "BL"
    ALIGN     4
LBL:        DD    DOCON
        DD    ABL
        DD    0H
        DD    TWO-(CW*(C_HOFFSET))
        DD    N_LBL
    DD    0

;

;  **********
;  *   $@   *
;  **********
;
    ALIGN     4
N_SFET:   DD      2
        DB      "$@"
    ALIGN     4
SFET:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LBL-(CW*(C_HOFFSET))
        DD    N_SFET
    DD    0

        DD LDUP, CELLP, SWAP, FETCH
        DD SEMIS
;

;  **********
;  *   $!   *
;  **********
;
    ALIGN     4
N_SSTOR:   DD      2
        DB      "$!"
    ALIGN     4
SSTOR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SFET-(CW*(C_HOFFSET))
        DD    N_SSTOR
    DD    0

        DD TDUP, STORE, CELLP, SWAP, LCMOVE
        DD SEMIS
;

;  *************
;  *   $!-BD   *
;  *************
;
    ALIGN     4
N_SSTORBD:   DD      5
        DB      "$!-BD"
    ALIGN     4
SSTORBD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SSTOR-(CW*(C_HOFFSET))
        DD    N_SSTORBD
    DD    0

        DD TDUP, CSTOR, ONEP, SWAP, LCMOVE
        DD SEMIS
;

;  ***********
;  *   $+!   *
;  ***********
;
    ALIGN     4
N_SADD:   DD      3
        DB      "$+!"
    ALIGN     4
SADD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SSTORBD-(CW*(C_HOFFSET))
        DD    N_SADD
    DD    0

        DD   LDUP, FETCH, TOR ; Remember old count.
        DD   TDUP, PSTORE
        DD   CELLP, FROMR, PLUS, SWAP, LCMOVE
        DD SEMIS
;

;  ***********
;  *   $C+   *
;  ***********
;
    ALIGN     4
N_CHAPP:   DD      3
        DB      "$C+"
    ALIGN     4
CHAPP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SADD-(CW*(C_HOFFSET))
        DD    N_CHAPP
    DD    0

        DD   LDUP, TOR
        DD   LDUP, FETCH, PLUS, CELLP, CSTOR
        DD   ONE, FROMR, PSTORE
        DD SEMIS
;

;  **********
;  *   $,   *
;  **********
;
    ALIGN     4
N_SCOMMA:   DD      2
        DB      "$,"
    ALIGN     4
SCOMMA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CHAPP-(CW*(C_HOFFSET))
        DD    N_SCOMMA
    DD    0

        DD HERE, TOR
        DD LDUP, CELLP, ALLOT
        DD RR, SSTOR, FROMR
 DD LALIGN
        DD SEMIS
;

;  ***********
;  *   C/L   *
;  ***********
;
    ALIGN     4
N_CSLL:   DD      3
        DB      "C/L"
    ALIGN     4
CSLL:        DD    DOCON
        DD    64
        DD    0H
        DD    SCOMMA-(CW*(C_HOFFSET))
        DD    N_CSLL
    DD    0

;
;
;

        ALIGN      2       ; Otherwise INT 13 hangs when filling
        ; in into the lba block as a buffer address.
        DB      0       ;Padding the odd # of bytes in the name FIRST.

;  *************
;  *   FIRST   *
;  *************
;
    ALIGN     4
N_FIRST:   DD      5
        DB      "FIRST"
    ALIGN     4
FIRST:        DD    DOVAR
        DD    BUF1
        DD    0H
        DD    CSLL-(CW*(C_HOFFSET))
        DD    N_FIRST
    DD    0

BUF1:   RESB    (KBBUF+2*CW)*NBUF
;
;

;  *************
;  *   LIMIT   *
;  *************
;
    ALIGN     4
N_LIMIT:   DD      5
        DB      "LIMIT"
    ALIGN     4
LIMIT:        DD    DOCON
        DD    BUF1+(KBBUF+2*CW)*NBUF
        DD    0H
        DD    FIRST-(CW*(C_HOFFSET))
        DD    N_LIMIT
    DD    0

; THE END  OF THE MEMORY

;  **********
;  *   EM   *
;  **********
;
    ALIGN     4
N_LEM:   DD      2
        DB      "EM"
    ALIGN     4
LEM:        DD    DOCON
        DD    ACTUAL_EM
        DD    0H
        DD    LIMIT-(CW*(C_HOFFSET))
        DD    N_LEM
    DD    0

;

;  **********
;  *   BM   *
;  **********
;
    ALIGN     4
N_LBM:   DD      2
        DB      "BM"
    ALIGN     4
LBM:        DD    DOCON
        DD    ORIG
        DD    0H
        DD    LEM-(CW*(C_HOFFSET))
        DD    N_LBM
    DD    0

;

;  *************
;  *   B/BUF   *
;  *************
;
    ALIGN     4
N_BBUF:   DD      5
        DB      "B/BUF"
    ALIGN     4
BBUF:        DD    DOCON
        DD    KBBUF
        DD    0H
        DD    LBM-(CW*(C_HOFFSET))
        DD    N_BBUF
    DD    0

;
; All user variables are initialised
; with the values from USINI.
; The implementation relies on the initialisation of
; those with numbers (1..11), so change in concord with USINI.

;  **********
;  *   U0   *
;  **********
;
    ALIGN     4
N_UZERO:   DD      2
        DB      "U0"
    ALIGN     4
UZERO:        DD    DOUSE
        DD    (CW*(1))
        DD    0H
        DD    BBUF-(CW*(C_HOFFSET))
        DD    N_UZERO
    DD    0

;

;  **********
;  *   S0   *
;  **********
;
    ALIGN     4
N_SZERO:   DD      2
        DB      "S0"
    ALIGN     4
SZERO:        DD    DOUSE
        DD    (CW*(2))
        DD    0H
        DD    UZERO-(CW*(C_HOFFSET))
        DD    N_SZERO
    DD    0

;

;  **********
;  *   R0   *
;  **********
;
    ALIGN     4
N_RZERO:   DD      2
        DB      "R0"
    ALIGN     4
RZERO:        DD    DOUSE
        DD    (CW*(3))
        DD    0H
        DD    SZERO-(CW*(C_HOFFSET))
        DD    N_RZERO
    DD    0

;

;  ***********
;  *   TIB   *
;  ***********
;
    ALIGN     4
N_TIB:   DD      3
        DB      "TIB"
    ALIGN     4
TIB:        DD    DOUSE
        DD    (CW*(4))
        DD    0H
        DD    RZERO-(CW*(C_HOFFSET))
        DD    N_TIB
    DD    0

;

;  **************
;  *   RUBOUT   *
;  **************
;
    ALIGN     4
N_RUBOUT:   DD      6
        DB      "RUBOUT"
    ALIGN     4
RUBOUT:        DD    DOUSE
        DD    (CW*(5))
        DD    0H
        DD    TIB-(CW*(C_HOFFSET))
        DD    N_RUBOUT
    DD    0

;

;  ***************
;  *   WARNING   *
;  ***************
;
    ALIGN     4
N_LWARN:   DD      7
        DB      "WARNING"
    ALIGN     4
LWARN:        DD    DOUSE
        DD    (CW*(7))
        DD    0H
        DD    RUBOUT-(CW*(C_HOFFSET))
        DD    N_LWARN
    DD    0

;

;  *************
;  *   FENCE   *
;  *************
;
    ALIGN     4
N_FENCE:   DD      5
        DB      "FENCE"
    ALIGN     4
FENCE:        DD    DOUSE
        DD    (CW*(8))
        DD    0H
        DD    LWARN-(CW*(C_HOFFSET))
        DD    N_FENCE
    DD    0

;

;  **********
;  *   DP   *
;  **********
;
    ALIGN     4
N_LDP:   DD      2
        DB      "DP"
    ALIGN     4
LDP:        DD    DOUSE
        DD    (CW*(9))
        DD    0H
        DD    FENCE-(CW*(C_HOFFSET))
        DD    N_LDP
    DD    0

;

;  ****************
;  *   VOC-LINK   *
;  ****************
;
    ALIGN     4
N_VOCL:   DD      8
        DB      "VOC-LINK"
    ALIGN     4
VOCL:        DD    DOUSE
        DD    (CW*(10))
        DD    0H
        DD    LDP-(CW*(C_HOFFSET))
        DD    N_VOCL
    DD    0

;

;  **************
;  *   OFFSET   *
;  **************
;
    ALIGN     4
N_LOFFSET:   DD      6
        DB      "OFFSET"
    ALIGN     4
LOFFSET:        DD    DOUSE
        DD    (CW*(11))
        DD    0H
        DD    VOCL-(CW*(C_HOFFSET))
        DD    N_LOFFSET
    DD    0

;
; End of user variables with fixed place.
;
;

;  *************
;  *   WHERE   *
;  *************
;
    ALIGN     4
N_LWHERE:   DD      5
        DB      "WHERE"
    ALIGN     4
LWHERE:        DD    DOUSE
        DD    (CW*(12))
        DD    0H
        DD    LOFFSET-(CW*(C_HOFFSET))
        DD    N_LWHERE
    DD    0
    ;  Occupies two CELLS!
;

;  ***********
;  *   SCR   *
;  ***********
;
    ALIGN     4
N_SCR:   DD      3
        DB      "SCR"
    ALIGN     4
SCR:        DD    DOUSE
        DD    (CW*(33))
        DD    0H
        DD    LWHERE-(CW*(C_HOFFSET))
        DD    N_SCR
    DD    0

;

;  *************
;  *   STATE   *
;  *************
;
    ALIGN     4
N_STATE:   DD      5
        DB      "STATE"
    ALIGN     4
STATE:        DD    DOUSE
        DD    (CW*(18))
        DD    0H
        DD    SCR-(CW*(C_HOFFSET))
        DD    N_STATE
    DD    0

;

;  ************
;  *   BASE   *
;  ************
;
    ALIGN     4
N_BASE:   DD      4
        DB      "BASE"
    ALIGN     4
BASE:        DD    DOUSE
        DD    (CW*(19))
        DD    0H
        DD    STATE-(CW*(C_HOFFSET))
        DD    N_BASE
    DD    0

;

;  ***********
;  *   DPL   *
;  ***********
;
    ALIGN     4
N_DPL:   DD      3
        DB      "DPL"
    ALIGN     4
DPL:        DD    DOUSE
        DD    (CW*(20))
        DD    0H
        DD    BASE-(CW*(C_HOFFSET))
        DD    N_DPL
    DD    0

;

;  ***********
;  *   FLD   *
;  ***********
;
    ALIGN     4
N_LFLD:   DD      3
        DB      "FLD"
    ALIGN     4
LFLD:        DD    DOUSE
        DD    (CW*(21))
        DD    0H
        DD    DPL-(CW*(C_HOFFSET))
        DD    N_LFLD
    DD    0

;


;  ***********
;  *   CSP   *
;  ***********
;
    ALIGN     4
N_LCSP:   DD      3
        DB      "CSP"
    ALIGN     4
LCSP:        DD    DOUSE
        DD    (CW*(22))
        DD    0H
        DD    LFLD-(CW*(C_HOFFSET))
        DD    N_LCSP
    DD    0

;
;

;  **********
;  *   R#   *
;  **********
;
    ALIGN     4
N_RNUM:   DD      2
        DB      "R#"
    ALIGN     4
RNUM:        DD    DOUSE
        DD    (CW*(23))
        DD    0H
        DD    LCSP-(CW*(C_HOFFSET))
        DD    N_RNUM
    DD    0

;

;  ***********
;  *   HLD   *
;  ***********
;
    ALIGN     4
N_HLD:   DD      3
        DB      "HLD"
    ALIGN     4
HLD:        DD    DOUSE
        DD    (CW*(24))
        DD    0H
        DD    RNUM-(CW*(C_HOFFSET))
        DD    N_HLD
    DD    0

;

;  ***********
;  *   OUT   *
;  ***********
;
    ALIGN     4
N_LOUT:   DD      3
        DB      "OUT"
    ALIGN     4
LOUT:        DD    DOUSE
        DD    (CW*(25))
        DD    0H
        DD    HLD-(CW*(C_HOFFSET))
        DD    N_LOUT
    DD    0

;

;  *************
;  *   (BLK)   *
;  *************
;
    ALIGN     4
N_PBLK:   DD      5
        DB      "(BLK)"
    ALIGN     4
PBLK:        DD    DOUSE
        DD    (CW*(26))
        DD    0H
        DD    LOUT-(CW*(C_HOFFSET))
        DD    N_PBLK
    DD    0

;

;  **********
;  *   IN   *
;  **********
;
    ALIGN     4
N_LIN:   DD      2
        DB      "IN"
    ALIGN     4
LIN:        DD    DOUSE
        DD    (CW*(29))
        DD    0H
        DD    PBLK-(CW*(C_HOFFSET))
        DD    N_LIN
    DD    0

;


;  *************
;  *   (>IN)   *
;  *************
;
    ALIGN     4
N_PIIN:   DD      5
        DB      "(>IN)"
    ALIGN     4
PIIN:        DD    DOUSE
        DD    (CW*(30))
        DD    0H
        DD    LIN-(CW*(C_HOFFSET))
        DD    N_PIIN
    DD    0

;
;

;  ************
;  *   ARGS   *
;  ************
;
    ALIGN     4
N_ARGS:   DD      4
        DB      "ARGS"
    ALIGN     4
ARGS:        DD    DOUSE
        DD    (CW*(31))
        DD    0H
        DD    PIIN-(CW*(C_HOFFSET))
        DD    N_ARGS
    DD    0

;

;  ***************
;  *   HANDLER   *
;  ***************
;
    ALIGN     4
N_HANDLER:   DD      7
        DB      "HANDLER"
    ALIGN     4
HANDLER:        DD    DOUSE
        DD    (CW*(32))
        DD    0H
        DD    ARGS-(CW*(C_HOFFSET))
        DD    N_HANDLER
    DD    0

;

;  ***************
;  *   CURRENT   *
;  ***************
;
    ALIGN     4
N_CURR:   DD      7
        DB      "CURRENT"
    ALIGN     4
CURR:        DD    DOUSE
        DD    (CW*(34))
        DD    0H
        DD    HANDLER-(CW*(C_HOFFSET))
        DD    N_CURR
    DD    0

;

;  *****************
;  *   REMAINDER   *
;  *****************
;
    ALIGN     4
N_REMAIND:   DD      9
        DB      "REMAINDER"
    ALIGN     4
REMAIND:        DD    DOUSE
        DD    (CW*(14))
        DD    0H
        DD    CURR-(CW*(C_HOFFSET))
        DD    N_REMAIND
    DD    0

;      IMPORTANT
; REQUIRES ONE MORE CELL!
;

;  ***************
;  *   CONTEXT   *
;  ***************
;
    ALIGN     4
N_CONTEXT:   DD      7
        DB      "CONTEXT"
    ALIGN     4
CONTEXT:        DD    DOUSE
        DD    (CW*(37))
        DD    0H
        DD    REMAIND-(CW*(C_HOFFSET))
        DD    N_CONTEXT
    DD    0
 ; Up to  37+8
;      IMPORTANT
;     8 USER SPACE CELLS MUST BE KEPT FREE
;     IN ADDITION TO THE ONE FOR CONTEXT
;
;========== END USER VARIABLES =============;
;

;  **********
;  *   1+   *
;  **********
;
    ALIGN     4
N_ONEP:   DD      2
        DB      "1+"
    ALIGN     4
ONEP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CONTEXT-(CW*(C_HOFFSET))
        DD    N_ONEP
    DD    0

        DD      ONE
        DD      PLUS
        DD      SEMIS
;

;  *************
;  *   CELL+   *
;  *************
;
    ALIGN     4
N_CELLP:   DD      5
        DB      "CELL+"
    ALIGN     4
CELLP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ONEP-(CW*(C_HOFFSET))
        DD    N_CELLP
    DD    0

        DD      LIT, CW
        DD      PLUS
        DD      SEMIS
;

;
;  *************
;  *   CELLS   *
;  *************
;
    ALIGN     4
N_LCELLS:   DD      5
        DB      "CELLS"
    ALIGN     4
LCELLS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CELLP-(CW*(C_HOFFSET))
        DD    N_LCELLS
    DD    0

        DD       TWO
        DD      LSHIFT
        DD      SEMIS
;

;  *************
;  *   CHAR+   *
;  *************
;
    ALIGN     4
N_CHARP:   DD      5
        DB      "CHAR+"
    ALIGN     4
CHARP:        DD    DOCOL
        DD    (ONEP+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    0H
        DD    LCELLS-(CW*(C_HOFFSET))
        DD    N_CHARP
    DD    0

;

;  *************
;  *   CHARS   *
;  *************
;
    ALIGN     4
N_CHARS:   DD      5
        DB      "CHARS"
    ALIGN     4
CHARS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    CHARP-(CW*(C_HOFFSET))
        DD    N_CHARS
    DD    0

        DD      SEMIS
;

;  *************
;  *   ALIGN   *
;  *************
;
    ALIGN     4
N_LALIGN:   DD      5
        DB      "ALIGN"
    ALIGN     4
LALIGN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    CHARS-(CW*(C_HOFFSET))
        DD    N_LALIGN
    DD    0


        DD      LDP, FETCH
        DD      ALIGNED
        DD      LDP, STORE
;
        DD      SEMIS
;

;  ***************
;  *   ALIGNED   *
;  ***************
;
    ALIGN     4
N_ALIGNED:   DD      7
        DB      "ALIGNED"
    ALIGN     4
ALIGNED:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LALIGN-(CW*(C_HOFFSET))
        DD    N_ALIGNED
    DD    0


        POP     EAX
        DEC     EAX
        OR      AL,3
        INC     EAX
        PUSH    EAX
;
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   HERE   *
;  ************
;
    ALIGN     4
N_HERE:   DD      4
        DB      "HERE"
    ALIGN     4
HERE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ALIGNED-(CW*(C_HOFFSET))
        DD    N_HERE
    DD    0

        DD      LDP
        DD      FETCH
        DD      SEMIS
;

;  *************
;  *   ALLOT   *
;  *************
;
    ALIGN     4
N_ALLOT:   DD      5
        DB      "ALLOT"
    ALIGN     4
ALLOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    HERE-(CW*(C_HOFFSET))
        DD    N_ALLOT
    DD    0

        DD      LDP
        DD      PSTORE
        DD      SEMIS
;

;  *********
;  *   ,   *
;  *********
;
    ALIGN     4
N_COMMA:   DD      1
        DB      ","
    ALIGN     4
COMMA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ALLOT-(CW*(C_HOFFSET))
        DD    N_COMMA
    DD    0

        DD      HERE
        DD      STORE
        DD      LIT, CW
        DD      ALLOT
        DD      SEMIS
;

;  **********
;  *   C,   *
;  **********
;
    ALIGN     4
N_CCOMM:   DD      2
        DB      "C,"
    ALIGN     4
CCOMM:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    COMMA-(CW*(C_HOFFSET))
        DD    N_CCOMM
    DD    0

        DD      HERE
        DD      CSTOR
        DD      ONE
        DD      ALLOT
        DD      SEMIS
;

;  *********
;  *   -   *
;  *********
;
    ALIGN     4
N_LSUB:   DD      1
        DB      "-"
    ALIGN     4
LSUB:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CCOMM-(CW*(C_HOFFSET))
        DD    N_LSUB
    DD    0

        POP     EDX      ;S1
        POP     EAX
        SUB     EAX,EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]      ;S1 = S2 - S1
;

;  *********
;  *   =   *
;  *********
;
    ALIGN     4
N_EQUAL:   DD      1
        DB      "="
    ALIGN     4
EQUAL:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LSUB-(CW*(C_HOFFSET))
        DD    N_EQUAL
    DD    0

        DD      LSUB
        DD      ZEQU
        DD      SEMIS
;

;  *********
;  *   <   *
;  *********
;
    ALIGN     4
N_LESS:   DD      1
        DB      "<"
    ALIGN     4
LESS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    EQUAL-(CW*(C_HOFFSET))
        DD    N_LESS
    DD    0

        POP     EDX      ;S1
        POP     EBX      ;S2
        XOR     EAX,EAX   ;0 default RESULT
        CMP     EBX,EDX
        JNL     LES1
        DEC     EAX
LES1:   PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   U<   *
;  **********
;
    ALIGN     4
N_ULESS:   DD      2
        DB      "U<"
    ALIGN     4
ULESS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LESS-(CW*(C_HOFFSET))
        DD    N_ULESS
    DD    0

        POP     EAX
        POP     EDX
        SUB     EDX,EAX
        SBB     EAX,EAX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *********
;  *   >   *
;  *********
;
    ALIGN     4
N_GREAT:   DD      1
        DB      ">"
    ALIGN     4
GREAT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ULESS-(CW*(C_HOFFSET))
        DD    N_GREAT
    DD    0

        DD      SWAP
        DD      LESS
        DD      SEMIS
;

;  **********
;  *   <>   *
;  **********
;
    ALIGN     4
N_UNEQ:   DD      2
        DB      "<>"
    ALIGN     4
UNEQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    GREAT-(CW*(C_HOFFSET))
        DD    N_UNEQ
    DD    0

        DD      LSUB
        DD      ZEQU
        DD      ZEQU
        DD      SEMIS
;

;  ***********
;  *   ROT   *
;  ***********
;
    ALIGN     4
N_ROT:   DD      3
        DB      "ROT"
    ALIGN     4
ROT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    UNEQ-(CW*(C_HOFFSET))
        DD    N_ROT
    DD    0

        POP     EDX      ;S1
        POP     EBX      ;S2
        POP     EAX      ;S3
        PUSH    EBX
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   SPACE   *
;  *************
;
    ALIGN     4
N_SPACE:   DD      5
        DB      "SPACE"
    ALIGN     4
SPACE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ROT-(CW*(C_HOFFSET))
        DD    N_SPACE
    DD    0

        DD      LBL
        DD      EMIT
        DD      SEMIS
;

;  ************
;  *   ?DUP   *
;  ************
;
    ALIGN     4
N_QDUP:   DD      4
        DB      "?DUP"
    ALIGN     4
QDUP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SPACE-(CW*(C_HOFFSET))
        DD    N_QDUP
    DD    0

        DD      LDUP
        DD      ZBRAN
        DD      QDUP1-$-CW ; IF
        DD      LDUP    ;THEN
QDUP1:  DD      SEMIS
;

;  **************
;  *   LATEST   *
;  **************
;
    ALIGN     4
N_LATEST:   DD      6
        DB      "LATEST"
    ALIGN     4
LATEST:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QDUP-(CW*(C_HOFFSET))
        DD    N_LATEST
    DD    0

        DD      CURR
        DD      FETCH
        DD      TLFA
        DD      FETCH
        DD      SEMIS
;

;  ************
;  *   >CFA   *
;  ************
;
    ALIGN     4
N_TCFA:   DD      4
        DB      ">CFA"
    ALIGN     4
TCFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LATEST-(CW*(C_HOFFSET))
        DD    N_TCFA
    DD    0

        DD      LIT, (CW*(C_HOFFSET))
        DD      PLUS
        DD      SEMIS
;

;  ************
;  *   >DFA   *
;  ************
;
    ALIGN     4
N_TDFA:   DD      4
        DB      ">DFA"
    ALIGN     4
TDFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TCFA-(CW*(C_HOFFSET))
        DD    N_TDFA
    DD    0

        DD      LIT, (CW*(D_HOFFSET))
        DD      PLUS
        DD      SEMIS
;

;  ************
;  *   >FFA   *
;  ************
;
    ALIGN     4
N_TFFA:   DD      4
        DB      ">FFA"
    ALIGN     4
TFFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TDFA-(CW*(C_HOFFSET))
        DD    N_TFFA
    DD    0

        DD      LIT, (CW*(F_HOFFSET))
        DD      PLUS
        DD      SEMIS
;

;  ************
;  *   >LFA   *
;  ************
;
    ALIGN     4
N_TLFA:   DD      4
        DB      ">LFA"
    ALIGN     4
TLFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TFFA-(CW*(C_HOFFSET))
        DD    N_TLFA
    DD    0

        DD      LIT, (CW*(L_HOFFSET))
        DD      PLUS
        DD      SEMIS
;

;  ************
;  *   >NFA   *
;  ************
;
    ALIGN     4
N_TNFA:   DD      4
        DB      ">NFA"
    ALIGN     4
TNFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TLFA-(CW*(C_HOFFSET))
        DD    N_TNFA
    DD    0

        DD      LIT,(CW*(N_HOFFSET))
        DD      PLUS
        DD      SEMIS
;


;
;  ************
;  *   >SFA   *
;  ************
;
    ALIGN     4
N_TSFA:   DD      4
        DB      ">SFA"
    ALIGN     4
TSFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TNFA-(CW*(C_HOFFSET))
        DD    N_TSFA
    DD    0

        DD      LIT,(CW*(S_HOFFSET))
        DD      PLUS
        DD      SEMIS
;
;

;  ************
;  *   >PHA   *
;  ************
;
    ALIGN     4
N_TPHA:   DD      4
        DB      ">PHA"
    ALIGN     4
TPHA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TSFA-(CW*(C_HOFFSET))
        DD    N_TPHA
    DD    0

        DD      LIT,(CW*(PH_OFFSET))
        DD      PLUS
        DD      SEMIS
;

;  *************
;  *   >BODY   *
;  *************
;
    ALIGN     4
N_TOBODY:   DD      5
        DB      ">BODY"
    ALIGN     4
TOBODY:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TPHA-(CW*(C_HOFFSET))
        DD    N_TOBODY
    DD    0

        DD      CTOD
        DD      TDFA, FETCH
        DD      CELLP           ; Skip DOES> pointer.
        DD      SEMIS
;

;  *************
;  *   BODY>   *
;  *************
;
    ALIGN     4
N_BODYF:   DD      5
        DB      "BODY>"
    ALIGN     4
BODYF:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TOBODY-(CW*(C_HOFFSET))
        DD    N_BODYF
    DD    0

        DD      LIT,(CW*(BD_OFFSET))
        DD      LSUB
        DD      SEMIS
;

;  ************
;  *   CFA>   *
;  ************
;
    ALIGN     4
N_CTOD:   DD      4
        DB      "CFA>"
    ALIGN     4
CTOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BODYF-(CW*(C_HOFFSET))
        DD    N_CTOD
    DD    0

        DD      LIT,(CW*(C_HOFFSET))
        DD      LSUB
        DD      SEMIS
;

;  ************
;  *   >WID   *
;  ************
;
    ALIGN     4
N_TWID:   DD      4
        DB      ">WID"
    ALIGN     4
TWID:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CTOD-(CW*(C_HOFFSET))
        DD    N_TWID
    DD    0

        DD      TOBODY
        DD      CELLP ; Skip vfa link.
        DD      SEMIS
;

;  ************
;  *   >VFA   *
;  ************
;
    ALIGN     4
N_TVFA:   DD      4
        DB      ">VFA"
    ALIGN     4
TVFA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TWID-(CW*(C_HOFFSET))
        DD    N_TVFA
    DD    0

        DD      TOBODY
        DD      SEMIS
;


;  ************
;  *   !CSP   *
;  ************
;
    ALIGN     4
N_SCSP:   DD      4
        DB      "!CSP"
    ALIGN     4
SCSP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TVFA-(CW*(C_HOFFSET))
        DD    N_SCSP
    DD    0

        DD      SPFET
        DD      LCSP
        DD      STORE
        DD      SEMIS
;
;

;  **************
;  *   ?ERROR   *
;  **************
;
    ALIGN     4
N_QERR:   DD      6
        DB      "?ERROR"
    ALIGN     4
QERR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SCSP-(CW*(C_HOFFSET))
        DD    N_QERR
    DD    0

        DD      SWAP
        DD      ZBRAN
        DD      QERR1-$-CW ;IF
        DD      LIN, FETCH
        DD      SRC, FETCH
        DD      LWHERE, TSTOR
        DD      THROW
        DD      BRAN
        DD      QERR2-$-CW  ;ELSE
QERR1:  DD      DROP    ;THEN
QERR2:  DD      SEMIS
;

;  **************
;  *   ?ERRUR   *
;  **************
;
    ALIGN     4
N_QERRUR:   DD      6
        DB      "?ERRUR"
    ALIGN     4
QERRUR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QERR-(CW*(C_HOFFSET))
        DD    N_QERRUR
    DD    0

        DD      ZERO, MIN, LDUP, QERR
        DD      SEMIS
;


;  **************
;  *   ?DELIM   *
;  **************
;
    ALIGN     4
N_QDELIM:   DD      6
        DB      "?DELIM"
    ALIGN     4
QDELIM:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QERRUR-(CW*(C_HOFFSET))
        DD    N_QDELIM
    DD    0

        DD      INBRS
        DD      QBL
        DD      ZEQU
        DD      LIT, 10, QERR
        DD      DROP
        DD      SEMIS
;

;  ************
;  *   ?CSP   *
;  ************
;
    ALIGN     4
N_QCSP:   DD      4
        DB      "?CSP"
    ALIGN     4
QCSP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QDELIM-(CW*(C_HOFFSET))
        DD    N_QCSP
    DD    0

        DD      SPFET
        DD      LCSP
        DD      FETCH
        DD      LSUB
        DD      LIT, 20, QERR
        DD      SEMIS
;

;  *************
;  *   ?COMP   *
;  *************
;
    ALIGN     4
N_QCOMP:   DD      5
        DB      "?COMP"
    ALIGN     4
QCOMP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QCSP-(CW*(C_HOFFSET))
        DD    N_QCOMP
    DD    0

        DD      STATE
        DD      FETCH
        DD      ZEQU
        DD      LIT, 17, QERR
        DD      SEMIS
;

;  *************
;  *   ?EXEC   *
;  *************
;
    ALIGN     4
N_QEXEC:   DD      5
        DB      "?EXEC"
    ALIGN     4
QEXEC:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QCOMP-(CW*(C_HOFFSET))
        DD    N_QEXEC
    DD    0

        DD      STATE
        DD      FETCH
        DD      LIT, 18, QERR
        DD      SEMIS
;

;  **************
;  *   ?PAIRS   *
;  **************
;
    ALIGN     4
N_QPAIR:   DD      6
        DB      "?PAIRS"
    ALIGN     4
QPAIR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QEXEC-(CW*(C_HOFFSET))
        DD    N_QPAIR
    DD    0

        DD      LSUB
        DD      LIT, 19, QERR
        DD      SEMIS
;


;  ****************
;  *   ?LOADING   *
;  ****************
;
    ALIGN     4
N_QLOAD:   DD      8
        DB      "?LOADING"
    ALIGN     4
QLOAD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QPAIR-(CW*(C_HOFFSET))
        DD    N_QLOAD
    DD    0

        DD      BLK
        DD      FETCH
        DD      ZEQU
        DD      LIT, 22, QERR
        DD      SEMIS
;
;
;
;

;  *********
;  *   [   *
;  *********
;
    ALIGN     4
N_LBRAC:   DD      1
        DB      "["
    ALIGN     4
LBRAC:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    QLOAD-(CW*(C_HOFFSET))
        DD    N_LBRAC
    DD    0

        DD      ZERO
        DD      STATE
        DD      STORE
        DD      SEMIS
;

;  *********
;  *   ]   *
;  *********
;
    ALIGN     4
N_RBRAC:   DD      1
        DB      "]"
    ALIGN     4
RBRAC:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LBRAC-(CW*(C_HOFFSET))
        DD    N_RBRAC
    DD    0

        DD      ONE
        DD      STATE
        DD      STORE
        DD      SEMIS
;

;  **************
;  *   HIDDEN   *
;  **************
;
    ALIGN     4
N_HIDDEN:   DD      6
        DB      "HIDDEN"
    ALIGN     4
HIDDEN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RBRAC-(CW*(C_HOFFSET))
        DD    N_HIDDEN
    DD    0

        DD      TFFA
        DD      LIT,B_INVIS
        DD      TOGGL
        DD      SEMIS
;

;  ***********
;  *   HEX   *
;  ***********
;
    ALIGN     4
N_HEX:   DD      3
        DB      "HEX"
    ALIGN     4
HEX:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    HIDDEN-(CW*(C_HOFFSET))
        DD    N_HEX
    DD    0

        DD      LIT,16
        DD      BASE
        DD      STORE
        DD      SEMIS
;

;  ***************
;  *   DECIMAL   *
;  ***************
;
    ALIGN     4
N_DECA:   DD      7
        DB      "DECIMAL"
    ALIGN     4
DECA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    HEX-(CW*(C_HOFFSET))
        DD    N_DECA
    DD    0

        DD      LIT,10
        DD      BASE
        DD      STORE
        DD      SEMIS
;

;  ***************
;  *   (;CODE)   *
;  ***************
;
    ALIGN     4
N_PSCOD:   DD      7
        DB      "(;CODE)"
    ALIGN     4
PSCOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DECA-(CW*(C_HOFFSET))
        DD    N_PSCOD
    DD    0

        DD      FROMR
        DD      LATEST
        DD      TCFA
        DD      STORE
        DD      SEMIS
;

;

;  **************
;  *   CREATE   *
;  **************
;
    ALIGN     4
N_CREATE:   DD      6
        DB      "CREATE"
    ALIGN     4
CREATE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PSCOD-(CW*(C_HOFFSET))
        DD    N_CREATE
    DD    0

        DD      LPWORD
        DD      PCREAT
        DD      LIT, HLNOOP, COMMA
        DD      PSCOD
DODOE:  LEA     EBP,[EBP - (CW*(1))] ;Push HIP.
        MOV     [EBP],ESI
        MOV     ESI,[EAX+(CW*((D_HOFFSET-C_HOFFSET)))] ;NEW IP
        LEA     EAX,[ESI+(CW*(1))]
        MOV     ESI,[ESI]
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
HLNOOP: DD      SEMIS
;

;  *************
;  *   DOES>   *
;  *************
;
    ALIGN     4
N_DOES:   DD      5
        DB      "DOES>"
    ALIGN     4
DOES:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CREATE-(CW*(C_HOFFSET))
        DD    N_DOES
    DD    0

        DD      FROMR
        DD      LATEST
        DD      TDFA
        DD      FETCH
        DD      STORE
        DD      SEMIS
;

;  *************
;  *   COUNT   *
;  *************
;
    ALIGN     4
N_COUNT:   DD      5
        DB      "COUNT"
    ALIGN     4
COUNT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DOES-(CW*(C_HOFFSET))
        DD    N_COUNT
    DD    0

        DD      LDUP
        DD      ONEP
        DD      SWAP
        DD      CFET
        DD      SEMIS
;

;  *****************
;  *   -TRAILING   *
;  *****************
;
    ALIGN     4
N_DTRAI:   DD      9
        DB      "-TRAILING"
    ALIGN     4
DTRAI:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    COUNT-(CW*(C_HOFFSET))
        DD    N_DTRAI
    DD    0

        DD      LDUP
        DD      ZERO
        DD     XQDO
        DD      DTRA4-$-CW
DTRA1:  DD      OVER
        DD      OVER
        DD      PLUS
        DD      ONE
        DD      LSUB
        DD      CFET
        DD      QBL
        DD      ZEQU
        DD      ZBRAN
        DD      DTRA2-$-CW ;IF
        DD      LLEAV
DTRA2:  DD      ONE
        DD      LSUB    ; THEN
        DD     XLOOP
        DD      DTRA1-$-CW    ; LOOP
DTRA4:
        DD      SEMIS
;


;  **********
;  *   S"   *
;  **********
;
    ALIGN     4
N_SQUOT:   DD      2
        DB      'S"'
    ALIGN     4
SQUOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DTRAI-(CW*(C_HOFFSET))
        DD    N_SQUOT
    DD    0

        DD      DENQ
        DD      SEMIS
;
;

;  **********
;  *   ."   *
;  **********
;
    ALIGN     4
N_DOTQ:   DD      2
        DB      '."'
    ALIGN     4
DOTQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    SQUOT-(CW*(C_HOFFSET))
        DD    N_DOTQ
    DD    0

        DD      DENQ
        DD      STATE
        DD      FETCH
        DD      ZBRAN
        DD      DOTQ1-$-CW ; IF
        DD      LIT, LTYPE, COMMA
        DD      BRAN
        DD      DOTQ2-$-CW
DOTQ1:
        DD      LTYPE
DOTQ2:
        DD      SEMIS   ; THEN
;

;  **********
;  *   .(   *
;  **********
;
    ALIGN     4
N_DOTP:   DD      2
        DB      ".("
    ALIGN     4
DOTP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DOTQ-(CW*(C_HOFFSET))
        DD    N_DOTP
    DD    0

        DD      LIT, ')'
        DD      PPARS
        DD      LTYPE
        DD      SEMIS
;

;  ***************
;  *   SET-SRC   *
;  ***************
;
    ALIGN     4
N_SETSRC:   DD      7
        DB      "SET-SRC"
    ALIGN     4
SETSRC:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DOTP-(CW*(C_HOFFSET))
        DD    N_SETSRC
    DD    0

        DD      OVER, PLUS
        DD      SWAP, SRC, TSTOR
        DD      SRC, FETCH
        DD      LIN, STORE ;  IN

;       DD      DOTS
        DD      SEMIS
;

;  ****************
;  *   EVALUATE   *
;  ****************
;
    ALIGN     4
N_EVALUATE:   DD      8
        DB      "EVALUATE"
    ALIGN     4
EVALUATE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SETSRC-(CW*(C_HOFFSET))
        DD    N_EVALUATE
    DD    0

        DD      SAVE
        DD      SETSRC
        DD      LIT, INTER, CATCH
        DD      RESTO
        DD      THROW
        DD      SEMIS
;

;  ************
;  *   FILL   *
;  ************
;
    ALIGN     4
N_FILL:   DD      4
        DB      "FILL"
    ALIGN     4
FILL:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    EVALUATE-(CW*(C_HOFFSET))
        DD    N_FILL
    DD    0

        POP     EAX      ; FILL CHAR
        POP     ECX      ; FILL COUNT
        POP     EDI      ; BEGIN ADDR
;       MOV    BX,DS
;       MOV    ES,BX   ; ES <- DS
        CLD             ; INC DIRECTION
        REP     STOSB   ;STORE BYTE
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ************
;  *   CORA   *
;  ************
;
    ALIGN     4
N_CORA:   DD      4
        DB      "CORA"
    ALIGN     4
CORA:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FILL-(CW*(C_HOFFSET))
        DD    N_CORA
    DD    0

;       MOV    ES,BX   ; ES <- DS
;       MOV    BX,DS
        MOV     EDX,ESI   ;SAVE
        XOR     EAX,EAX   ; Result
        POP     ECX      ; count
        POP     EDI      ; addr2
        POP     ESI      ; addr1
        CLD             ; INC DIRECTION
        REP     CMPSB   ; Compare BYTE
        JZ      CORA3
        MOV     AL,1    ;Remainder is already 0
        JNC     CORA3
        NEG     EAX
CORA3:
        MOV     ESI,EDX  ;Restore
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   $I   *
;  **********
;
    ALIGN     4
N_SINDEX:   DD      2
        DB      "$I"
    ALIGN     4
SINDEX:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CORA-(CW*(C_HOFFSET))
        DD    N_SINDEX
    DD    0

;       MOV    ES,BX   ; ES <- DS
;       MOV    BX,DS
        POP     EAX      ; char
        POP     ECX      ; count
        POP     EDI      ; addr
        OR      EDI,EDI   ;Clear zero flag.
        CLD             ; INC DIRECTION
        REPNZ     SCASB   ; Compare BYTE
        JZ      SINDEX1
        XOR     EDI,EDI    ;Not found: 0
        INC     EDI
SINDEX1:
        DEC     EDI
        PUSH    EDI
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   $S   *
;  **********
;
    ALIGN     4
N_SSPLIT:   DD      2
        DB      "$S"
    ALIGN     4
SSPLIT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SINDEX-(CW*(C_HOFFSET))
        DD    N_SSPLIT
    DD    0

;       MOV    ES,BX   ; ES <- DS
;       MOV    BX,DS
        POP     EAX      ; char
        POP     ECX      ; count
        MOV     EBX,ECX
        POP     EDI      ; addr
        OR      EDI,EDI   ;Clear zero flag.
        MOV     EDX,EDI   ; Copy
        CLD             ; INC DIRECTION
        REPNZ     SCASB   ; Compare BYTE
        JZ      SSPLIT1
; Not present.
        PUSH    ECX   ; Nil pointer.
        JMP SSPLIT2
SSPLIT1:
        PUSH    EDI
        SUB     EBX,ECX
        DEC     EBX      ;Delimiter is not part of first string.
SSPLIT2:
        PUSH    ECX   ;Remaining length
        PUSH    EDX   ;Start of first string.
        PUSH    EBX   ;Skipped length.
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  *************
;  *   ERASE   *
;  *************
;
    ALIGN     4
N_LERASE:   DD      5
        DB      "ERASE"
    ALIGN     4
LERASE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SSPLIT-(CW*(C_HOFFSET))
        DD    N_LERASE
    DD    0

        DD      ZERO
        DD      FILL
        DD      SEMIS
;


;  *************
;  *   BLANK   *
;  *************
;
    ALIGN     4
N_BLANK:   DD      5
        DB      "BLANK"
    ALIGN     4
BLANK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LERASE-(CW*(C_HOFFSET))
        DD    N_BLANK
    DD    0

        DD      LBL
        DD      FILL
        DD      SEMIS
;
;

;  ************
;  *   HOLD   *
;  ************
;
    ALIGN     4
N_HOLD:   DD      4
        DB      "HOLD"
    ALIGN     4
HOLD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BLANK-(CW*(C_HOFFSET))
        DD    N_HOLD
    DD    0

        DD      LIT,-1
        DD      HLD
        DD      PSTORE
        DD      HLD
        DD      FETCH
        DD      CSTOR
        DD      SEMIS
;

;  ***********
;  *   PAD   *
;  ***********
;
    ALIGN     4
N_PAD:   DD      3
        DB      "PAD"
    ALIGN     4
PAD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    HOLD-(CW*(C_HOFFSET))
        DD    N_PAD
    DD    0

        DD      HERE
; Allow for a one line name, a double binary number and some hold char's
        DD      LIT,84+128+64
        DD      PLUS
        DD      SEMIS
;


;  ************
;  *   WORD   *
;  ************
;
    ALIGN     4
N_IWORD:   DD      4
        DB      "WORD"
    ALIGN     4
IWORD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PAD-(CW*(C_HOFFSET))
        DD    N_IWORD
    DD    0

        DD      LDUP, LBL, EQUAL
        DD      ZBRAN
        DD      IWORD1-$-CW
         DD      DROP
         DD      LPWORD
        DD      BRAN
        DD      IWORD2-$-CW
IWORD1: DD      TOR
IWORD3:  DD      INBRS, RR, EQUAL
        DD      ZBRAN
        DD      IWORD4-$-CW
        DD      DROP
        DD      BRAN
        DD      IWORD3-$-CW
IWORD4:
        DD      DROP
        DD      LIT, -1, LIN, PSTORE ; Backtrace to first non-delimiter.
        DD      FROMR, PPARS
;        DD      DOTS
IWORD2:
        DD      HERE
        DD      LIT,22H
        DD      BLANK
        DD      HERE
        DD      SSTORBD     ; FIXME
        DD      HERE
;        DD      DOTS
        DD      SEMIS
;
;

;  ************
;  *   CHAR   *
;  ************
;
    ALIGN     4
N_LCHAR:   DD      4
        DB      "CHAR"
    ALIGN     4
LCHAR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    IWORD-(CW*(C_HOFFSET))
        DD    N_LCHAR
    DD    0

        DD      LPWORD, DROP, CFET
        DD      SEMIS
;

;  **************
;  *   [CHAR]   *
;  **************
;
    ALIGN     4
N_BCHAR:   DD      6
        DB      "[CHAR]"
    ALIGN     4
BCHAR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    LCHAR-(CW*(C_HOFFSET))
        DD    N_BCHAR
    DD    0

        DD      LCHAR, LITER
        DD      SEMIS
;

;  ****************
;  *   (NUMBER)   *
;  ****************
;
    ALIGN     4
N_PNUMB:   DD      8
        DB      "(NUMBER)"
    ALIGN     4
PNUMB:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BCHAR-(CW*(C_HOFFSET))
        DD    N_PNUMB
    DD    0

        DD      ZERO, ZERO
        DD      ZERO, DPL, STORE
NPNUM1:  DD      INBRS   ; BEGIN
        DD      LDUP, LIT, ADOT, EQUAL
        DD      ZBRAN
        DD      NPNUM2A-$-CW ; IF
        DD      DROP, DPL, STORE, ZERO
        DD      BRAN
        DD      NPNUM3-$-CW ; ELSE
NPNUM2A:
        DD      LDUP, LIT, ',', EQUAL
        DD      ZBRAN
        DD      NPNUM2-$-CW ; IF
        DD      TDROP, ZERO
        DD      BRAN
        DD      NPNUM3-$-CW ; ELSE
NPNUM2:
        DD      LDUP, QBL
        DD      ZBRAN
        DD      NPNUM4-$-CW ; IF
        DD      DROP, DROP, ONE
        DD      BRAN
        DD      NPNUM3-$-CW ; ELSE
NPNUM4:
        DD      SWAP, DROP
        DD      BASE, FETCH, DIGIT
        DD      ZEQU
        DD      LIT, 10, QERR

        DD      SWAP
        DD      BASE
        DD      FETCH
        DD      USTAR
        DD      DROP
        DD      ROT
        DD      BASE
        DD      FETCH
        DD      USTAR
        DD      DPLUS
        DD      ZERO
NPNUM3:                 ; THEN THEN
        DD      ZBRAN
        DD      NPNUM1-$-CW
        DD      SEMIS
;

;  **************
;  *   NUMBER   *
;  **************
;
    ALIGN     4
N_NUMB:   DD      6
        DB      "NUMBER"
    ALIGN     4
NUMB:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PNUMB-(CW*(C_HOFFSET))
        DD    N_NUMB
    DD    0

LNUMB:
        DD      LIT, -1, LIN, PSTORE
        DD      PNUMB, SDLITE
        DD      SEMIS
;


;  ***************
;  *   >NUMBER   *
;  ***************
;
    ALIGN     4
N_TONUMB:   DD      7
        DB      ">NUMBER"
    ALIGN     4
TONUMB:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    NUMB-(CW*(C_HOFFSET))
        DD    N_TONUMB
    DD    0

        DD      TDUP, PLUS, TOR     ; End available on return stack.
        DD      ZERO
        DD     XQDO
        DD      TONUM9-$-CW
TONUM1:
        DD      LDUP, CFET, BASE, FETCH, DIGIT
        DD      ZEQU
        DD      ZBRAN
        DD      TONUM4-$-CW ; IF
        DD      DROP
        DD      LLEAV
TONUM4:
        DD      SWAP, TOR ; Address out of the way.
        DD      SWAP
        DD      BASE
        DD      FETCH
        DD      USTAR
        DD      DROP
        DD      ROT
        DD      BASE
        DD      FETCH
        DD      USTAR
        DD      DPLUS
        DD      FROMR, ONEP     ; Address back.
        DD     XLOOP
        DD      TONUM1-$-CW
TONUM9:
        DD      FROMR
        DD      OVER, LSUB
        DD      SEMIS
;
;

;  *************
;  *   FOUND   *
;  *************
;
    ALIGN     4
N_FOUND:   DD      5
        DB      "FOUND"
    ALIGN     4
FOUND:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TONUMB-(CW*(C_HOFFSET))
        DD    N_FOUND
    DD    0

        DD      CONTEXT, TOR
FOUND1: DD      RR, FETCH
;        DD      DOTS
        DD      PFIND, LDUP, ZEQU
        DD      ZBRAN
        DD      FOUND3-$-CW
        DD      DROP
        DD      RR, FETCH, LIT, FORTH, LSUB ;Was this ONLY?
        DD      ZBRAN
        DD      FOUND2-$-CW
        DD      FROMR, CELLP, TOR
        DD      BRAN
        DD      FOUND1-$-CW
FOUND2: DD      ZERO
FOUND3: DD      RDROP
        DD      SWAP,DROP,SWAP,DROP
        DD      SEMIS
;

;  ***************
;  *   PRESENT   *
;  ***************
;
    ALIGN     4
N_PRESENT:   DD      7
        DB      "PRESENT"
    ALIGN     4
PRESENT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FOUND-(CW*(C_HOFFSET))
        DD    N_PRESENT
    DD    0

        DD      LDUP, TOR
        DD      FOUND
        DD      LDUP
        DD      ZBRAN
        DD      PRES1-$-CW
        DD      LDUP
        DD      TNFA, FETCH, FETCH ;  Get precise length.
        DD      RR, EQUAL
        DD      LAND
PRES1:
        DD      RDROP
        DD      SEMIS
;


;  ************
;  *   FIND   *
;  ************
;
    ALIGN     4
N_FIND:   DD      4
        DB      "FIND"
    ALIGN     4
FIND:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PRESENT-(CW*(C_HOFFSET))
        DD    N_FIND
    DD    0

        DD      LDUP, COUNT, PRESENT
        DD      LDUP
        DD      ZBRAN
        DD      FIND1-$-CW ;IF
        DD      SWAP, DROP ; The address.
        ; Fine point, get xt by TCFA. Even if a NOOP.
        DD      LDUP, TCFA, SWAP
        DD      TFFA, FETCH
        DD      LIT, B_IMMED, LAND
        DD      LIT, -1, SWAP
        DD      ZBRAN
        DD      FIND1-$-CW ;IF
        DD      NEGATE
FIND1:               ;THEN THEN
        DD      SEMIS
;

;  **************
;  *   (FIND)   *
;  **************
;
    ALIGN     4
N_PFIND:   DD      6
        DB      "(FIND)"
    ALIGN     4
PFIND:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FIND-(CW*(C_HOFFSET))
        DD    N_PFIND
    DD    0

PFIND0:
        DD      LDUP
        DD      ZBRAN
        DD      PFIND1-$-CW
        DD      PMATCH, ZEQU
        DD      ZBRAN
        DD      PFIND1-$-CW
        DD     TLFA, FETCH
        DD      BRAN
        DD      PFIND0-$-CW
PFIND1:
        DD      SEMIS
;

;  *************
;  *   ERROR   *
;  *************
;
    ALIGN     4
N_ERROR:   DD      5
        DB      "ERROR"
    ALIGN     4
ERROR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PFIND-(CW*(C_HOFFSET))
        DD    N_ERROR
    DD    0

        DD      LWHERE, TFET
        DD      OVER, LIT, 20, LSUB
        DD      MAX
        DD      SWAP,OVER, LSUB
        DD      ETYPE
        DD      SKIP
         DD      18
SB3: DB      "? ciforth ERROR # "
       ALIGN     4
        DD      LIT, SB3
        DD      LIT, 18
        DD      ETYPE
        DD      BASE, FETCH
        DD      DECA
        DD      OVER
        DD      STOD, ZERO, PDDOTR      ;This is about (.)
        DD      ETYPE
        DD      BASE, STORE
        DD      MESS
        DD      SEMIS
;

;  *************
;  *   CATCH   *
;  *************
;
    ALIGN     4
N_CATCH:   DD      5
        DB      "CATCH"
    ALIGN     4
CATCH:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ERROR-(CW*(C_HOFFSET))
        DD    N_CATCH
    DD    0

        DD      SPFET, CELLP, TOR
        DD      HANDLER, FETCH, TOR
        DD      RPFET, HANDLER, STORE
        DD      EXEC
        DD      FROMR, HANDLER, STORE
        DD      RDROP, ZERO
        DD      SEMIS
;

;  *************
;  *   THROW   *
;  *************
;
    ALIGN     4
N_THROW:   DD      5
        DB      "THROW"
    ALIGN     4
THROW:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    CATCH-(CW*(C_HOFFSET))
        DD    N_THROW
    DD    0

        DD      LDUP
        DD      ZBRAN
        DD      THROW1-$-CW
        DD      HANDLER, FETCH, ZEQU
        DD      ZBRAN
        DD      THROW2-$-CW
        DD      ERROR
        DD      MTBUF  ; A (too) crude way to remove locks
        DD      SZERO, FETCH, SPSTO
        DD      QUIT
THROW2:
        DD      HANDLER, FETCH, RPSTO
        DD      FROMR, HANDLER, STORE
        DD      FROMR, SWAP, TOR
        DD      SPSTO
        DD      FROMR
        DD      X
THROW1:
        DD      DROP
        DD      SEMIS
;


;  ****************
;  *   (ABORT")   *
;  ****************
;
    ALIGN     4
N_PABORTQ:   DD      8
        DB      '(ABORT")'
    ALIGN     4
PABORTQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    THROW-(CW*(C_HOFFSET))
        DD    N_PABORTQ
    DD    0

        DD      ROT
        DD      ZBRAN
        DD      PABQ1-$-CW ;IF
        DD      ETYPE
        DD      SIGNON, ABORT
        DD      BRAN
        DD      PABQ2-$-CW ;ELSE
PABQ1:  DD       TDROP
PABQ2:   DD      SEMIS
;

;  **************
;  *   ABORT"   *
;  **************
;
    ALIGN     4
N_ABORTQ:   DD      6
        DB      'ABORT"'
    ALIGN     4
ABORTQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    PABORTQ-(CW*(C_HOFFSET))
        DD    N_ABORTQ
    DD    0

        DD      QCOMP
        DD      DENQ
        DD      LIT, PABORTQ, COMMA
        DD      SEMIS
;
;

;  ***********
;  *   ID.   *
;  ***********
;
    ALIGN     4
N_IDDOT:   DD      3
        DB      "ID."
    ALIGN     4
IDDOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ABORTQ-(CW*(C_HOFFSET))
        DD    N_IDDOT
    DD    0

        DD      LDUP, TFFA
        DD      FETCH, LIT, B_DUMMY, LXOR
        DD      ZBRAN
        DD      IDDOT1-$-CW
        DD      TNFA
        DD      FETCH
        DD      SFET
        DD      LTYPE
        DD      SPACE
        DD      SPACE
        DD      SPACE
        DD      BRAN
        DD      IDDOT2-$-CW
IDDOT1:
        DD      DROP
IDDOT2:
        DD      SEMIS
;

;  ****************
;  *   (CREATE)   *
;  ****************
;
    ALIGN     4
N_PCREAT:   DD      8
        DB      "(CREATE)"
    ALIGN     4
PCREAT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    IDDOT-(CW*(C_HOFFSET))
        DD    N_PCREAT
    DD    0

        DD      LDUP
        DD      ZEQU
        DD      LIT, 5, QERR
        DD      TDUP
        DD      PRESENT
        DD      LDUP
        DD      ZBRAN
        DD      CREA1-$-CW ;IF
        DD      TNFA, FETCH, SFET
        DD      ETYPE
        DD      LIT,4
        DD      MESS
        DD      X       ;THEN
CREA1:  DD      DROP
 DD      LALIGN
        DD      SCOMMA
 DD      LALIGN
        DD      HERE,TOR

        DD      RR, TPHA, COMMA         ; Code field.

        DD      RR, TPHA, COMMA         ; Data field.

        DD      ZERO, COMMA ; Flag field.

        DD      CURR, FETCH, TLFA
        DD      LDUP, FETCH, COMMA   ; Link field.
        DD      RR, SWAP, STORE

        DD      COMMA   ; Name field.


        DD      BLK, FETCH, LDUP, ZEQU
        DD      ZBRAN
        DD      CREA2-$-CW
        DD      DROP, LIN, FETCH
CREA2:  DD      COMMA  ; Source field.

        DD      RDROP
        DD      SEMIS
;

;  *****************
;  *   [COMPILE]   *
;  *****************
;
    ALIGN     4
N_BCOMP:   DD      9
        DB      "[COMPILE]"
    ALIGN     4
BCOMP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    PCREAT-(CW*(C_HOFFSET))
        DD    N_BCOMP
    DD    0

        DD      LPWORD
        DD      PRESENT
        DD      LDUP
        DD      ZEQU
        DD      LIT, 16, QERR
        DD      TCFA
        DD      COMMA
        DD      SEMIS
;

;  ****************
;  *   POSTPONE   *
;  ****************
;
    ALIGN     4
N_POSTP:   DD      8
        DB      "POSTPONE"
    ALIGN     4
POSTP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    BCOMP-(CW*(C_HOFFSET))
        DD    N_POSTP
    DD    0

        DD      LPWORD
        DD      PRESENT
        DD      LDUP
        DD      ZEQU
        DD      LIT, 15, QERR
        DD      LDUP, TFFA, FETCH
        DD      LIT, B_IMMED, LAND, ZEQU
        DD      ZBRAN
        DD      POSTP1-$-CW
         DD      LIT, LIT, COMMA
         DD      COMMA
         DD      LIT, COMMA, COMMA
        DD      BRAN
        DD      POSTP2-$-CW
POSTP1:
         DD      COMMA
POSTP2:
        DD      SEMIS
;

;  ***************
;  *   LITERAL   *
;  ***************
;
    ALIGN     4
N_LITER:   DD      7
        DB      "LITERAL"
    ALIGN     4
LITER:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    POSTP-(CW*(C_HOFFSET))
        DD    N_LITER
    DD    0

        DD      STATE
        DD      FETCH
        DD      ZBRAN
        DD      LITE1-$-CW ;IF
        DD      LIT, LIT, COMMA
        DD      COMMA   ;THEN
LITE1:  DD      SEMIS
;

;  ****************
;  *   DLITERAL   *
;  ****************
;
    ALIGN     4
N_DLITE:   DD      8
        DB      "DLITERAL"
    ALIGN     4
DLITE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    LITER-(CW*(C_HOFFSET))
        DD    N_DLITE
    DD    0

        DD      STATE
        DD      FETCH
        DD      ZBRAN
        DD      DLIT1-$-CW ; IF
        DD      SWAP
        DD      LITER
        DD      LITER   ; THEN
DLIT1:  DD      SEMIS
;
;

;  *****************
;  *   SDLITERAL   *
;  *****************
;
    ALIGN     4
N_SDLITE:   DD      9
        DB      "SDLITERAL"
    ALIGN     4
SDLITE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DLITE-(CW*(C_HOFFSET))
        DD    N_SDLITE
    DD    0

        DD      DPL
        DD      FETCH
        DD      ZBRAN
        DD      SDLIT1-$-CW ; IF
        DD      DLITE
        DD      BRAN
        DD      SDLIT2-$-CW ; IF
SDLIT1:
        DD      DROP, LITER
SDLIT2:
        DD      SEMIS
;


;  **************
;  *   ?STACK   *
;  **************
;
    ALIGN     4
N_QSTAC:   DD      6
        DB      "?STACK"
    ALIGN     4
QSTAC:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SDLITE-(CW*(C_HOFFSET))
        DD    N_QSTAC
    DD    0

        DD      SPFET
        DD      SZERO
        DD      FETCH
        DD      SWAP
        DD      ULESS
        DD      ONE, QERR
        DD      SPFET
        DD      HERE
        DD      LIT,80H
        DD      PLUS
        DD      ULESS
        DD      LIT, 7, QERR
        DD      SEMIS
        ;
;
;

;  *****************
;  *   INTERPRET   *
;  *****************
;
    ALIGN     4
N_INTER:   DD      9
        DB      "INTERPRET"
    ALIGN     4
INTER:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QSTAC-(CW*(C_HOFFSET))
        DD    N_INTER
    DD    0

INTE1:
        DD      LPWORD
        DD      LDUP      ; Zero length.
        DD      ZBRAN
        DD      INTE8-$-CW ;WHILE
;       DD      DOTS
;       DD      TDUP, LTYPE
        DD      OVER, TOR       ; Save old parse pointer.
        DD      FOUND
        DD      LDUP, ZEQU
        DD      LIT, 12, QERR
        DD      LDUP, TFFA, FETCH
        DD      LDUP, LIT, B_DENOT, LAND ;Retain copy of flags.
        DD      ZBRAN
        DD      INTE3B-$-CW ;IF
        DD      OVER, TNFA, FETCH, FETCH
        DD      RR, PLUS, LIN, STORE  ;Skip over prefix.
INTE3B:                  ;THEN
        DD      RDROP           ; Drop old parse pointer.
        DD      LIT, B_IMMED, LAND
        DD      STATE, FETCH, ZEQU, LOR
        DD      ZBRAN
        DD      INTE3-$-CW ;IF
        DD      EXEC
        DD      BRAN
        DD      INTE4-$-CW ;IF
INTE3:
        DD      COMMA
                        ;THEN
INTE4:
        DD      QSTAC
        DD      BRAN
        DD      INTE1-$-CW  ;AGAIN
INTE8:  DD      DROP, DROP
        DD      SEMIS
;

;  *****************
;  *   IMMEDIATE   *
;  *****************
;
    ALIGN     4
N_IMMED:   DD      9
        DB      "IMMEDIATE"
    ALIGN     4
IMMED:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    INTER-(CW*(C_HOFFSET))
        DD    N_IMMED
    DD    0

        DD      LATEST
        DD      TFFA
        DD      LIT, B_IMMED
        DD      TOGGL
        DD      SEMIS
;

;  ******************
;  *   VOCABULARY   *
;  ******************
;
    ALIGN     4
N_VOCAB:   DD      10
        DB      "VOCABULARY"
    ALIGN     4
VOCAB:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    IMMED-(CW*(C_HOFFSET))
        DD    N_VOCAB
    DD    0

        DD      CREATE
        DD      LATEST   ; Link this DEA into VOC-LINK chain.
        DD      VOCL
        DD      FETCH
        DD      COMMA
        DD      VOCL
        DD      STORE
        DD      ZERO, COMMA   ; Dummy code field
        DD      ZERO, COMMA   ; Dummy data field
        DD      LIT, B_DUMMY, COMMA ; Dummy flag field
        DD      ZERO, COMMA ;Links to the word FORTH

        DD      DOES
DOVOC:
        DD      ALSO
        DD      CELLP   ; Make it a WID.
        DD      CONTEXT
        DD      STORE
        DD      SEMIS
        ;
;
;   The link to task is a cold start value only.
;   It is updated each time a definition is
;   appended to the 'FORTH' vocabulary.
;

;

;  *******************
;  *   DEFINITIONS   *
;  *******************
;
    ALIGN     4
N_DEFIN:   DD      11
        DB      "DEFINITIONS"
    ALIGN     4
DEFIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    VOCAB-(CW*(C_HOFFSET))
        DD    N_DEFIN
    DD    0

        DD      CONTEXT
        DD      FETCH
        DD      CURR
        DD      STORE
        DD      SEMIS
;

;  ************
;  *   ALSO   *
;  ************
;
    ALIGN     4
N_ALSO:   DD      4
        DB      "ALSO"
    ALIGN     4
ALSO:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DEFIN-(CW*(C_HOFFSET))
        DD    N_ALSO
    DD    0

        DD      CONTEXT, LDUP, CELLP
        DD      LIT, (CW*(8-1))
        DD      LMOVE
        DD      LIT, FORTH  ;End sentinel for array of word lists.
        DD      CONTEXT, LIT, (CW*(8)), PLUS
        DD      STORE ;Trim sets of wordset.
        DD      SEMIS
;

;  ****************
;  *   PREVIOUS   *
;  ****************
;
    ALIGN     4
N_PREVI:   DD      8
        DB      "PREVIOUS"
    ALIGN     4
PREVI:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ALSO-(CW*(C_HOFFSET))
        DD    N_PREVI
    DD    0

        DD      CONTEXT, LDUP, CELLP, SWAP
        DD      LIT, (CW*(8))
        DD      LMOVE
        DD      SEMIS
;

;  ************
;  *   ONLY   *
;  ************
;
    ALIGN     4
N_ONLY:   DD      4
        DB      "ONLY"
    ALIGN     4
ONLY:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PREVI-(CW*(C_HOFFSET))
        DD    N_ONLY
    DD    0

        DD      LIT, FORTH, CONTEXT, STORE
        DD      CONTEXT, LDUP, CELLP
        DD      LIT, (CW*(8-1))
        DD      LCMOVE
        DD      SEMIS
;

;  *********
;  *   (   *
;  *********
;
    ALIGN     4
N_PAREN:   DD      1
        DB      "("
    ALIGN     4
PAREN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    ONLY-(CW*(C_HOFFSET))
        DD    N_PAREN
    DD    0

        DD      LIT,')'
        DD      PPARS
        DD      TDROP
        DD      SEMIS
;
;

;  *********
;  *   \   *
;  *********
;
    ALIGN     4
N_BACKS:   DD      1
        DB      "\"
    ALIGN     4
BACKS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    PAREN-(CW*(C_HOFFSET))
        DD    N_BACKS
    DD    0

; Backup one character, just in case we are at the end of a line.
        DD      LIT, -1, LIN, PSTORE
        DD      LIT,ALF
        DD      PPARS
        DD      TDROP
        DD      SEMIS
;

;  ************
;  *   QUIT   *
;  ************
;
    ALIGN     4
N_QUIT:   DD      4
        DB      "QUIT"
    ALIGN     4
QUIT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BACKS-(CW*(C_HOFFSET))
        DD    N_QUIT
    DD    0

        DD      LBRAC
QUIT1:                  ;BEGIN
        DD      RZERO
        DD      FETCH
        DD      RPSTO

        DD      PACCEP
;
        DD       SETSRC
        DD      INTER
        DD      OK
        DD      BRAN
        DD      QUIT1-$-CW  ;AGAIN
        DD      SEMIS   ;Unnecessary, but helpful for decompilation.
;

;  **********
;  *   OK   *
;  **********
;
    ALIGN     4
N_OK:   DD      2
        DB      "OK"
    ALIGN     4
OK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QUIT-(CW*(C_HOFFSET))
        DD    N_OK
    DD    0

        DD      STATE
        DD      FETCH
        DD      ZEQU
        DD      ZBRAN
        DD      OK2-$-CW ;IF
        DD      SKIP
         DD      3
SB4: DB      " OK"
       ALIGN     4
        DD      LIT, SB4
        DD      LIT, 3
        DD      LTYPE
        DD      CR
OK2:
        DD      SEMIS
;

;  *************
;  *   ABORT   *
;  *************
;
    ALIGN     4
N_ABORT:   DD      5
        DB      "ABORT"
    ALIGN     4
ABORT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    OK-(CW*(C_HOFFSET))
        DD    N_ABORT
    DD    0

        DD      SZERO, FETCH, SPSTO
        DD      ZERO, HANDLER, STORE
        DD      DECA
        DD      ONLY
        DD      FORTH
        DD      DEFIN
        DD      QUIT
        DD      SEMIS   ;Unnecessary, but helpful for decompilation.
;
;      WARM START VECTOR COMES HERE
;      For booting code we enter here, real mode and using the switchsegment.
;      BY control BREAK.
WARM_ENTRY:

BITS   16         ;Pops back at next _SWITCH_
        MOV     ESP, LONG[USINI+(CW*(2))]    ;PARAM. STACK
        MOV     EBP, LONG[USINI+(CW*(3))]    ;RETURN STACK
;Without this clearing of the disk status, the system hangs too.
        XOR     AX,AX
        MOV     DL, DRIVE
        INT     13H     ;Reset floppy disk system.
        XOR     AX,AX
        MOV     DL, 80H
        INT     13H     ;Reset hard disk system.
;Without this clearing of the keyboard status, the system hangs at
;the next call of KEY (INT 10H function 0EH)
        XOR     AX,AX
        MOV     DS,AX
        MOV     [417H],AX
;        MOV     [496H],AX ;This should improve things, but doesn't.
;

        CLI

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX

        MOV     ESP, LONG[SPSAVE]
        MOV     ESI, WRM1
        LODSD                 ; NEXT
        JMP      LONG[EAX]                      ;Hope stacks are still okay.
;
WRM1:   DD      WARM
;

;  ************
;  *   WARM   *
;  ************
;
    ALIGN     4
N_WARM:   DD      4
        DB      "WARM"
    ALIGN     4
WARM:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ABORT-(CW*(C_HOFFSET))
        DD    N_WARM
    DD    0

        DD      MTBUF
        DD      SIGNON
        DD      ABORT
        DD      SEMIS   ;Unnecessary, but helpful for decompilation.
;
;


;  ************
;  *   COLD   *
;  ************
;
    ALIGN     4
N_COLD:   DD      4
        DB      "COLD"
    ALIGN     4
COLD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    WARM-(CW*(C_HOFFSET))
        DD    N_COLD
    DD    0

        DD      ZERO, HANDLER, STORE
        DD      MTBUF
        DD      FIRST
        DD      STALEST,STORE
        DD      FIRST
        DD      PREV,STORE
; Fill user area for single task.
        DD      LIT, USINI
        DD      LIT, USINI+(CW*(1)), FETCH
        DD      LIT, US
        DD      LCMOVE
;
        DD      DECA    ; FIXME has to go done by ABORT anyway.
        DD      ONLY    ; FIXME has to go done by ABORT anyway.
        DD      FORTH   ; FIXME has to go done by ABORT anyway.
        DD      DEFIN   ; FIXME has to go done by ABORT anyway.
        DD      ONE            ; Sign on wanted.
;

        DD      ZBRAN
        DD      COLD5-$-CW
        DD      SIGNON    ; Suppressed for scripting! Or any options.
COLD5:
        DD      ABORT
        DD      BYE     ; In case of turnkey programs.
        DD      SEMIS   ; Unnecessary, but helpful for decompilation.
;

;  ***********
;  *   S>D   *
;  ***********
;
    ALIGN     4
N_STOD:   DD      3
        DB      "S>D"
    ALIGN     4
STOD:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    COLD-(CW*(C_HOFFSET))
        DD    N_STOD
    DD    0

        POP     EDX      ;S1
        SUB     EAX,EAX
        OR      EDX,EDX
        JNS     STOD1   ;POS
        DEC     EAX      ;NEG
STOD1:  PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   ABS   *
;  ***********
;
    ALIGN     4
N_LABS:   DD      3
        DB      "ABS"
    ALIGN     4
LABS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    STOD-(CW*(C_HOFFSET))
        DD    N_LABS
    DD    0

        DD      LDUP
        DD      ZLESS
        DD      ZBRAN
        DD      PM1-$-CW   ;IF
        DD      NEGATE   ;THEN
PM1:
        DD      SEMIS
;

;  ************
;  *   DABS   *
;  ************
;
    ALIGN     4
N_DABS:   DD      4
        DB      "DABS"
    ALIGN     4
DABS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LABS-(CW*(C_HOFFSET))
        DD    N_DABS
    DD    0

        DD      LDUP
        DD      ZLESS
        DD      ZBRAN
        DD      DPM1-$-CW  ;IF
        DD      DNEGA   ;THEN
DPM1:
        DD      SEMIS
;

;  ***********
;  *   MIN   *
;  ***********
;
    ALIGN     4
N_MIN:   DD      3
        DB      "MIN"
    ALIGN     4
MIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DABS-(CW*(C_HOFFSET))
        DD    N_MIN
    DD    0

        DD      TDUP
        DD      GREAT
        DD      ZBRAN
        DD      MIN1-$-CW  ;IF
        DD      SWAP    ;THEN
MIN1:   DD      DROP
        DD      SEMIS
;

;  ***********
;  *   MAX   *
;  ***********
;
    ALIGN     4
N_MAX:   DD      3
        DB      "MAX"
    ALIGN     4
MAX:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MIN-(CW*(C_HOFFSET))
        DD    N_MAX
    DD    0

        DD      TDUP
        DD      LESS
        DD      ZBRAN
        DD      MAX1-$-CW  ;IF
        DD      SWAP    ;THEN
MAX1:   DD      DROP
        DD      SEMIS
;

;  **************
;  *   LSHIFT   *
;  **************
;
    ALIGN     4
N_LSHIFT:   DD      6
        DB      "LSHIFT"
    ALIGN     4
LSHIFT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MAX-(CW*(C_HOFFSET))
        DD    N_LSHIFT
    DD    0

        POP     ECX
        POP     EAX
        SHL     EAX,CL
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **************
;  *   RSHIFT   *
;  **************
;
    ALIGN     4
N_RSHIFT:   DD      6
        DB      "RSHIFT"
    ALIGN     4
RSHIFT:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LSHIFT-(CW*(C_HOFFSET))
        DD    N_RSHIFT
    DD    0

        POP     ECX
        POP     EAX
        SHR     EAX,CL
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   M*   *
;  **********
;
    ALIGN     4
N_MSTAR:   DD      2
        DB      "M*"
    ALIGN     4
MSTAR:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RSHIFT-(CW*(C_HOFFSET))
        DD    N_MSTAR
    DD    0

        POP     EAX
        POP     EBX
        IMUL     EBX      ;SIGNED
        XCHG    EAX,EDX   ;AX NOW = MSW
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ;STORE DOUBLE CELL
;

;  **************
;  *   SM/REM   *
;  **************
;
    ALIGN     4
N_MSLAS:   DD      6
        DB      "SM/REM"
    ALIGN     4
MSLAS:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MSTAR-(CW*(C_HOFFSET))
        DD    N_MSLAS
    DD    0

        POP     EBX      ;DIVISOR
        POP     EDX      ;MSW OF DIVIDEND
        POP     EAX      ;LSW OF DIVIDEND
        IDIV     EBX      ;16 BIT DIVIDE
        PUSH    EDX
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]             ;STORE QUOT/REM
;


;  **********
;  *   2/   *
;  **********
;
    ALIGN     4
N_TWOSL:   DD      2
        DB      "2/"
    ALIGN     4
TWOSL:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MSLAS-(CW*(C_HOFFSET))
        DD    N_TWOSL
    DD    0

        DD      STOD, TWO, FMSLAS
        DD      SWAP, DROP
        DD      SEMIS
;

;  **********
;  *   2*   *
;  **********
;
    ALIGN     4
N_TWOST:   DD      2
        DB      "2*"
    ALIGN     4
TWOST:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TWOSL-(CW*(C_HOFFSET))
        DD    N_TWOST
    DD    0

        DD      TWO, STAR
        DD      SEMIS
;

;  **********
;  *   1-   *
;  **********
;
    ALIGN     4
N_ONEM:   DD      2
        DB      "1-"
    ALIGN     4
ONEM:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TWOST-(CW*(C_HOFFSET))
        DD    N_ONEM
    DD    0

        DD      ONE, LSUB
        DD      SEMIS
;
;

;  **************
;  *   FM/MOD   *
;  **************
;
    ALIGN     4
N_FMSLAS:   DD      6
        DB      "FM/MOD"
    ALIGN     4
FMSLAS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ONEM-(CW*(C_HOFFSET))
        DD    N_FMSLAS
    DD    0

        DD      LDUP, TOR
        DD      TDUP, LXOR, TOR
        DD      MSLAS
        DD      FROMR, ZLESS
        DD      ZBRAN
        DD      FMMOD1-$-CW
        DD      OVER
        DD      ZBRAN
        DD      FMMOD1-$-CW
        DD      ONE, LSUB
        DD      SWAP, FROMR, PLUS, SWAP
        DD      BRAN
        DD      FMMOD2-$-CW
FMMOD1:
        DD      RDROP
FMMOD2:
        DD      SEMIS
;

;  *********
;  *   *   *
;  *********
;
    ALIGN     4
N_STAR:   DD      1
        DB      "*"
    ALIGN     4
STAR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FMSLAS-(CW*(C_HOFFSET))
        DD    N_STAR
    DD    0

        DD      MSTAR
        DD      DROP
        DD      SEMIS
;

;  ************
;  *   /MOD   *
;  ************
;
    ALIGN     4
N_SLMOD:   DD      4
        DB      "/MOD"
    ALIGN     4
SLMOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    STAR-(CW*(C_HOFFSET))
        DD    N_SLMOD
    DD    0

        DD      TOR
        DD      STOD
        DD      FROMR
        DD      MSLAS
        DD      SEMIS
;

;  *********
;  *   /   *
;  *********
;
    ALIGN     4
N_SLASH:   DD      1
        DB      "/"
    ALIGN     4
SLASH:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SLMOD-(CW*(C_HOFFSET))
        DD    N_SLASH
    DD    0

        DD      SLMOD
        DD      SWAP
        DD      DROP
        DD      SEMIS
;

;  ***********
;  *   MOD   *
;  ***********
;
    ALIGN     4
N_LMOD:   DD      3
        DB      "MOD"
    ALIGN     4
LMOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SLASH-(CW*(C_HOFFSET))
        DD    N_LMOD
    DD    0

        DD      SLMOD
        DD      DROP
        DD      SEMIS
;

;  *************
;  *   */MOD   *
;  *************
;
    ALIGN     4
N_SSMOD:   DD      5
        DB      "*/MOD"
    ALIGN     4
SSMOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LMOD-(CW*(C_HOFFSET))
        DD    N_SSMOD
    DD    0

        DD      TOR
        DD      MSTAR
        DD      FROMR
        DD      MSLAS
        DD      SEMIS
;

;  **********
;  *   */   *
;  **********
;
    ALIGN     4
N_SSLA:   DD      2
        DB      "*/"
    ALIGN     4
SSLA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SSMOD-(CW*(C_HOFFSET))
        DD    N_SSLA
    DD    0

        DD      SSMOD
        DD      SWAP
        DD      DROP
        DD      SEMIS
;

;  *************
;  *   M/MOD   *
;  *************
;
    ALIGN     4
N_MSMOD:   DD      5
        DB      "M/MOD"
    ALIGN     4
MSMOD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SSLA-(CW*(C_HOFFSET))
        DD    N_MSMOD
    DD    0

        DD      TOR
        DD      ZERO
        DD      RR
        DD      USLAS
        DD      FROMR
        DD      SWAP
        DD      TOR
        DD      USLAS
        DD      FROMR
        DD      SEMIS
;

;  **************
;  *   (LINE)   *
;  **************
;
    ALIGN     4
N_PLINE:   DD      6
        DB      "(LINE)"
    ALIGN     4
PLINE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MSMOD-(CW*(C_HOFFSET))
        DD    N_PLINE
    DD    0

        DD      TOR
        DD      LIT,64
        DD      MSTAR
        DD      BBUF
        DD      FMSLAS
        DD      FROMR ; This blocks, so is screens.
        DD      PLUS
        DD      BLOCK
        DD      PLUS
        DD      LIT,63
        DD      SEMIS
;

;  **************
;  *   ERRSCR   *
;  **************
;
    ALIGN     4
N_ERRSCR:   DD      6
        DB      "ERRSCR"
    ALIGN     4
ERRSCR:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PLINE-(CW*(C_HOFFSET))
        DD    N_ERRSCR
    DD    0

        DD ERRORSCREEN
;

;  ***************
;  *   MESSAGE   *
;  ***************
;
    ALIGN     4
N_MESS:   DD      7
        DB      "MESSAGE"
    ALIGN     4
MESS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ERRSCR-(CW*(C_HOFFSET))
        DD    N_MESS
    DD    0

        DD      LWARN
        DD      FETCH
        DD      ZBRAN
        DD      MESS1-$-CW ;IF
        DD      ERRSCR, FETCH
        DD      PLINE, ONEP     ; Also print the '\n' !
        DD      ETYPE
        DD      X
MESS1:                  ;THEN
        DD      DROP
        DD      SEMIS
;

;  ***********
;  *   PC@   *
;  ***********
;
    ALIGN     4
N_PCFET:   DD      3
        DB      "PC@"
    ALIGN     4
PCFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MESS-(CW*(C_HOFFSET))
        DD    N_PCFET
    DD    0

; FETCH CHARACTER (BYTE) FROM PORT
        POP     EDX      ; PORT ADDR
        XOR     EAX,EAX
        IN      AL,DX  ; BYTE INPUT
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***********
;  *   PC!   *
;  ***********
;
    ALIGN     4
N_PCSTO:   DD      3
        DB      "PC!"
    ALIGN     4
PCSTO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PCFET-(CW*(C_HOFFSET))
        DD    N_PCSTO
    DD    0

        POP     EDX      ;PORT ADDR
        POP     EAX      ;DATA
        OUT     DX,AL   ; BYTE OUTPUT
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   P@   *
;  **********
;
    ALIGN     4
N_PFET:   DD      2
        DB      "P@"
    ALIGN     4
PFET:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PCSTO-(CW*(C_HOFFSET))
        DD    N_PFET
    DD    0

        POP     EDX      ;PORT ADDR
        IN      EAX,DX  ;WORD INPUT
        PUSH    EAX
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  **********
;  *   P!   *
;  **********
;
    ALIGN     4
N_PSTO:   DD      2
        DB      "P!"
    ALIGN     4
PSTO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PFET-(CW*(C_HOFFSET))
        DD    N_PSTO
    DD    0

        POP     EDX      ;PORT ADDR
        POP     EAX      ;DATA
        OUT     DX,EAX   ;WORD OUTPUT
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;

;  ***************
;  *   STALEST   *
;  ***************
;
    ALIGN     4
N_STALEST:   DD      7
        DB      "STALEST"
    ALIGN     4
STALEST:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PSTO-(CW*(C_HOFFSET))
        DD    N_STALEST
    DD    0

        DD BUF1
;

;  ************
;  *   PREV   *
;  ************
;
    ALIGN     4
N_PREV:   DD      4
        DB      "PREV"
    ALIGN     4
PREV:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    STALEST-(CW*(C_HOFFSET))
        DD    N_PREV
    DD    0

        DD      BUF1
;

;  *************
;  *   #BUFF   *
;  *************
;
    ALIGN     4
N_NOBUF:   DD      5
        DB      "#BUFF"
    ALIGN     4
NOBUF:        DD    DOCON
        DD    NBUF
        DD    0H
        DD    PREV-(CW*(C_HOFFSET))
        DD    N_NOBUF
    DD    0

;

;  ************
;  *   +BUF   *
;  ************
;
    ALIGN     4
N_PBUF:   DD      4
        DB      "+BUF"
    ALIGN     4
PBUF:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    NOBUF-(CW*(C_HOFFSET))
        DD    N_PBUF
    DD    0

        DD      LIT,(KBBUF+2*CW)
        DD      PLUS,LDUP
        DD      LIMIT,EQUAL
        DD      ZBRAN
        DD      PBUF1-$-CW
        DD      DROP,FIRST
PBUF1:  DD      LDUP, PREV, FETCH, LSUB
        DD      SEMIS
;

;  **************
;  *   UPDATE   *
;  **************
;
    ALIGN     4
N_UPDAT:   DD      6
        DB      "UPDATE"
    ALIGN     4
UPDAT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PBUF-(CW*(C_HOFFSET))
        DD    N_UPDAT
    DD    0

        DD      PREV, FETCH
        DD      LDUP, CELLP,CELLP
        DD      SWAP, FETCH
        DD      LOFFSET,  FETCH, PLUS
        DD      ZERO
        DD      RSLW
        DD      SEMIS
;

;  *********************
;  *   EMPTY-BUFFERS   *
;  *********************
;
    ALIGN     4
N_MTBUF:   DD      13
        DB      "EMPTY-BUFFERS"
    ALIGN     4
MTBUF:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    UPDAT-(CW*(C_HOFFSET))
        DD    N_MTBUF
    DD    0

        DD      FIRST
        DD      LIMIT,OVER
        DD      LSUB,LERASE
        DD      SEMIS
        ;
;

;  ****************
;  *   (BUFFER)   *
;  ****************
;
    ALIGN     4
N_BUFFER:   DD      8
        DB      "(BUFFER)"
    ALIGN     4
BUFFER:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MTBUF-(CW*(C_HOFFSET))
        DD    N_BUFFER
    DD    0

; Find the buffer, if it is already here.
    DD      PREV, FETCH
BUFFER1:
    DD          TOR, RR, FETCH, OVER, EQUAL
    DD      ZBRAN
        DD      BUFFER3-$-CW
    DD        DROP, FROMR, EXIT
BUFFER3:
    DD          FROMR
    DD      PBUF, ZEQU
    DD      ZBRAN
        DD      BUFFER1-$-CW
    DD       DROP
; Just allocate the stalest buffer.
    DD       STALEST,   FETCH, TOR
; Remember the next stalest buffer.
    DD       RR
BUFFER2:
    DD       PBUF, OVER, CELLP, FETCH,
    DD       LIT, -1, GREAT, LAND
    DD      ZBRAN
        DD      BUFFER2-$-CW
    DD       STALEST, STORE
; Fill in the house keeping.
    DD       RR, STORE
    DD       ZERO, RR, CELLP, STORE
    DD       RR, PREV, STORE
    DD       FROMR
    DD  SEMIS
;


;  *************
;  *   BLOCK   *
;  *************
;
    ALIGN     4
N_BLOCK:   DD      5
        DB      "BLOCK"
    ALIGN     4
BLOCK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BUFFER-(CW*(C_HOFFSET))
        DD    N_BLOCK
    DD    0


        DD      BUFFER
        DD      LDUP, CELLP, FETCH, ZEQU
        DD      ZBRAN
        DD      BLOCK1-$-CW
        DD      LDUP, CELLP, CELLP
        DD      OVER, FETCH
        DD      LOFFSET,  FETCH, PLUS
        DD      ONE
        DD      RSLW
        DD      ONE, OVER, CELLP, STORE
BLOCK1:
        DD      LDUP, PREV, STORE
        DD      CELLP, CELLP
        DD      SEMIS
;

;  *************
;  *   FLUSH   *
;  *************
;
    ALIGN     4
N_FLUSH:   DD      5
        DB      "FLUSH"
    ALIGN     4
FLUSH:        DD    DOCOL
        DD    (MTBUF+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    0H
        DD    BLOCK-(CW*(C_HOFFSET))
        DD    N_FLUSH
    DD    0


; Unlock all buffers
        DD      LIMIT
        DD      FIRST, CELLP
        DD     XDO
        DD      FLUS2-$-CW
FLUS1:  DD      ZERO, IDO, STORE
        DD      LIT,(KBBUF+2*CW)
        DD      PLOOP
        DD      (FLUS1-$)
FLUS2:
        DD      SEMIS
;

;  ************
;  *   SAVE   *
;  ************
;
    ALIGN     4
N_SAVE:   DD      4
        DB      "SAVE"
    ALIGN     4
SAVE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FLUSH-(CW*(C_HOFFSET))
        DD    N_SAVE
    DD    0

        DD      FROMR
        DD      SRC, TFET
        DD      LIN, FETCH
        DD      TOR, TOR, TOR
        DD      TOR
        DD SEMIS
;

;  ***************
;  *   RESTORE   *
;  ***************
;
    ALIGN     4
N_RESTO:   DD      7
        DB      "RESTORE"
    ALIGN     4
RESTO:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SAVE-(CW*(C_HOFFSET))
        DD    N_RESTO
    DD    0

        DD      FROMR
        DD      FROMR, FROMR, FROMR
        DD      LIN, STORE
        DD      SRC, TSTOR
        DD      TOR
        DD SEMIS
;


;  ******************
;  *   SAVE-INPUT   *
;  ******************
;
    ALIGN     4
N_SAVEI:   DD      10
        DB      "SAVE-INPUT"
    ALIGN     4
SAVEI:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RESTO-(CW*(C_HOFFSET))
        DD    N_SAVEI
    DD    0

        DD      SRC, TFET
        DD      LIN, FETCH
        DD      LIT, 3
        DD SEMIS
;

;  *********************
;  *   RESTORE-INPUT   *
;  *********************
;
    ALIGN     4
N_RESTOI:   DD      13
        DB      "RESTORE-INPUT"
    ALIGN     4
RESTOI:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SAVEI-(CW*(C_HOFFSET))
        DD    N_RESTOI
    DD    0

        DD      DROP
        DD      LIN, STORE
        DD      SRC, TSTOR
        DD      LIT, -1
        DD SEMIS
;
;

;  ************
;  *   LOCK   *
;  ************
;
    ALIGN     4
N_LLOCK:   DD      4
        DB      "LOCK"
    ALIGN     4
LLOCK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RESTOI-(CW*(C_HOFFSET))
        DD    N_LLOCK
    DD    0

        DD      BLOCK
        DD      LIT, CW, LSUB
        DD      LIT, -2, SWAP, PSTORE
        DD      SEMIS
;

;  **************
;  *   UNLOCK   *
;  **************
;
    ALIGN     4
N_LUNLOCK:   DD      6
        DB      "UNLOCK"
    ALIGN     4
LUNLOCK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LLOCK-(CW*(C_HOFFSET))
        DD    N_LUNLOCK
    DD    0

        DD      BLOCK
        DD      LIT, CW, LSUB
        DD      TWO, SWAP, PSTORE
        DD      SEMIS
;

;  ************
;  *   LOAD   *
;  ************
;
    ALIGN     4
N_LOAD:   DD      4
        DB      "LOAD"
    ALIGN     4
LOAD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LUNLOCK-(CW*(C_HOFFSET))
        DD    N_LOAD
    DD    0

        DD      LDUP, THRU
        DD      SEMIS
;

;  ************
;  *   THRU   *
;  ************
;
    ALIGN     4
N_THRU:   DD      4
        DB      "THRU"
    ALIGN     4
THRU:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LOAD-(CW*(C_HOFFSET))
        DD    N_THRU
    DD    0

        DD      SAVE
        DD      ONEP, SWAP
        DD     XDO
        DD      THRU2-$-CW
THRU1:
        DD      IDO, LLOCK
        DD      IDO, BLOCK
        DD      LIT, KBBUF
        DD      SETSRC
        DD      LIT, INTER, CATCH
        DD      IDO, LUNLOCK
        DD      QDUP
        DD      ZBRAN
        DD      THRU3-$-CW
        DD      RDROP, RDROP, RDROP; UNLOOP.
        DD      RESTO
        DD      THROW
THRU3:
        DD     XLOOP
        DD      THRU1-$-CW
THRU2:
        DD      RESTO
        DD      SEMIS
;

;

;  ***********
;  *   BLK   *
;  ***********
;
    ALIGN     4
N_BLK:   DD      3
        DB      "BLK"
    ALIGN     4
BLK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    THRU-(CW*(C_HOFFSET))
        DD    N_BLK
    DD    0

        DD      LIN, FETCH
        DD      FIRST, LIMIT, WITHIN
        DD      SRC, TFET, LSUB
        DD      LIT, 1024, EQUAL, LAND
        DD      ZBRAN
        DD      BLK1-$-CW
        DD      SRC, FETCH, TWO, LCELLS, LSUB, FETCH
        DD      BRAN
        DD      BLK2-$-CW
BLK1:
        DD      ZERO
BLK2:
        DD      PBLK, STORE
        DD      PBLK
        DD      SEMIS
;

;  ***********
;  *   -->   *
;  ***********
;
    ALIGN     4
N_ARROW:   DD      3
        DB      "-->"
    ALIGN     4
ARROW:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    BLK-(CW*(C_HOFFSET))
        DD    N_ARROW
    DD    0

        DD      QLOAD
        DD      BLK, FETCH
        DD      LDUP, LUNLOCK
        DD      ONEP
        DD      LDUP, LLOCK
        DD      LDUP, BLK, STORE
        DD      BLOCK
        DD      LIT, KBBUF
        DD      SETSRC
        DD      SEMIS
        ;
;
;

; Generic call on BIOS. A boon for experimenters.


; Because there is no such thing as a variable interrupt:
; THIS IS SELF MODIFYING CODE! NOT REENTRANT! DO NOT PUT IN ROM!
; BEWARE OF THE SOFTWARE POLICE!
;  *************
;  *   BIOSO   *
;  *************
;
    ALIGN     4
N_BIOSO:   DD      5
        DB      "BIOSO"
    ALIGN     4
BIOSO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ARROW-(CW*(C_HOFFSET))
        DD    N_BIOSO
    DD    0

        POP     EAX      ; Function code
        ; Once we are more acknowledgeable, put segment overwrite here.
        MOV     BYTE [RQBIOS+1],AL ; Patch the code.
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EAX
        PUSH     ESI      ; Save Forth registers. NEEDED?
        PUSH     EBP
        XCHG    ESI,EAX   ; Save AX in (already free) SI


        MOV      LONG[SPSAVE],ESP
        AND     EAX,EAX
        MOV     ESP,EAX

        JMP     GDT_SWITCH: $+3+CW+M4_SWITCHOFFSET
        MOV EAX,CR0
        DEC AL
        MOV CR0,EAX            ;set real mode
        BITS   16
        MOV     AX,SWITCHSEGMENT
        MOV     DS,AX
        MOV     ES,AX
        MOV     AX,SS_RST ; Make stack valid
        MOV     SS,AX
        STI
        XCHG    SI,AX
RQBIOS:  INT(0)          ; Request number to be overwritten.
        PUSHF      ; Save status into DI
        POP     DI
        XCHG    SI,AX ; Save AX in (still free) SI

        CLI

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX

        MOV     ESP, LONG[SPSAVE]
        XCHG    ESI,EAX
        POP     EBP      ; Restore Forth registers. NEEDED?
        POP     ESI
        PUSH     EAX
        PUSH     EBX
        PUSH     ECX
        PUSH     EDX
        PUSH     EDI     ; i.e. flags
        LODSD                 ; NEXT
        JMP      LONG[EAX]
SPSAVE: DD       0H
; SELF MODIFYING CODE ENDS HERE! YOU HAVE BEEN WARNED!
;
;
; Generic call on BIOS. A boon for experimenters.



; Because there is no such thing as a variable interrupt:
; THIS IS SELF MODIFYING CODE! NOT REENTRANT! DO NOT PUT IN ROM!
; BEWARE OF THE SOFTWARE POLICE!
;  *************
;  *   BIOSN   *
;  *************
;
    ALIGN     4
N_BIOSN:   DD      5
        DB      "BIOSN"
    ALIGN     4
BIOSN:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BIOSO-(CW*(C_HOFFSET))
        DD    N_BIOSN
    DD    0

        POP     EAX      ; Function code
        ; Once we are more acknowledgeable, put segment overwrite here.
        MOV     BYTE [RQBIOSN+1],AL ; Patch the code.
        POP     EAX
        POP     EBX
        POP     ECX
        POP     EDX
        PUSH     ESI      ; Save Forth registers. NEEDED?
        PUSH     EBP
        XCHG    ESI,EAX   ; Save AX in (already free) SI


        MOV      LONG[SPSAVE],ESP
        AND     EAX,EAX
        MOV     ESP,EAX

        JMP     GDT_SWITCH: $+3+CW+M4_SWITCHOFFSET
        MOV EAX,CR0
        DEC AL
        MOV CR0,EAX            ;set real mode
        BITS   16
        MOV     AX,SWITCHSEGMENT
        MOV     DS,AX
        MOV     ES,AX
        MOV     AX,SS_RST ; Make stack valid
        MOV     SS,AX
        STI
        XCHG    SI,AX
RQBIOSN:  INT(0)          ; Request number to be overwritten.
        PUSHF      ; Save status into DI
        POP     DI
        XCHG    SI,AX ; Save AX in (still free) SI

        CLI

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX

        MOV     ESP, LONG[SPSAVE]
        XCHG    ESI,EAX
        POP     EBP      ; Restore Forth registers. NEEDED?
        POP     ESI
        PUSH     EAX
        PUSH     EDI     ; i.e. flags
        LODSD                 ; NEXT
        JMP      LONG[EAX]
; SELF MODIFYING CODE ENDS HERE! YOU HAVE BEEN WARNED!
;
;


;  *************
;  *   BDOSO   *
;  *************
;
    ALIGN     4
N_BDOSO:   DD      5
        DB      "BDOSO"
    ALIGN     4
BDOSO:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BIOSN-(CW*(C_HOFFSET))
        DD    N_BDOSO
    DD    0

        POP     EDX
        POP     ECX
        POP     EBX
        POP     EAX
        PUSH     ESI      ; Save Forth registers. NEEDED?
        PUSH     EBP
        XCHG    ESI,EAX   ; Save AX in (already free) SI


        MOV      LONG[SPSAVE],ESP
        AND     EAX,EAX
        MOV     ESP,EAX

        JMP     GDT_SWITCH: $+3+CW+M4_SWITCHOFFSET
        MOV EAX,CR0
        DEC AL
        MOV CR0,EAX            ;set real mode
        BITS   16
        MOV     AX,SWITCHSEGMENT
        MOV     DS,AX
        MOV     ES,AX
        MOV     AX,SS_RST ; Make stack valid
        MOV     SS,AX
        STI
        XCHG    SI,AX
        INT     21H
        PUSHF      ; Save status into DI
        POP     DI; Not EDI!
        XCHG    SI,AX  ; Save AX in (still free) SI

        CLI

        MOV EAX,CR0
        INC AL
        MOV CR0,EAX            ;set protected mode
        BITS   16
        JMP    GDT_CS: $+5
        BITS   32
        MOV     EAX,GDT_DS
        MOV     DS,EAX
        MOV     ES,EAX
        MOV     SS,EAX

        MOV     ESP, LONG[SPSAVE]
        XCHG    ESI,EAX
        POP     EBP      ; Restore Forth registers. NEEDED?
        POP     ESI
        PUSH     EAX
        PUSH     EBX
        PUSH     ECX
        PUSH     EDX
        PUSH     EDI     ; i.e. flags
        LODSD                 ; NEXT
        JMP      LONG[EAX]
;
;
;
;


;  ***********
;  *   MS@   *
;  ***********
;
    ALIGN     4
N_MSFET:   DD      3
        DB      "MS@"
    ALIGN     4
MSFET:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BDOSO-(CW*(C_HOFFSET))
        DD    N_MSFET
    DD    0

        DD      LIT,   2C00H, X, X, X
        DD      LIT,    21H,  BIOSO, DROP
        DD      TOR, TDROP, DROP, FROMR
        DD      LIT, 100H, SLMOD, LIT, 100, STAR, PLUS
        DD      LIT, 10, STAR
        DD      SEMIS
;

;  **********
;  *   MS   *
;  **********
;
    ALIGN     4
N_MS:   DD      2
        DB      "MS"
    ALIGN     4
MS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MSFET-(CW*(C_HOFFSET))
        DD    N_MS
    DD    0

        DD      MSFET, PLUS, LIT, 100000, TOR
LMS0:   DD      LDUP,  MSFET
        DD      LSUB, RR, PLUS
        DD      RR, LMOD, RR, LIT, 2, SLASH
        DD      SWAP, LESS
        DD      ZBRAN
        DD      LMS0-$-CW
        DD      RDROP, DROP
        DD      SEMIS
;
        ;
;------------------------------------
;       SYSTEM DEPENDANT CHAR I/O
;------------------------------------
;
;
;
;
;
;
;


;  *************
;  *   ETYPE   *
;  *************
;
    ALIGN     4
N_ETYPE:   DD      5
        DB      "ETYPE"
    ALIGN     4
ETYPE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    MS-(CW*(C_HOFFSET))
        DD    N_ETYPE
    DD    0

        DD      LTYPE
        DD      SEMIS
;

;  **************
;  *   ACCEPT   *
;  **************
;
    ALIGN     4
N_ACCEP:   DD      6
        DB      "ACCEPT"
    ALIGN     4
ACCEP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ETYPE-(CW*(C_HOFFSET))
        DD    N_ACCEP
    DD    0

        DD      PACCEP
        DD      TSWAP, ROT, MIN
        DD      LDUP, TOR, LMOVE, FROMR
        DD      SEMIS
;
;
;


;  **************
;  *   (EMIT)   *
;  **************
;
    ALIGN     4
N_PEMIT:   DD      6
        DB      "(EMIT)"
    ALIGN     4
PEMIT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ACCEP-(CW*(C_HOFFSET))
        DD    N_PEMIT
    DD    0

        DD      LIT, 0E00H, PLUS, X, X, X
        DD      LIT, 0010H, BIOSO
        DD      TDROP, TDROP, DROP    ;Ignore errors.
        DD      SEMIS
;

;  ************
;  *   EMIT   *
;  ************
;
    ALIGN     4
N_EMIT:   DD      4
        DB      "EMIT"
    ALIGN     4
EMIT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PEMIT-(CW*(C_HOFFSET))
        DD    N_EMIT
    DD    0

             DD      ONE,LOUT,PSTORE
             DD      LIT, 07FH, LAND
             DD      LDUP,LIT,ALF,EQUAL
             DD      ZBRAN
        DD      EMIT1-$-CW
             DD      LIT,ACR,PEMIT
             DD      ZERO,LOUT,STORE
EMIT1:       DD      PEMIT
             DD      SEMIS
;

;  ***********
;  *   KEY   *
;  ***********
;
    ALIGN     4
N_KEY:   DD      3
        DB      "KEY"
    ALIGN     4
KEY:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    EMIT-(CW*(C_HOFFSET))
        DD    N_KEY
    DD    0

        DD      LIT, 1000H, X, X, X
        DD      LIT, 0016H, BIOSO
        DD      TDROP, TDROP
        DD      LIT, 00FFH, LAND
        DD      SEMIS
;

;  ************
;  *   KEY?   *
;  ************
;
    ALIGN     4
N_KEYQ:   DD      4
        DB      "KEY?"
    ALIGN     4
KEYQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    KEY-(CW*(C_HOFFSET))
        DD    N_KEYQ
    DD    0

             DD      LIT, 01100H, ZERO, ZERO, ZERO
             DD      LIT, 016H, BIOSO
             DD      LIT, 040H, LAND, ZEQU, TOR
             DD      DROP, DROP, DROP, DROP
             DD      FROMR, SEMIS
;

;  ************
;  *   POUT   *
;  ************
;
    ALIGN     4
N_POUT:   DD      4
        DB      "POUT"
    ALIGN     4
POUT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    KEYQ-(CW*(C_HOFFSET))
        DD    N_POUT
    DD    0

        DD      X, X, X
        DD      LIT, 0017H, BIOSO
        DD      DROP, TDROP, TDROP           ;Ignore errors.
        DD      SEMIS
;

;  ************
;  *   TYPE   *
;  ************
;
    ALIGN     4
N_LTYPE:   DD      4
        DB      "TYPE"
    ALIGN     4
LTYPE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    POUT-(CW*(C_HOFFSET))
        DD    N_LTYPE
    DD    0

             DD      QDUP
             DD      ZBRAN
        DD      TYPE1-$-CW
             DD      OVER,   PLUS
             DD      SWAP
             DD     XDO
        DD      TYPE4-$-CW
TYPE2:       DD      IDO,    CFET,    EMIT
             DD     XLOOP
        DD      TYPE2-$-CW
             DD      BRAN
        DD      TYPE3-$-CW
TYPE4:
TYPE1:       DD      DROP
TYPE3:       DD      SEMIS
;
;


;  ****************
;  *   (ACCEPT)   *
;  ****************
;
    ALIGN     4
N_PACCEP:   DD      8
        DB      "(ACCEPT)"
    ALIGN     4
PACCEP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LTYPE-(CW*(C_HOFFSET))
        DD    N_PACCEP
    DD    0

        DD      TIB
        DD      FETCH
        DD      LIT,RTS/2
        DD      TDUP
;  Old fig code of EXPECT :
             DD      OVER
             DD      PLUS
             DD      OVER
             DD     XDO
        DD      EXPE8-$-CW
EXPE1:       DD      KEY
             DD      LDUP
             DD      RUBOUT
             DD      FETCH
             DD      EQUAL
             DD      ZBRAN
        DD      EXPE2-$-CW ; IF
             DD      DROP
             DD      LDUP
             DD      IDO
             DD      EQUAL
             DD      LDUP
             DD      FROMR
             DD      TWO     ; Remove last 2 chars
             DD      LSUB
             DD      PLUS
             DD      TOR
             DD      ZBRAN
        DD      EXPE6-$-CW ; IF
             DD      LIT
             DD      BELL
             DD      BRAN
        DD      EXPE7-$-CW  ; ELSE
EXPE6:       DD      LIT
             DD      BSOUT   ; THEN
EXPE7:       DD      BRAN
        DD      EXPE3-$-CW  ; ELSE
EXPE2:       DD      LDUP
             DD      LIT,0DH
             DD      EQUAL
             DD      ZBRAN
        DD      EXPE4-$-CW ; IF
             ; Emulate old fashioned LEAVE.
             DD      FROMR, RDROP, LDUP, TOR, TOR
             DD      DROP
             DD      LBL
             DD      ZERO
             DD      BRAN
        DD      EXPE5-$-CW  ; ELSE
EXPE4:       DD      LDUP    ; THEN
EXPE5:       DD      IDO
             DD      CSTOR
             DD      ZERO
             DD      IDO
             DD      ONEP
             DD      STORE   ; THEN
EXPE3:       DD      EMIT
             DD     XLOOP
        DD      EXPE1-$-CW    ; LOOP
EXPE8:
             DD      DROP
;        ;      EXPEC
        DD      ZERO, SINDEX
        DD      TIB, FETCH, SWAP, OVER, LSUB
        DD      SEMIS
;
 ;

        ;
;------------------------------------
;       SYSTEM DEPENDANT DISK I/O
;------------------------------------


;  ******************
;  *   DISK-ERROR   *
;  ******************
;
    ALIGN     4
N_DERR:   DD      10
        DB      "DISK-ERROR"
    ALIGN     4
DERR:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PACCEP-(CW*(C_HOFFSET))
        DD    N_DERR
    DD    0

        DD      -1
;
;
;

;  *************
;  *   SHELL   *
;  *************
;
    ALIGN     4
N_SHELL:   DD      5
        DB      "SHELL"
    ALIGN     4
SHELL:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DERR-(CW*(C_HOFFSET))
        DD    N_SHELL
    DD    0


                DD      14
        DB      "C:\COMMAND.COM"
        RESB    256                ; Allow for some path
  RESB    100H     ; Double serve as stack at start up.
;

;
;
;


;  *************
;  *   DRIVE   *
;  *************
;
    ALIGN     4
N_LDRIVE:   DD      5
        DB      "DRIVE"
    ALIGN     4
LDRIVE:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SHELL-(CW*(C_HOFFSET))
        DD    N_LDRIVE
    DD    0

LFDRIVE DB      DRIVE                   ; To be used by Forth.
LFSPT   DB      SPT
LFHEADS DB      HEADS
        ALIGN     4
;
;
;

;

;  ***************
;  *   SEC/BLK   *
;  ***************
;
    ALIGN     4
N_SPBLK:   DD      7
        DB      "SEC/BLK"
    ALIGN     4
SPBLK:        DD    DOCON
        DD    SPB
        DD    0H
        DD    LDRIVE-(CW*(C_HOFFSET))
        DD    N_SPBLK
    DD    0

;
;
;


;  **************
;  *   SEC-RW   *
;  **************
;
    ALIGN     4
N_SECRW:   DD      6
        DB      "SEC-RW"
    ALIGN     4
SECRW:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SPBLK-(CW*(C_HOFFSET))
        DD    N_SECRW
    DD    0

        DD      LIT, LFSPT, CFET, SLMOD
        DD      LIT, LFHEADS, CFET, SLMOD   ; Now #sec, #head, #cyl
        DD      TOR
        DD      RR, LIT, 8, RSHIFT, LIT, 6, LSHIFT ; Bit 8, 9 of cyl
        DD      ROT, ONEP, LOR         ; Compose and in place.
        DD      FROMR, LIT, 8, LSHIFT     ;Bits 0..7 of #cyl.
        DD      LOR   ; High, low -- reg CX.
        DD      SWAP
        DD      LIT, 8, LSHIFT
        DD      LIT, LFDRIVE, CFET
        DD      LOR   ; High, low -- reg DX.
        DD      LIT, 13H, BIOSO
        DD      ONE, LAND, DERR, PSTORE
        DD      TDROP, TDROP
        DD      SEMIS
;

;      ( ADDR  OFFSET+BLK#  FLAG (0=W, 1=R) --- )
;  ***********
;  *   R\W   *
;  ***********
;
    ALIGN     4
N_RSLW:   DD      3
        DB      "R\W"
    ALIGN     4
RSLW:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SECRW-(CW*(C_HOFFSET))
        DD    N_RSLW
    DD    0

         DD   ROT, LIT, M4_SWITCHOFFSET, PLUS, ROT, ROT
        DD      ZERO, DERR, STORE
        DD      ZBRAN
        DD      RSLW1-$-CW
        DD      LIT, 0201H      ; Read (AH) one (AL) sector
        DD      BRAN
        DD      RSLW2-$-CW
RSLW1:  DD      LIT, 0301H      ; Write (AH) one (AL) sector
RSLW2:  DD      SWAP
        DD      SPBLK,  STAR
        DD      SPBLK,  OVER, PLUS
        DD      SWAP
        DD     XDO
        DD      RSLW9-$-CW
RSLW0:
        DD      SWAP, TDUP
        DD      IDO
        DD      SECRW
        DD      LIT, BPS, PLUS, SWAP
        DD     XLOOP
        DD      RSLW0-$-CW
RSLW9:
        DD      DROP, DROP
        DD      DERR, FETCH,     QDUP
        DD      ZBRAN
        DD      RSLW5-$-CW              ;OK
        DD      ZLESS
        DD      ZBRAN
        DD      RSLW3-$-CW
        DD      LIT,    9   ;Write error
        DD      BRAN
        DD      RSLW4-$-CW
RSLW3:  DD      LIT,    8   ;Read error
RSLW4:  DD      ZERO,   PREV,   FETCH,     STORE   ;This  buffer
                                                   ; is no good!
        DD      QERR    ;  8 or 9.
RSLW5:  DD      SEMIS
;
;
;
;


;  *********
;  *   '   *
;  *********
;
    ALIGN     4
N_ITICK:   DD      1
        DB      "'"
    ALIGN     4
ITICK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    RSLW-(CW*(C_HOFFSET))
        DD    N_ITICK
    DD    0

        DD      LPWORD, PRESENT
        DD      LDUP, ZEQU
        DD      LIT, 11, QERR
        DD      SEMIS
;

;  ***********
;  *   [']   *
;  ***********
;
    ALIGN     4
N_BTICK:   DD      3
        DB      "[']"
    ALIGN     4
BTICK:        DD    DOCOL
        DD    (TICK+(CW*(PH_OFFSET-C_HOFFSET)))
        DD    B_IMMED
        DD    ITICK-(CW*(C_HOFFSET))
        DD    N_BTICK
    DD    0

;
;

;  ******************
;  *   FORGET-VOC   *
;  ******************
;
    ALIGN     4
N_FORGV:   DD      10
        DB      "FORGET-VOC"
    ALIGN     4
FORGV:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BTICK-(CW*(C_HOFFSET))
        DD    N_FORGV
    DD    0

        DD      TDUP
        DD      SWAP
        DD      ULESS
        DD      ZBRAN
        DD      FORGV1-$-CW
;  Forget part of contents.
        DD      SWAP
        DD      TOR
        DD      TWID
        DD      LDUP
FORGV3:
        DD      TLFA,FETCH    ; Next voc
        DD      LDUP
        DD      RR
        DD      ULESS
        DD      ZBRAN
        DD      FORGV3-$-CW
        DD      SWAP
        DD      TLFA
        DD      STORE
        DD      FROMR
        DD      BRAN
        DD      FORGV2-$-CW
FORGV1:
;        Vocabulary itself is also forgotten.
        DD      TVFA
        DD      FETCH     ; Unlink by linking next vocabulary.
        DD      VOCL
        DD      STORE
        DD      ONLY, FORTH
        DD      DEFIN
FORGV2: DD      SEMIS
;

;  **************
;  *   FORGET   *
;  **************
;
    ALIGN     4
N_FORG:   DD      6
        DB      "FORGET"
    ALIGN     4
FORG:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FORGV-(CW*(C_HOFFSET))
        DD    N_FORG
    DD    0

        DD      TICK
        DD      LDUP
        DD      FENCE
        DD      FETCH
        DD      LESS
        DD      LIT, 21, QERR
        DD      LIT,FORGV
        DD      FORV
        DD      TNFA, FETCH, LDP, STORE
        DD      SEMIS
;

;  *************
;  *   (BACK   *
;  *************
;
    ALIGN     4
N_PBACK:   DD      5
        DB      "(BACK"
    ALIGN     4
PBACK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FORG-(CW*(C_HOFFSET))
        DD    N_PBACK
    DD    0

        DD      HERE
        DD      SEMIS
;

;  *************
;  *   BACK)   *
;  *************
;
    ALIGN     4
N_BACKP:   DD      5
        DB      "BACK)"
    ALIGN     4
BACKP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PBACK-(CW*(C_HOFFSET))
        DD    N_BACKP
    DD    0

        DD      HERE
        DD      CELLP
        DD      LSUB
        DD      COMMA
        DD      SEMIS
;

;  ****************
;  *   (FORWARD   *
;  ****************
;
    ALIGN     4
N_PFORWARD:   DD      8
        DB      "(FORWARD"
    ALIGN     4
PFORWARD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BACKP-(CW*(C_HOFFSET))
        DD    N_PFORWARD
    DD    0

        DD      HERE
        DD      X
        DD      COMMA
        DD      SEMIS
;

;  ****************
;  *   FORWARD)   *
;  ****************
;
    ALIGN     4
N_FORWARDP:   DD      8
        DB      "FORWARD)"
    ALIGN     4
FORWARDP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PFORWARD-(CW*(C_HOFFSET))
        DD    N_FORWARDP
    DD    0

        DD      HERE
        DD      OVER
        DD      CELLP
        DD      LSUB
        DD      SWAP
        DD      STORE
        DD      SEMIS
;

;  *************
;  *   BEGIN   *
;  *************
;
    ALIGN     4
N_BEGIN:   DD      5
        DB      "BEGIN"
    ALIGN     4
BEGIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    FORWARDP-(CW*(C_HOFFSET))
        DD    N_BEGIN
    DD    0

        DD      PBACK
        DD      QCOMP, ONE
        DD      SEMIS
;

;  ************
;  *   THEN   *
;  ************
;
    ALIGN     4
N_THEN:   DD      4
        DB      "THEN"
    ALIGN     4
THEN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    BEGIN-(CW*(C_HOFFSET))
        DD    N_THEN
    DD    0

        DD      QCOMP, TWO, QPAIR
        DD      FORWARDP
        DD      SEMIS
;

;  **********
;  *   DO   *
;  **********
;
    ALIGN     4
N_DO:   DD      2
        DB      "DO"
    ALIGN     4
DO:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    THEN-(CW*(C_HOFFSET))
        DD    N_DO
    DD    0

         DD      LIT, XDO, COMMA, PFORWARD, PBACK
        DD      LIT,3    ; Magic number
        DD      SEMIS
;

;  ***********
;  *   ?DO   *
;  ***********
;
    ALIGN     4
N_QDO:   DD      3
        DB      "?DO"
    ALIGN     4
QDO:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    DO-(CW*(C_HOFFSET))
        DD    N_QDO
    DD    0

         DD      LIT, XQDO, COMMA, PFORWARD, PBACK
        DD      LIT,3    ; Magic number
        DD      SEMIS
;

;  ************
;  *   LOOP   *
;  ************
;
    ALIGN     4
N_LLOOP:   DD      4
        DB      "LOOP"
    ALIGN     4
LLOOP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    QDO-(CW*(C_HOFFSET))
        DD    N_LLOOP
    DD    0

        DD      LIT, 3, QPAIR
        DD      LIT, XLOOP, COMMA, BACKP
        DD      FORWARDP ; For DO to push the leave address.
        DD      SEMIS
;

;  *************
;  *   +LOOP   *
;  *************
;
    ALIGN     4
N_PLOOP:   DD      5
        DB      "+LOOP"
    ALIGN     4
PLOOP:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    LLOOP-(CW*(C_HOFFSET))
        DD    N_PLOOP
    DD    0

        DD      LIT, 3, QPAIR
        DD      LIT, XPLOO, COMMA, BACKP
        DD      FORWARDP ; For DO to push the leave address.
        DD      SEMIS
;

;  *************
;  *   UNTIL   *
;  *************
;
    ALIGN     4
N_UNTIL:   DD      5
        DB      "UNTIL"
    ALIGN     4
UNTIL:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    PLOOP-(CW*(C_HOFFSET))
        DD    N_UNTIL
    DD    0

        DD      ONE, QPAIR
        DD      LIT, ZBRAN, COMMA, BACKP
        DD      SEMIS
;

;  *************
;  *   AGAIN   *
;  *************
;
    ALIGN     4
N_AGAIN:   DD      5
        DB      "AGAIN"
    ALIGN     4
AGAIN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    UNTIL-(CW*(C_HOFFSET))
        DD    N_AGAIN
    DD    0

        DD      ONE, QPAIR
        DD      LIT, BRAN, COMMA, BACKP
        DD      SEMIS
;

;  **************
;  *   REPEAT   *
;  **************
;
    ALIGN     4
N_REPEA:   DD      6
        DB      "REPEAT"
    ALIGN     4
REPEA:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    AGAIN-(CW*(C_HOFFSET))
        DD    N_REPEA
    DD    0

        DD      ONE, QPAIR   ; Matches BEGIN ?
        DD      LIT, BRAN, COMMA, BACKP
        DD      QCOMP, LIT, 4, QPAIR ; Matches WHILE ?
        DD      FORWARDP ; WHILE target.
        DD      SEMIS
;

;  **********
;  *   IF   *
;  **********
;
    ALIGN     4
N_LIF:   DD      2
        DB      "IF"
    ALIGN     4
LIF:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    REPEA-(CW*(C_HOFFSET))
        DD    N_LIF
    DD    0

        DD      LIT, ZBRAN, COMMA, PFORWARD
        DD      TWO     ; Magic number
        DD      SEMIS
;

;  ************
;  *   ELSE   *
;  ************
;
    ALIGN     4
N_LELSE:   DD      4
        DB      "ELSE"
    ALIGN     4
LELSE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    LIF-(CW*(C_HOFFSET))
        DD    N_LELSE
    DD    0

        DD      QCOMP, TWO, QPAIR
        DD      LIT, BRAN, COMMA, PFORWARD
        DD      SWAP
        DD      FORWARDP
        DD      TWO     ; Magic number
        DD      SEMIS
;

;  *************
;  *   WHILE   *
;  *************
;
    ALIGN     4
N_LWHILE:   DD      5
        DB      "WHILE"
    ALIGN     4
LWHILE:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    B_IMMED
        DD    LELSE-(CW*(C_HOFFSET))
        DD    N_LWHILE
    DD    0

        DD      TOR    ;  Save backward target.
        DD      TOR
        DD      LIT, ZBRAN, COMMA, PFORWARD
        DD      LIT, 4 ; Magic number
        DD      FROMR
        DD      FROMR
        DD      SEMIS
;

;  **************
;  *   SPACES   *
;  **************
;
    ALIGN     4
N_SPACES:   DD      6
        DB      "SPACES"
    ALIGN     4
SPACES:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LWHILE-(CW*(C_HOFFSET))
        DD    N_SPACES
    DD    0

        DD      ZERO
        DD      MAX
        DD      ZERO
        DD     XQDO
        DD      SPAX1-$-CW
SPAX2:  DD      SPACE
        DD     XLOOP
        DD      SPAX2-$-CW    ;LOOP
SPAX1:
        DD      SEMIS
;

;  **********
;  *   <#   *
;  **********
;
    ALIGN     4
N_BDIGS:   DD      2
        DB      "<#"
    ALIGN     4
BDIGS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SPACES-(CW*(C_HOFFSET))
        DD    N_BDIGS
    DD    0

        DD      PAD
        DD      HLD
        DD      STORE
        DD      SEMIS
;

;  **********
;  *   #>   *
;  **********
;
    ALIGN     4
N_EDIGS:   DD      2
        DB      "#>"
    ALIGN     4
EDIGS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BDIGS-(CW*(C_HOFFSET))
        DD    N_EDIGS
    DD    0

        DD      DROP
        DD      DROP
        DD      HLD
        DD      FETCH
        DD      PAD
        DD      OVER
        DD      LSUB
        DD      SEMIS
;

;  ************
;  *   SIGN   *
;  ************
;
    ALIGN     4
N_SIGN:   DD      4
        DB      "SIGN"
    ALIGN     4
SIGN:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    EDIGS-(CW*(C_HOFFSET))
        DD    N_SIGN
    DD    0

        DD      ZLESS
        DD      ZBRAN
        DD      SIGN1-$-CW ;IF
        DD      LIT,2DH
        DD      HOLD    ;THEN
SIGN1:  DD      SEMIS
;

;  *********
;  *   #   *
;  *********
;
    ALIGN     4
N_DIG:   DD      1
        DB      "#"
    ALIGN     4
DIG:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    SIGN-(CW*(C_HOFFSET))
        DD    N_DIG
    DD    0

        DD      BASE
        DD      FETCH
        DD      MSMOD
        DD      ROT
        DD      LIT,9
        DD      OVER
        DD      LESS
        DD      ZBRAN
        DD      DIG1-$-CW  ;IF
        DD      LIT,7
        DD      PLUS    ;THEN
DIG1:   DD      LIT,30H
        DD      PLUS
        DD      HOLD
        DD      SEMIS
;

;  **********
;  *   #S   *
;  **********
;
    ALIGN     4
N_DIGS:   DD      2
        DB      "#S"
    ALIGN     4
DIGS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DIG-(CW*(C_HOFFSET))
        DD    N_DIGS
    DD    0

DIGS1:  DD      DIG     ;BEGIN
        DD      OVER
        DD      OVER
        DD      LOR
        DD      ZEQU
        DD      ZBRAN
        DD      DIGS1-$-CW ;UNTIL
        DD      SEMIS
;

;  *************
;  *   (D.R)   *
;  *************
;
    ALIGN     4
N_PDDOTR:   DD      5
        DB      "(D.R)"
    ALIGN     4
PDDOTR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DIGS-(CW*(C_HOFFSET))
        DD    N_PDDOTR
    DD    0

        DD      TOR
        DD      SWAP
        DD      OVER
        DD      DABS
        DD      BDIGS
        DD      DIGS
        DD      ROT
        DD      SIGN
        DD      EDIGS
        DD      FROMR
        DD      OVER
        DD      LSUB, ZERO, MAX
        DD      ZERO
        DD     XQDO
        DD      PDDOT1-$-CW
PDDOT2:  DD      LBL, HOLD  ;WARNING: HOLD outside of #>.
        DD     XLOOP
        DD      PDDOT2-$-CW
PDDOT1:
        DD      EDIGS  ;Drop string instead of number.
        DD      SEMIS
;

;  ***********
;  *   D.R   *
;  ***********
;
    ALIGN     4
N_DDOTR:   DD      3
        DB      "D.R"
    ALIGN     4
DDOTR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    PDDOTR-(CW*(C_HOFFSET))
        DD    N_DDOTR
    DD    0

        DD      PDDOTR
        DD      LTYPE
        DD      SEMIS
;

;  **********
;  *   .R   *
;  **********
;
    ALIGN     4
N_DOTR:   DD      2
        DB      ".R"
    ALIGN     4
DOTR:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DDOTR-(CW*(C_HOFFSET))
        DD    N_DOTR
    DD    0

        DD      TOR
        DD      STOD
        DD      FROMR
        DD      DDOTR
        DD      SEMIS
;

;  **********
;  *   D.   *
;  **********
;
    ALIGN     4
N_DDOT:   DD      2
        DB      "D."
    ALIGN     4
DDOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DOTR-(CW*(C_HOFFSET))
        DD    N_DDOT
    DD    0

        DD      ZERO
        DD      DDOTR
        DD      SPACE
        DD      SEMIS
;

;  *********
;  *   .   *
;  *********
;
    ALIGN     4
N_DOT:   DD      1
        DB      "."
    ALIGN     4
DOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DDOT-(CW*(C_HOFFSET))
        DD    N_DOT
    DD    0

        DD      STOD
        DD      DDOT
        DD      SEMIS
;

;  *********
;  *   ?   *
;  *********
;
    ALIGN     4
N_QUES:   DD      1
        DB      "?"
    ALIGN     4
QUES:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DOT-(CW*(C_HOFFSET))
        DD    N_QUES
    DD    0

        DD      FETCH
        DD      DOT
        DD      SEMIS
;

;  **********
;  *   U.   *
;  **********
;
    ALIGN     4
N_UDOT:   DD      2
        DB      "U."
    ALIGN     4
UDOT:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    QUES-(CW*(C_HOFFSET))
        DD    N_UDOT
    DD    0

        DD      ZERO
        DD      DDOT
        DD      SEMIS
;

;  *****************
;  *   FOR-WORDS   *
;  *****************
;
    ALIGN     4
N_FORW:   DD      9
        DB      "FOR-WORDS"
    ALIGN     4
FORW:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    UDOT-(CW*(C_HOFFSET))
        DD    N_FORW
    DD    0

        DD      SWAP
        DD      TOR
        DD      TOR
FORW1:  DD      FROMR
        DD      RR
        DD      OVER
        DD      TLFA
        DD      FETCH
        DD      TOR
        DD      EXEC
        DD      RR
        DD      ZEQU
        DD      ZBRAN
        DD      FORW1-$-CW
        DD      RDROP
        DD      RDROP
        DD      SEMIS
;

;  ****************
;  *   FOR-VOCS   *
;  ****************
;
    ALIGN     4
N_FORV:   DD      8
        DB      "FOR-VOCS"
    ALIGN     4
FORV:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FORW-(CW*(C_HOFFSET))
        DD    N_FORV
    DD    0

        DD      TOR
        DD      VOCL
        DD      FETCH
        DD      TOR
FORV1:  DD      FROMR
        DD      RR
        DD      OVER
        DD      TVFA
        DD      FETCH
        DD      TOR
        DD      EXEC
        DD      RR
        DD      ZEQU
        DD      ZBRAN
        DD      FORV1-$-CW
        DD      RDROP
        DD      RDROP
        DD      SEMIS
;

;  *************
;  *   WORDS   *
;  *************
;
    ALIGN     4
N_WORDS:   DD      5
        DB      "WORDS"
    ALIGN     4
WORDS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    FORV-(CW*(C_HOFFSET))
        DD    N_WORDS
    DD    0

        DD      CSLL
        DD      LOUT
        DD      STORE
        DD      LIT, IDDOT
        DD      CONTEXT
        DD      FETCH
        DD      FORW
        DD      SEMIS
;
;


;  ***********
;  *   BYE   *
;  ***********
;
    ALIGN     4
N_BYE:   DD      3
        DB      "BYE"
    ALIGN     4
BYE:        DD    $+(CW*(PH_OFFSET-C_HOFFSET))
        DD    $+(CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    WORDS-(CW*(C_HOFFSET))
        DD    N_BYE
    DD    0

; EXIT TO PC-DOS, if run from PC-DOS, otherwise hang or whatever.
        MOV     EBX,[(XCODE+(CW*(PH_OFFSET-C_HOFFSET)))]


        MOV      LONG[SPSAVE],ESP
        AND     EAX,EAX
        MOV     ESP,EAX

        JMP     GDT_SWITCH: $+3+CW+M4_SWITCHOFFSET
        MOV EAX,CR0
        DEC AL
        MOV CR0,EAX            ;set real mode
        BITS   16
        MOV     AX,SWITCHSEGMENT
        MOV     DS,AX
        MOV     ES,AX
        MOV     AX,SS_RST ; Make stack valid
        MOV     SS,AX
        STI
RETDOSV: JMP 0:0        ; Filled in during boot
        BITS   32

;

;  *****************
;  *   EXIT-CODE   *
;  *****************
;
    ALIGN     4
N_XCODE:   DD      9
        DB      "EXIT-CODE"
    ALIGN     4
XCODE:        DD    DOVAR
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    BYE-(CW*(C_HOFFSET))
        DD    N_XCODE
    DD    0

        DD      0
;
;
;

;  ************
;  *   LIST   *
;  ************
;
    ALIGN     4
N_LLIST:   DD      4
        DB      "LIST"
    ALIGN     4
LLIST:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    XCODE-(CW*(C_HOFFSET))
        DD    N_LLIST
    DD    0

        DD      SCR,STORE
        DD      SKIP
         DD      6
SB5: DB      "SCR # "
       ALIGN     4
        DD      LIT, SB5
        DD      LIT, 6
        DD      LTYPE
        DD      BASE, FETCH
        DD      DECA
        DD      SCR, FETCH, DOT
        DD      BASE, STORE
        DD      SCR, FETCH, BLOCK
        DD      LIT,1024
LLIST1: DD      LIT, ALF, SSPLIT
        DD      CR, LTYPE
        DD      OVER,ZEQU ;DUP would not show a last empty line!
        DD      ZBRAN
        DD      LLIST1-$-CW
        DD      TDROP
        DD      SEMIS
;

;  *************
;  *   INDEX   *
;  *************
;
    ALIGN     4
N_INDEX:   DD      5
        DB      "INDEX"
    ALIGN     4
INDEX:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LLIST-(CW*(C_HOFFSET))
        DD    N_INDEX
    DD    0

        DD      LIT,AFF
        DD      EMIT,CR
        DD      ONEP,SWAP
        DD     XDO
        DD      INDE9-$-CW
INDE1:  DD      CR,IDO
        DD      LIT,3
        DD      DOTR,SPACE
        DD      ZERO,IDO
        DD      PLINE, LTYPE, KEYQ
        DD      ZBRAN
        DD      INDE2-$-CW
        DD      LLEAV
INDE2:  DD     XLOOP
        DD      INDE1-$-CW
INDE9:
        DD      SEMIS
;

;  **********
;  *   .S   *
;  **********
;
    ALIGN     4
N_DOTS:   DD      2
        DB      ".S"
    ALIGN     4
DOTS:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    INDEX-(CW*(C_HOFFSET))
        DD    N_DOTS
    DD    0

        DD      CR
        DD      LIT, 'S', EMIT
        DD      LIT, ASO, EMIT
        DD      SPACE
        DD      SPFET, SZERO, FETCH
DOC2:   DD      OVER, OVER,  EQUAL, ZEQU
        DD      ZBRAN
        DD      DOC1-$-CW
        DD      ZERO, CELLP, LSUB, LDUP, FETCH, DOT
        DD      BRAN
        DD      DOC2-$-CW
DOC1:    DD DROP, DROP
        DD      LIT, ASC, EMIT
        DD SEMIS
;

;  ********************
;  *   ENVIRONMENT?   *
;  ********************
;
    ALIGN     4
N_ENVQ:   DD      12
        DB      "ENVIRONMENT?"
    ALIGN     4
ENVQ:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    DOTS-(CW*(C_HOFFSET))
        DD    N_ENVQ
    DD    0

        DD      LIT, ENV, TWID, PFIND
        DD      TOR, TDROP, FROMR
        DD      LDUP
        DD      ZBRAN
        DD      ENVQ1-$-CW
        DD      EXEC
        DD      LIT, -1
ENVQ1:
        DD      SEMIS
;


;  *************
;  *   TRIAD   *
;  *************
;
    ALIGN     4
N_TRIAD:   DD      5
        DB      "TRIAD"
    ALIGN     4
TRIAD:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    ENVQ-(CW*(C_HOFFSET))
        DD    N_TRIAD
    DD    0

        DD      LIT,AFF
        DD      EMIT
        DD      LIT,3
        DD      SLASH
        DD      LIT,3
        DD      STAR
        DD      LIT,3
        DD      OVER,PLUS
        DD      SWAP
        DD     XDO
        DD      TRIA9-$-CW
TRIA1:  DD      CR,IDO
        DD      LLIST
        DD      KEYQ
        DD      ZBRAN
        DD      TRIA2-$-CW
        DD      LLEAV   ;LEAVE
TRIA2:  DD     XLOOP
        DD      TRIA1-$-CW    ;THEN
TRIA9:
        DD      CR
        DD      ZERO, MESS
        DD      SEMIS
;
;
; This word is not even fig!

;  ***************
;  *   .SIGNON   *
;  ***************
;
    ALIGN     4
N_SIGNON:   DD      7
        DB      ".SIGNON"
    ALIGN     4
SIGNON:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    TRIAD-(CW*(C_HOFFSET))
        DD    N_SIGNON
    DD    0

; PRINT CPU TYPE (8088)
        DD      CR
        DD      BASE,FETCH
        DD      LIT,36, BASE,STORE
        DD      LCPU, DDOT
        DD      BASE,STORE
;
        DD      LNAME, LTYPE, SPACE
        DD      LVERSION, LTYPE, SPACE
        DD      CR
        DD      SEMIS
;

;

;  **************
;  *   LOW-DP   *
;  **************
;
    ALIGN     4
N_LOWDP:   DD      6
        DB      "LOW-DP"
    ALIGN     4
LOWDP:        DD    DOUSE
        DD    (CW*(16))
        DD    0H
        DD    SIGNON-(CW*(C_HOFFSET))
        DD    N_LOWDP
    DD    0

;

;  **************
;  *   LOW-EM   *
;  **************
;
    ALIGN     4
N_LOWEM:   DD      6
        DB      "LOW-EM"
    ALIGN     4
LOWEM:        DD    DOUSE
        DD    (CW*(17))
        DD    0H
        DD    LOWDP-(CW*(C_HOFFSET))
        DD    N_LOWEM
    DD    0

;
;
;
;**** LAST DICTIONARY WORD ****

;  ************
;  *   TASK   *
;  ************
;
    ALIGN     4
N_TASK:   DD      4
        DB      "TASK"
    ALIGN     4
TASK:        DD    DOCOL
        DD    $ + (CW*(PH_OFFSET-D_HOFFSET))
        DD    0H
        DD    LOWEM-(CW*(C_HOFFSET))
        DD    N_TASK
    DD    0

        DD      SEMIS
;

TEXTEND  EQU     $       ; Show end of dictionary.
INITDP   EQU     110000H ;Where we want new words.
ACTUAL_EM EQU    EM  ; Different for relocatable code only.
 ;

%if 0

The remaining memory ( up to 'EM' ) is
used for:

        1. EXTENSION DICTIONARY
        2. PARAMETER STACK
        3. TERMINAL INPUT BUFFER
        4. RETURN STACK
        5. USER VARIABLE AREA
        6. DISK BUFFERS (UNLESS REQUIRED <1 MBYTE)


%endif

;
;

 ;    ENDS
        ;
%if 0

  MISC. NOTES AND SCATTERED THOUGHTS

- Remember that all the FORTH words in this version are
  upper case letters.  Use <CAPS LOCK> when in FORTH.

;

- Subscribe to FORTH Dimensions.  It is a valuable source
  of system and application ideas.  Talking with fellow
  FORTH programmers is sure to stir up some exciting ideas.
  Consider joining a FIG chapter.  See the back of FORTH
  Dimensions for more info.

%endif

; Define the entry point, not valid for auto booting.
        ;     ORIG























;
