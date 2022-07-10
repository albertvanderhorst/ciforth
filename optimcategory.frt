\ $Id: optimcategory.frt,v 5.17 2020/06/04 16:54:27 albert Exp $
\ Copyright (2012): Albert van der Horst {by GNU Public License}

\ This code is part of the optimiser for ciforth Forth code.
\ It contains the categories that determine which operations
\ notably commutations are allowed with certain assembler PIFU's.

\ In the following L-size means the instruction size such as
\ found in the 80386 without Q/E prefix, but including a 0F prefix.

\ Some categories are to be found in the analyseras.frt :
\ POPS PUSHES

\ The notation XXX>< is used to indicated things that commute
\ with XXX. NOT-XXX>< are things that do not commute.
\ Commutation can be blocked by common registers, separately.
\ Note that a MOVI can be removed at the end
\ of a basic block, as long as the register is dead there,
\ because it doesn't affect flags.
ASSEMBLER
DATA NOT-MOVI>< HERE 0 ,
' MOVI|X,    ,
' MOVI|B,    ,          ' REPZ,      ,
HERE SWAP !

\ We already have the `PUSHES and `POPS. Maybe there is more.
DATA NOT-PUSHI>< HERE 0 ,
' MOVI|X,    ,
HERE SWAP !

\ Instruction that have an implied register not apparent from the
\ disassembly in `DISS and not AX/AL.
DATA IMPLIED-CATEGORY  HERE 0 ,
' REPZ,      ,          ' STOS,      ,          ' LODS,      ,
' SHL,       ,          ' SHR,       ,
' SCAS,      ,          ' CMPS,      ,          ' MOVS,      ,
' OUTS,      ,          ' INS,       ,          ' OUT|D,     ,
' IN|D,      ,          ' SCAS,      ,          ' INT,       ,
HERE SWAP !

DATA REGISTER-PAIRS HERE  0 ,
 ' AX| ,  ' CX| ,  ' DX| ,  ' BX| ,  ' SP| ,  ' BP| ,  ' SI| ,  ' DI| ,
 ' AX'| , ' CX'| , ' DX'| , ' BX'| , ' SP'| , ' BP'| , ' SI'| , ' DI'| ,
 ' [AX] , ' [CX] , ' [DX] , ' [BX] ,   0000 , ' [BP] , ' [SI] , ' [DI] ,
HERE SWAP !

\ For  register  find the corresponding primed  register .
: reg->reg'   REGISTER-PAIRS BAG-WHERE 8 CELLS + @ ;
\ For  register  find the corresponding indirect register .
: reg->[reg]   REGISTER-PAIRS BAG-WHERE 16 CELLS + @ ;
\ For primed  register  find the corresponding unprimed register.
: reg'->reg   REGISTER-PAIRS BAG-WHERE 8 CELLS - @ ;

\ For  register  return its  number  0..7.
\ FIXME : must also handle primed registers!
: r>#   PIFU'! DAT-R ;
: r'>#   reg'->reg PIFU'! DAT-R ;
REGRESS 'DI| r># S: 7
REGRESS 'DI'| r'># S: 7

\ A register bitmask to be called modmask contains two halves of 32 bits.
\ The lower half is source, i.e. the instruction uses it.
\ The upper half is destination, i.e. the instruction changes it.
\ Note that pupo is considered one instruction for this idea.
\ The bit numbering is as per the basic rm content.
\ A ghost register adds 8 to the bit number of the non-ghost register.
\ Bit 16 corresponds to flags. Bit 17 corresponds to the carry flag.
\ Registers and flags can be used and changed at the same time.

\ The  instruction  : "  is  forbidden to commute with `MOVI|X,"
: MOVI><FORBIDDEN?   DUP NOT-MOVI>< BAG-WHERE SWAP
    IMPLIED-CATEGORY BAG-WHERE OR   0<> ;
REGRESS 'REPZ, MOVI><FORBIDDEN? S: TRUE
\ The  instruction  : "  is  forbidden to commute with `PUSHI|X,"
: PUSHI><FORBIDDEN? >R  R@ PUSHES BAG-WHERE  R@ POPS BAG-WHERE OR
  R@ NOT-PUSHI>< BAG-WHERE OR  0<>   RDROP ;
REGRESS 'PUSH, PUSHI><FORBIDDEN? S: TRUE
\ FIXME the jury is still out
\ REGRESS 'MOVI|X, PUSHI><FORBIDDEN? S: TRUE
REGRESS 'MOV, PUSHI><FORBIDDEN? S: FALSE

\ Overview of the regular instructions.
\ Regular instruction have 3 L-sizes and up to two operands.
\ PUSHI is a one byte instruction with zero operands.
\ Two and three byte instructions have one operand in the N position,
\ and an optional source or target (F| resp. T|) in the ' position.
\ This is the last byte masked by 7 or C8.
\ This leaves three orthogonal properties:
\ L-size, nr of operands and immediacy. SZ1 SZ2 SZ3 OP0 OP1 OP2 IMM
\ The properties OP0 OP1 OP2 guarantee regularity and can be used
\ to find the registers (in combination with Q:)
\ Remember F| T| refers to the primed register that cannot
\ be replaced by a memory reference.
\ Preferable the case is F| , so the primed register is source.
\ For LEA, the primed register is always target.
\ zero reg, one reg, two reg, Q prefix,
\ T| , immediate data, irregular, terminates a basic block,
\ wants relative 32 bit addressing mode for data,
\ one byte instruction, two byte instruction, stack involved.
\ RM3 a mod/rm byte is in third position, implies 4 byte instr with Q.
\  ' is source  N is destination, normally, unless #DD
\ RGL : a regular instruction:
\  -it is size 1 and the one register is in the N position
\  or -it is another size and the one registers is in the N position
\ in the last byte.
\  or -it is another size and the two registers are in ' and N position
\ in the last byte.
\ FIXME : introduce RM2
\ The following masks indicate properties that can be OR-ed.
\ OP0
\ OP1
\ OP2
\ QE : a Qx: or Ex: prefix
\ #SN : two operands: direction bit set
\     w.r.t. N  always : it is source and replacable, or not used.
\ '-reg or ['-reg] is normally destination, overruled by #S'
\      one operand:  N is source, and replacable by content
\ #S' : two operands: primed register is source whatever #SN
\     a two source instruction is possible, like CMP TEST and with XO|
\ #DD : direction bit set T| RR'|
\ IMM : immediate data
\ IRR  : defies categorization
\ EBB  : ends a basic block branch/call/int
\ SZ1
\ SZ2
\ SZ3
\ #STK : uses the stack implicitly (not:access to SP as a register)

13 ENUM-MASK OP0 OP1 OP2 QE #DD IMM IRR EBB SZ1 SZ2 SZ3 #STK #S'
'#DD ALIAS #SN
IMM SZ1 OR OP1 OR CONSTANT IMM1
\ These instructions have one operand that is both source and destination
\ With the exception of TESTI and CMPI
IMM SZ2 OR OP1 OR CONSTANT IMM2
\ These instructions have one operand that is both source and destination
OP1 SZ2 OR CONSTANT RG1-2
\ OP2 CONSTANT RG2
OP2 SZ2 OR CONSTANT RG2
RG2 #S' OR CONSTANT RG2'
RG2 #SN OR CONSTANT RG2N
RG2 #SN OR #S' OR CONSTANT RG2N'
OP2 SZ3 OR CONSTANT RG3
\ #STK  IMM OP0 OR OR CONSTANT #PSHI
#STK OP1 OR SZ1 OR CONSTANT STK  \ With ~IMM
#STK OP0 OR SZ1 OR IMM OR CONSTANT #PSHI
OP0 OP1 OP2   OR OR CONSTANT REGULAR  \ All registers can be determined.
STK CONSTANT #POP
STK #DD OR CONSTANT #PUSH
\ This indicates in what respect an instruction is irregular.
\ Indicates: use of AX, of segment, of C, and needs further subdivision.
'IRR ALIAS IRRA   'IRR ALIAS IRRS   'IRR ALIAS IRRC  'IRR ALIAS IRRF
'IRR ALIAS IRRZ ( Unknown, zero)

\ Note that the SZ3 instructions are not accomodated by this table.

\ They can possibly be found after detecting that the first byte is
\ the 0F escape.        #S' OR
DATA opcode-properties
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 0x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 0x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 1x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 1x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 2x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 2x
RG2' , RG2' , RG2N , RG2N , IRRA , IRRA , IRRZ , IRRZ ,   \ 3x
RG2N' , RG2N' , RG2N' , RG2N' , IRRA , IRRA , IRRZ , IRRZ ,   \ 3x
QE , QE , QE , QE , QE , QE , QE , QE ,           \ 4x
QE , QE , QE , QE , QE , QE , QE , QE ,           \ 4x
#PUSH , #PUSH ,  #PUSH , #PUSH ,  #PUSH , #PUSH ,  #PUSH , #PUSH ,  \ 5x
#POP , #POP ,  #POP , #POP ,  #POP , #POP ,  #POP , #POP ,  \ 5x
IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ ,                   \ 6x
#PSHI ,  IRRZ , #PSHI ,  IRRZ , IRRZ , IRRZ , IRRZ , IRRZ ,     \ 6x
EBB , EBB , EBB , EBB , EBB , EBB , EBB , EBB ,                   \ 7x  jumps
EBB , EBB , EBB , EBB , EBB , EBB , EBB , EBB ,                   \ 7x
IMM2 , IMM2 , IRRZ , IRRA , RG2N' , RG2N' , RG2 , RG2 , \ 8x,
RG2' , RG2' , RG2N , RG2N , IRRZ , RG2 , IRRZ , RG1-2 ,  \ 8x
IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA ,   \ 9x
IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ ,                   \ 9x
IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA ,   \ Ax
IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA , IRRA ,   \ Ax
IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 ,      \ Bx
IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 , IMM1 ,      \ Bx
IRRZ , IRRZ , EBB , EBB , EBB , EBB , RG2 , RG2 ,               \ Cx
IRRZ , IRRZ , EBB , EBB , EBB , EBB , EBB , EBB ,                   \ Cx
RG1-2 , RG1-2 , IRR , IRR , IRR , IRR , IRRZ , IRR ,         \ Dx
IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ , IRRZ ,                   \ Dx
EBB , EBB , EBB , EBB , IRRZ , IRRZ , IRRZ , IRRZ ,               \ Ex  jumps and out
EBB , EBB , EBB , EBB , IRRZ , IRRZ , IRRZ , IRRZ ,               \ Ex  jumps and out
IRRZ , IRRZ , IRRZ , IRRZ , EBB , IRRZ , IRRF , IRRF ,  \ Fx
IRRZ , IRRZ , IRRZ , IRRZ , IRRZ   , IRRZ , IRRF , RG1-2 ,  \ Fx  some
DATA prop-F6/F7
IMM2 , IRRZ , RG1-2 , RG1-2 , IRR , IRR , IRR , IRR ,

DATA prop-FE
RG1-2 IMM OR , RG1-2 , EBB , EBB , EBB , EBB , RG1-2 , IRRZ ,

\ For  instr
: prop@   CELLS opcode-properties + @ ;
\ For   pointer-to-q-instruction  get:  properties  mask.
: Qp@   1+ C@ prop@ ;
REGRESS HERE ADC, F| !TALLY C@ prop@ S: RG2'
REGRESS HERE ADC, T| !TALLY C@ prop@ S: RG2N
REGRESS HERE ADCI, HERE OVER - !TALLY SWAP C@ prop@ S: 2 IMM2
\ REGRESS HERE MOVI, HERE OVER - !TALLY SWAP C@ prop@ S:
REGRESS HERE MOVI|X, HERE OVER - !TALLY SWAP C@ prop@ S: 1 IMM1
REGRESS HERE MOV, F| HERE OVER - !TALLY SWAP C@ prop@ S: 2 RG2'
REGRESS HERE MOV, T| HERE OVER - !TALLY SWAP C@ prop@ S: 2 RG2N
\ REGRESS HERE XCHG, HERE OVER - !TALLY SWAP C@ prop@ S: 2 IRR
REGRESS HERE QN: HERE OVER - !TALLY SWAP C@ prop@ S: 1 QE

\ For instr at  addr  :"Its reg's  can  be determined regularly"
: REGULAR?   C@+ prop@ QE = >R   C@ prop@ REGULAR AND 0<>
  R> AND ;
\ For tests: see IS-MOVI-COMPAT?
\ The rules are
\                              commutes
\   RG2   RG2    A->B C->D     unless B=D or B=C
\   RG2   OP1-x  A->B C        unless B=C
\   OP1-x RG2-x  C A->B        unless B=C
\   OP1-x OP1-x  B D           unless B=D
\ IF #DD the operands for RG2 must be interchanged.
\ From Morse 8086 Primer,  pg 154.
\ : enumm!  opcode-properties + OR!U ;
\ : doit
\ 100 0 DO
\    I 0F0 AND 40 =  IF QE I enumm! THEN
\    I 0F0 AND 40 <  IF  \ ADD c.s
\         I 07 AND DUP
\         0 4 WITHIN IF
\           RG2 I enumm!
\           I 2 AND IF #DD I enumm! THEN
\         THEN
\         4 6 WITHIN IF
\             IMM1 I enumm!
\         THEN
\    THEN
\    I 0F8 AND 80 = IF  \ ADDI c.s
\        RG1-2 I enumm!
\        IMM1 I enumm!
\    THEN
\    I 0F0 AND 80 = IF  \ MOV LEA TEST XCGH
\         I 0F AND 4 0C WITHIN IF
\             RG2 I enumm!
\             I F8 AND 88 =  I 2 AND   AND IF #DD I enumm! THEN
\    THEN THEN
\    I 0A8 =   I 0A9 = OR IF
\         IMM1 I enumm!
\    THEN
\    I 0F0 AND 0B0 =   IF
\         IMM1 I enumm!
\         RG1-2 I enumm!
\    THEN
\    I 0C6 =  I 0C7 = OR IF
\         IMM1 I enumm!
\    THEN
\ LOOP
\ ; doit \ LATEST HIDDEN
DECIMAL
PREVIOUS
