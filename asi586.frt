 ( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( ############## 8086 ASSEMBLER ADDITIONS ############################# )
( The patch for the assembler doesn't belong in the generic part        )
( To be used when overruling, e.g. prefix)
: lsbyte, 0 100 U/ SWAP C, ;
: W, lsbyte, lsbyte, DROP ;
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

( Al fixups-from-reverse are at most 2 bytes, so they all end in        )
( 0000 To improve readability we replace `xxxx0000' by `xxxx 0s'       )
: 0s 2 ROTLEFT ;

( ############## 8086 ASSEMBLER PROPER ################################ )
( The decreasing order means that a decompiler hits them in the         )
( right order                                                           )
0 2        ' W, CFA 01000000 COMMAER OW,    ( obligatory word     )
0 0 CELL+  ' ,  CFA   800000 COMMAER (RX,) ( cell relative to IP )
0 1        ' C, CFA   400000 COMMAER (RB,) ( byte relative to IP )
0 2        ' W, CFA   200000 COMMAER SG,   (  Segment: WORD      )
0 1        ' C, CFA   100000 COMMAER P,    ( port number ; byte     )
0 2        ' C, CFA   080000 COMMAER IS,    ( Sign extended byte )
0 0 CELL+  ' ,  CFA   040002 COMMAER IX,   ( immediate data : cell)
0 1        ' C, CFA   040001 COMMAER IB,   ( immediate byte data)
0 0 CELL+  ' ,  CFA   020008 COMMAER X,    ( immediate data : address/offset )
0 1        ' C, CFA   020004 COMMAER B,    ( immediate byte : address/offset )
0 1        ' C, CFA   010000 COMMAER SIBQ,   ( Most bizarre     )
0 1        ' C, CFA     8000 COMMAER SIMQ,   ( Most bizarre     )

3FFF INCONSISTENCY-PAIRS !
( Inconsistent:  1 OPERAND IS BYTE     2 OPERAND IS CELL                )
(                4 OFFSET   DB|        8 ADDRESS      DW|               )
( By setting 20 an opcode can force a memory reference, e.g. CALLFARO  )
(               10 Register op         20 Memory op                    )
(               40 D0| SIM|            80 [BP]' {16} [BP] SIB| {32}     )
(  sib:       0100 no ..             0200 [AX +8*| DI]               )
(  logical    0400 no ..             0800 Y| Y'| Z| Z'|              )
(  segment    1000 no ..             2000 ES| ..                        )
( Only valid for 16 bits real mode, in combination with an address      )
( overwite. Use W, L, and end the line in TALLY! to defeat checks.      )

0200 3800 0s T!
 0800 0s 0 8 xFAMILY|R AX] CX] DX] BX] 0] BP] SI] DI]
0200 C000 0s T!
 4000 0s 0 4 xFAMILY|R  +1* +2* +4* +8*
0200 10700 0s T!
 0100 0s 0 8 xFAMILY|R [AX [CX [DX [BX [SP [BP [SI [DI

0120 0700 0s T!
( 0100 0s 0 8 xFAMILY|R [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]' [SI]' [DI]' -- [BX]'
( A0 0720 0s 0600 0s xFIR [BP]'  ( Fits in the hole, safe inconsistency check)
 0100 0s 0000 0s 4 xFAMILY|R [AX] [CX] [DX] [BX]
0101A0 0700 0s 0400 0s xFIR SIB|   ( Fits in the hole, requires also SIB, )
0001A0 0700 0s 0500 0s xFIR [BP]   ( Fits in the hole, safe inconsistency check)
 0100 0s 0600 0s 2 xFAMILY|R [SI] [DI]

0111 0700 0s T!
 0100 0s 0 8 xFAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
0112 0700 0s T!
 0100 0s 0 8 xFAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
000160 C000 0s 0000 0s xFIR      D0|
020124 C000 0s 4000 0s xFIR      DB|
020128 C000 0s 8000 0s xFIR      DW|
000110 C000 0s C000 0s xFIR      R|
( 020008 C700 0s 0600 0s xFIR      MEM|' ( Overrules D0| [BP]')
0081A0 C700 0s 0400 0s xFIR SIM|   ( Overrules D0| SIB| )
020108 C700 0s 0500 0s xFIR      MEM| ( Overrules D0| [BP] )
1101 3800 0s T!
 0800 0s 0 8 xFAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
0102 3800 0s T!
 0800 0s 0 8 xFAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|

0240 1FF 00 1PI SIB,
0280 0F8 00 1PI SIM,
(   : (SIB}, TALLY @ !TALLY ((SIB}},                                    )
(     30 XOR ( Toggle from using memory to registers}                   )
(     TALLY ! ;                                                         )

( --------- 0F must be found last -------)
2000 1800 0s T!   0800 0s 0 0s 4 xFAMILY|R ES| CS| SS| DS|
2000 18 T!   01 06 2 1FAMILY, PUSH|SG, POP|SG,
0000 0002 0s T!   0002 0s 0 0s 2 xFAMILY|R F| T|
0401 0001 0s 0 0s xFIR B|
0402 0001 0s 1 0s xFIR W|
( --------- two fixup operands ----------)
0000 FF03 T!
 0008 0000 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
0000 FF01 T!
 0002 0084 2 2FAMILY, TEST, XCHG,
00 FF03 0088 2PI MOV,
22 FF00 008D 2PI LEA,
0022 FF00 T!   0001 00C4 2 2FAMILY, L|ES, L|DS,
22 FF00 0062 2PI BOUND,  ( 3)
02 FF00 0064 2PI ARPL,   ( 3)
040002 FF00 0069 2PI IMULI, ( 3)
080002 FF00 006B 2PI IMULSI, ( 3)
02 FF0000 T! 100 00020F 2 3FAMILY, LAR, LSL, ( 3)
02 FF0000 T! 800 00A30F 4 3FAMILY, BT, BTS, BTR, BTC, ( 3)
02 FF0000 T! 800 00A50F 2 3FAMILY, SHLD|C, SHRD|C,    ( 3)
0022 FF0000 T!   001 00B20F 4 3FAMILY, L|SS, -- L|FS, L|GS, ( 3)
1501 FF0000 T! 800 00B60F 2 3FAMILY, MOVZX|B, MOVSX|B,  ( 3)
1502 FF0000 T! 800 00B70F 2 3FAMILY, MOVZX|W, MOVSX|W,  ( 3)
( --------- one fixup operands ----------)
040000 C701 00C6 2PI MOVI,
0012 07 T!   08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
12 07 90 1PI XCHG|AX,
040011 07 B0 1PI MOVI|BR,
040012 07 B8 1PI MOVI|XR,
040000 C701 T!
 0800 0080 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
080002 C700 T!
 0800 0083 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
0000 C701 T!
 0800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,
 0800 00FE 2 2FAMILY, INC, DEC,
040000 C701 00F6 2PI TESTI,
02 C700 008F 2PI POP,
02 C700 30FE 2PI PUSH,
02 C700 T!  1000 10FF 2 2FAMILY, CALLO, JMPO,
22 C700 T!  1000 18FF 2 2FAMILY, CALLFARO, JMPFARO,
080002 C70000 T!   800 20BA0F 4 3FAMILY, BTI, BTSI, BTRI, BTCI, ( 3) 
02 C70000 T! ( It says X but in fact W : descriptor mostly - ) ( 3)  
 1000 0800 00000F 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,  ( 3) 
22 C70000 T! ( It says X but in fact memory of different sizes) ( 3)  
  1000 0800 000F0F 7 3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, -- LMSW,       ( 3) 
( --------- no fixup operands ----------)
01 20100 0s 0000 0s xFIR B'|
02 20100 0s 0100 0s xFIR W'|
020008 201 T!    02 A0 2 1FAMILY, MOVTA, MOVFA,
040000 201 T!
 08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
00 201 A8 1PI TESTI|A,
00 201 T!  02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
100000 0201 T!   02 E4 2 1FAMILY, IN|P, OUT|P,
000000 0201 T!   02 EC 2 1FAMILY, IN|D, OUT|D,
000000 0201 T!   02 6C 2 1FAMILY, INS, OUTS,   ( 3)

( --------- special fixups ----------)

0800     10100 0s T!   0100 0s 0 0s 2 xFAMILY|R Y| N|
0800     40E00 0s T!   0200 0s 0 0s 8 xFAMILY|R O| C| Z| CZ| S| P| L| LE|
400800 50F 70 1PI J,

2102 DF02 08C 2PI MOV|SG,

00 20002 0s 00 0s xFIR 1|   00 20002 0s 02 0s xFIR V|          ( 3) 
0100 2C703 T! ( 20000 is a lockin for 1| V|)                   ( 3) 
 0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, SAL, SAR,  ( 3) 

02 3841 0s T!   ( 40 is the lock-in byte for MOV|CD ..CRx| / DRx| ) ( 3)
 0800 0s 0000 0s 5 xFAMILY|R CR0| -- CR2| CR3| CR4|                 ( 3)
 0800 0s 0001 0s 8 xFAMILY|R DR0| DR1| DR2| DR3| DR4| DR5| DR6| DR7| ( 3)
12 3F4300 C0200F 3PI  MOV|CD,

800800 50F00 800F 2PI J|X,                                           ( 3)
0800 0001 0s T!   01 0s 0 2 xFAMILY|R Y'| N'|                          ( 3)
0800 000E 0s T!   02 0s 0 8 xFAMILY|R O'| C'| Z'| CZ'| S'| P'| L'| LE'| ( 3)
0901 C70F00 00900F 3PI SET,  ( 3)

( --------- no fixups ---------------)

040001 00 CD 1PI INT,
220008 00 9A 1PI CALLFAR,
220008 00 EA 1PI JMPFAR,
01000000 00 T!   08 C2 2 1FAMILY, RET+, RETFAR+,
800004 00 T!   01 E8 2 1FAMILY, CALL, JMP,
400000 00 EB 1PI JMPS,
00 00 T!
   08   26 4 1FAMILY, ES:, CS:, SS:, DS:,
   08   27 4 1FAMILY, DAA, DAS, AAA, AAS,
   01   98 8 1FAMILY, CBW, CWD, -- WAIT, PUSHF, POPF, SAHF, LAHF,
   08   C3 2 1FAMILY, RET,  RETFAR,
   01   CC 4 1FAMILY, INT3, -- INTO, IRET,
   01   D4 4 1FAMILY, AAM, AAD, -- XLAT,
   01   E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
   01   F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
   01   F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD,
   01   60 2 1FAMILY, PUSH|ALL, POP|ALL, ( 3)
   01   64 4 1FAMILY, FS:, GS:, OS:, AS:, ( 3)
 0100 A00F 3 2FAMILY, PUSH|FS, POP|FS, CPUID,
 0100 A80F 2 2FAMILY, PUSH|GS, POP|GS, ( RSM,)
  040002 00   68 1PI PUSHI|X,  ( 3)
  040001 00   6A 1PI PUSHI|B,  ( 3)
01040001 00   C8 1PI ENTER, ( 3)
      00 00   C9 1PI LEAVE, ( 3)
      00 00 060F 2PI CLTS,  ( 3)

( ############## 8086 ASSEMBLER PROPER END ############################ )
( You may always want to use these instead of (RB,)
    : RB, ISS @ - (RB,) ;      : RX, ISS @ - (RX,) ;
    : RW, ISS @ - (RW,) ;      : RL, ISS @ - (RL,) ;
(   : D0|  ' [BP] REJECT D0|  ;                                         )
(   : [BP] ' D0|  REJECT [BP] ;                                         )
(   : R| ' LES, REJECT ' LDS REJECT R| ;                                )
 ASSEMBLER DEFINITIONS
(   : NEXT                                                              )
(        LODS, W1|                                                      )
(        MOV, W| F| AX'| R| BX|                                         )
(        JMPO, D0| [BX]                                                 )
(    ;                                                                  )
( ############## 8086 ASSEMBLER POST ################################## )
CODE JAN MOV|SG, T| ES| R| AX| C;
