( $Id: )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( ############## 8086 ASSEMBLER ADDITIONS ############################# )
( The patch ofr the assembler doesn't belong in the generic part        )
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
0 CELL+  ' ,  CFA  800000 COMMAER (RX,) ( cell relative to IP )
1        ' C, CFA  400000 COMMAER (RB,) ( byte relative to IP )
2        ' W, CFA  200000 COMMAER SG,   (  Segment: WORD      )
1        ' C, CFA  100000 COMMAER P,    ( port number ; byte     )
2        ' OW, CFA  80000 COMMAER W,    ( obligatory word     )
0 CELL+  ' ,  CFA   40002 COMMAER IX,   ( immediate data : cell)
1        ' C, CFA   40001 COMMAER IB,   ( immediate byte data)
0 CELL+  ' ,  CFA   20008 COMMAER X,    ( immediate data : address/offset )
1        ' C, CFA   20004 COMMAER B,    ( immediate byte : address/offset )

FFFF INCONSISTENCY-PAIRS !
( Inconsistent:  1 OPERAND IS BYTE     2 OPERAND IS CELL                )
(                4 OFFSET   DB|        8 ADDRESS      DW|               )
( By setting 20 an opcode can force a memory reference, e.g. CALLFARO  )
(               10 Register op        20 Memory op                    )
(               40 D0                 80 [BP]' {16} [BP] {32}         )

( Only valid for 16 bits real mode, in combination with an address
  overwite. Use W, L, and end the line in TALLY! to defeat checks.
20 0700 0s T!
 0100 0s 0 8 xFAMILY|R [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]' [SI]' [DI]' -- [BX]'
A0 0700 0s 0600 0s xFIR [BP]'  ( Fits in the hole, safe inconsistency check) 
 0100 0s 0000 0s 4 xFAMILY|R [AX] [CX] [DX] [BX] 
010020 0700 0s 0400 0s xFIR [SIB]   ( Fits in the hole, requires also SIB, ) 
0000A0 0700 0s 0500 0s xFIR [BP]   ( Fits in the hole, safe inconsistency check) 
 0100 0s 0600 0s 2 xFAMILY|R [SI] [DI]

12 0700 0s T!
 0100 0s 0 8 xFAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
11 0700 0s T!
 0100 0s 0 8 xFAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|

000060 C000 0s 0000 0s xFIR      D0|
020024 C000 0s 4000 0s xFIR      DB|
020028 C000 0s 8000 0s xFIR      DW|
000010 C000 0s C000 0s xFIR      R|
020008 C700 0s 0600 0s xFIR      MEM|' ( Overrules D0| [BP]')
020008 C700 0s 0500 0s xFIR      MEM| ( Overrules D0| [BP] )

02 3800 0s T!
 0800 0s 0 8 xFAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
01 3800 0s T!
 0800 0s 0 8 xFAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|

00 FF 00 1PI ((SIB)),
: (SIB), TALLY @ !TALLY ((SIB)), 
  30 XOR ( Toggle from using memory to registers)
  TALLY ! ;
1        ' (SIB), CFA   10000 COMMAER SIB, ( Most bizarre     )

00 0002 0s T!   0002 0s 0 0s 2 xFAMILY|R F| T|
01 0001 0s 0 0s xFIR B|
02 0001 0s 1 0s xFIR W|

( --------- two fixup operands ----------)
00 FF03 T!
 0008 0000 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
00 FF01 T!
 0002 0084 2 2FAMILY, TEST, XCHG,
00 FF03 0088 2PI MOV,
22 FF00 008D 2PI LEA,
22 FF00 T!   0001 00C4 2 2FAMILY, LES, LDS,
00 FF01 00C6 2PI MOVI,

( --------- one fixup operands ----------)
12 07 T!   08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
12 07 90 1PI XCHG|AX,
040011 07 B0 1PI MOVI|BR,
040012 07 B8 1PI MOVI|XR,
040000 C701 T!
 0800 0080 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
040001 C700 T!
 0800 0083 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
0000 C701 T!
 0800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,
 0800 00FE 2 2FAMILY, INC, DEC,
040000 C701 00F6 2PI TESTI,
02 C700 008F 2PI POP,
02 C700 30FE 2PI PUSH,
02 C700 T!  1000 10FF 2 2FAMILY, CALLO, JMPO,
22 C700 T!  1000 18FF 2 2FAMILY, CALLFARO, JMPFARO,

( --------- no fixup operands ----------)
01 20100 0s 0000 0s xFIR B'|
02 20100 0s 0100 0s xFIR W'|
020008 201 T!    02 A0 2 1FAMILY, MOVTA, MOVFA,
040000 201 T!
 08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
00 201 A8 1PI TESTI|A,
00 201 T!  02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
100000 201 T!   02 E4 2 1FAMILY, IN|P, OUT|P,
000000 201 T!   02 EC 2 1FAMILY, IN|D, OUT|D,

( --------- special fixups ----------)

00 10100 0s T!   0100 0s 0 0s 2 xFAMILY|R Y| N|
00 00E00 0s T!   0200 0s 0 0s 8 xFAMILY|R O| C| Z| CZ| S| P| L| LE|
400000 10F 70 1PI J,

00 1800 0s T!   0800 0s 0 0s 4 xFAMILY|R ES| CS| SS| DS|
00 18 T!   01 06 2 1FAMILY, PUSH|SG, POP|SG,
02 DF02 08C 2PI MOV|SG,

00 10002 0s 00 0s xFIR 1|   00 10002 0s 02 0s xFIR V|
0 1C703 T!
(    0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,         )
 0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, SAL, SAR,

( --------- no fixups ---------------)

040001 00 CD 1PI INT,
220008 00 9A 1PI CALLFAR,
220008 00 EA 1PI JMPFAR,
080002 00 T!   08 C2 2 1FAMILY, RET+, RETFAR+,
800004 00 T!   01 E8 2 1FAMILY, CALL, JMP,
400000 00 EB 1PI JMPS,
00 00 T!
 08 26 4 1FAMILY, ES:, CS:, SS:, DS:,
 08 27 4 1FAMILY, DAA, DAS, AAA, AAS,
 01 98 8 1FAMILY, CBW, CWD, -- WAIT, PUSHF, POPF, SAHF, LAHF,
 08 C3 2 1FAMILY, RET,  RETFAR,
 01 CC 4 1FAMILY, INT3, -- INTO, IRET,
 01 D4 4 1FAMILY, AAM, AAD, -- XLAT,
 01 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
 01 F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
 01 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD,

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
