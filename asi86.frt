( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)

                          HEX
 ASSEMBLER DEFINITIONS

( ############## 8086 ASSEMBLER ADDITIONS ############################# )
( The patch for the assembler doesn't belong in the generic part        )
( To be used when overruling, e.g. prefix)
: lsbyte, 0 100 UM/MOD SWAP C, ;
: W, lsbyte, lsbyte, DROP ;
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;
: IS, C, ;
( There are some fixups-from-reverse that are larger than 2 l bytes.    )
( Using the out of mask bit trick, that has to be eliminated.           )
." This 8086 assembler runs only on 32 bits systems!" CR

( ############## 8086 ASSEMBLER PROPER ################################ )
( The decreasing order means that a decompiler hits them in the         )
( right order                                                           )
0 0 CELL+  0 8000 ' ,  >CFA  COMMAER (RX,) ( cell relative to IP )
0 1        0 4000 ' C, >CFA  COMMAER (RB,) ( byte relative to IP )
0 2        0 2000 ' W, >CFA  COMMAER SG,   (  Segment: WORD      )
0 1        0 1000 ' C, >CFA  COMMAER P,    ( port number ; byte     )
0 1        0  800 ' C, >CFA  COMMAER IS,   ( immediate byte data, obligatory size)
0 0 CELL+  2  400 ' ,  >CFA  COMMAER IX,   ( immediate data : cell)
0 1        1  400 ' C, >CFA  COMMAER IB,   ( immediate byte data)
0 0 CELL+  8  200 ' ,  >CFA  COMMAER X,    ( immediate data : address/offset )
0 1        4  200 ' C, >CFA  COMMAER B,    ( immediate byte : address/offset )
0 2        0  100 ' W, >CFA  COMMAER W,    ( obligatory word     )
( Bits in TALLY  1 OPERAND IS BYTE     2 OPERAND IS CELL                )
(                4 OFFSET   DB|        8 ADDRESS      DW|               )
( By setting 20 an opcode can force a memory reference, e.g. CALLFARO  )
(               10 Register op        20 Memory op                    )
(               40 D0                 80 [BP]                         )
( Only valid for 16 bits real mode  A0JUL04 AvdH )
20 0 07 T!R
 01 0 8 FAMILY|R [BX+SI] [BX+DI] [BP+SI] [BP+DI] [SI] [DI] -- [BX]
A0 0 07 06 FIR [BP]  ( Fits in the hole, safe incompatibility)
12 0 07 T!R
 01 0 8 FAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
11 0 07 T!R
 01 0 8 FAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|

60 000  C0 00 FIR      D0|
24 200  C0 40 FIR      DB|
28 200  C0 80 FIR      DW|
10 000  C0 C0 FIR      R|
08 200  C7 06 FIR      MEM|
 ( Overrules D0| [BP] )

02 00 38 T!R
 08 0 8 FAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
01 00 38 T!R
 08 0 8 FAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|

00 00 0200 T!R   0200 0 2 FAMILY|R F| T|
01 00 0100 0000 FIR B|
02 00 0100 0100 FIR W|

( --------- two fixup operands ----------)
00 00 FF03 T!
 0008 0000 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
00 00 FF01 T!
 0002 0084 2 2FAMILY, TEST, XCHG,
00 00 FF03 0088 2PI MOV,
22 00 FF00 008D 2PI LEA,
22 00 FF00 T!   0001 00C4 2 2FAMILY, LES, LDS,
00 0400 C701 00C6 2PI MOVI,

( --------- one fixup operands ----------)
12 00 07 T!   08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
12 00 07 90 1PI XCHG|AX,
11 0400 07 B0 1PI MOVI|BR,
12 0400 07 B8 1PI MOVI|XR,
00 0400 C701 T!
 0800 0080 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
02 0800 C700 T!
 0800 0083 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
00 00 C701 T!
 0800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,
 0800 00FE 2 2FAMILY, INC, DEC,
00 0400 C701 00F6 2PI TESTI,
02 00 C700 008F 2PI POP,
02 00 C700 30FE 2PI PUSH,
02 00 C700 T!  1000 10FF 2 2FAMILY, CALLO, JMPO,
22 00 C700 T!  1000 18FF 2 2FAMILY, CALLFARO, JMPFARO,

( --------- no fixup operands ----------)
01 00 02000001 00 FIR B'|
02 00 02000001 01 FIR W'|
8 0200 201 T!    02 A0 2 1FAMILY, MOVTA, MOVFA,
0 0400 201 T!
 08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
00 00 201 A8 1PI TESTI|A,                                                                                               020
00 00 201 T!  02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,                                                         040
00 1000 201 T!   02 E4 2 1FAMILY, IN|P, OUT|P,
00 0000 201 T!   02 EC 2 1FAMILY, IN|D, OUT|D,

( --------- special fixups ----------)

00 00 0100,0001 T!R   01 0 2 FAMILY|R Y| N|
00 00 0E T!R   02 0 8 FAMILY|R O| C| Z| CZ| S| P| L| LE|
00 4000 10F 70 1PI J,

00 00 18 T!R   08 0 4 FAMILY|R ES| CS| SS| DS|
00 00 18 T!   01 06 2 1FAMILY, PUSH|SG, POP|SG,
02 00 DF02 08C 2PI MOV|SG,

00 00  0100,0200 0000 FIR 1|
00 800 0100,0200 0200 FIR V|

00 0 01,C703 T!
(    0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,         )
 0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, SAL, SAR,

( --------- no fixups ---------------)

01 0400 00 CD 1PI INT,
0A 2200 00 9A 1PI CALLFAR,
0A 2200 00 EA 1PI JMPFAR,
02 0100 00 T!   08 C2 2 1FAMILY, RET+, RETFAR+,
04 8000 00 T!   01 E8 2 1FAMILY, CALL, JMP,
00 4000 00 EB 1PI JMPS,
00 0000 00 T!
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
(   : RW, ISS @ - (RW,} ;      : RL, ISS @ - (RL,} ;                    )
(   : D0|  ' [BP] REJECT D0|  ;                                         )
(   : [BP] ' D0|  REJECT [BP] ;                                         )
(   : R| ' LES, REJECT 'O LDS REJECT R| ;                               )
(   : NEXT                                                              )
(        LODS, W'|                                                      )
(        MOV, W| F| AX'| R| BX|                                         )
(        JMPO, D0| [BX]                                                 )
(    ;                                                                  )
( ############## 8086 ASSEMBLER POST ################################## )
( CODE JAN MOV|SG, T| ES| R| AX| ENDCODE                                )

                        DECIMAL
PREVIOUS DEFINITIONS
