( ############## 8086 ASSEMBLER ADDITIONS ############################# )
( The patch ofr the assembler doesn't belong in the generic part        )
( To be used when overruling, e.g. prefix)
: lsbyte, 0 100 U/ SWAP C, ;
: W, lsbyte, lsbyte, DROP ;
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

( Because there are no fixups-from-reverse that are larger than 2       )
( bytes this trick allows to debug -- but not run -- 8086 assembler     )
( of 32 bits system. The pattern 00 01 {$100} to fixup the last bit     )
( becomes 00 00 00 01 {$1000000} on a 16 bit system                     )
: 0s 2 ROTRIGHT ;  ." WARNING : testing version on 8086"                 )
( By defining 0s as a NOP you get a normal 8086 version                 )
( ############## 8086 ASSEMBLER PROPER ################################ )
( The decreasing order means that a decompiler hits them in the         )
( right order                                                           )
0 CELL+  ' ,  CFA  8000 COMMAER (RX,) ( cell relative to IP )
1        ' C, CFA  4000 COMMAER (RB,) ( byte relative to IP )
2        ' W, CFA  2000 COMMAER SG,   (  Segment: WORD      )
1        ' C, CFA  1000 COMMAER P,    ( port number ; byte     )
0 CELL+  ' ,  CFA   208 COMMAER X,    ( immediate data : address/offset )
1        ' C, CFA   204 COMMAER B,    ( immediate byte : address/offset )
0 CELL+  ' ,  CFA   102 COMMAER IX,   ( immediate data : cell)
1        ' C, CFA   101 COMMAER IB,   ( immediate byte data)
( Bits in TALLY  1 OPERAND IS BYTE     2 OPERAND IS CELL                )
(                4 OFFSET   DB|        8 ADDRESS      DW|               )
(               10 Any register        0 Other,indexing               )

( Only valid for 16 bits real mode  A0JUL04 AvdH )
1 .
000 C000 0s 0000 0s xFIR      D0|
11 .
204 C000 0s 4000 0s xFIR      DB|
208 C000 0s 8000 0s xFIR      DW|
00  C000 0s C000 0s xFIR      R|
00 0700 0s T!
 100 0s 0 8 xFAMILY|R [BX+SI] [BX+DI] [BP+SI] [BP+DI] [SI] [DI] [BP] [BX]
 100 0s 0 8 xFAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
 100 0s 0 8 xFAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
2 .
00 3800 0s T!
0800 0s 0 8 xFAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
0208 C700 0s C600 0s xFIR |MEM ( Overrules D0| [BP] )

00 1800 0s T!   800 0s 0 0s 4 xFAMILY|R ES| CS| SS| DS|
00 18 T!     1 6 2 1FAMILY, PUSH|SG, POP|SG,

00 00 T!
 8 26 4 1FAMILY, ES:, CS:, SS:, DS:,
 8 27 4 1FAMILY, DAA, DAS, AAA, AAS,

08 01 T! 02 A0 2 1FAMILY, MOVTA, MOVFA,

3 .
0000 0100 0s T!        100 0s 0 0s 2 xFAMILY|R Y| N|
0000 0E00 0s T!   200 0s 0 0s 8 xFAMILY|R O| C| Z| CZ| S| P| L| LE|
40 0F 70 1PI J,  ( As in J, L| Y| <CALC> S, )
00 07 T!   08 40 4 1FAMILY, INCX, DECX, PUSHX, POPX,
00 07 90 1PI XCHGX,
1 01 0s 0 0s xFIR B| 
2 01 0s 1 0s xFIR W|
00 02 0s T!  2 0s 0 0s 2 xFAMILY|R F| T|
00 FF03 T!
8 0 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
00 FF01 T!
2 84 2 2FAMILY, TEST, XCHG,
00 FF03 088 2PI MOV,
00 FF02 08C 2PI MOVSG,
00 FF00 08D 2PI LEA,
01 0100 0s 000 0s xFIR B'|     02 0100 0s 100 0s xFIR W'|
100 01 T!
 08 04 8 1FAMILY, ADDAI, ORAI, ADCAI, SBBAI, ANDAI, SUBAI, XORAI, CMPAI,
4 .
00 01   A8 1PI TESTAI,
0 FF01 C6 2PI MOVI,
1 0 CD 1PI INT,
00 00 T!
 1 98 8 1FAMILY, CBW, CWD, hole WAIT, PUSHF, POPF, SAHF, LAHF,
28 00 9A 1PI CALLFAR,
0 1 T!  2 A4 6 1FAMILY, MOVS, CMPS, hole STOS, LODS, SCAS,
( 1) 101 7 B0 1PI MOVRI,  ( 2) 102 7 B8 1PI MOVXI,
202 0 T!  8 C2 2 1FAMILY, RET+, RETFAR+,
0 0 T!  8 C3 2 1FAMILY, RET,  RETFAR,
0 FF00 T!   1 C4 2 2FAMILY, LES, LDS,
0 0 T!
 1 CC 4 1FAMILY, INT3, hole INTO, IRET,
 1 D4 4 1FAMILY, AAM, AAD, hole XLAT,
5 .
 1 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
 2 EC 2 1FAMILY, IN|D, OUT|D,
1000 0 T!   2 E4 2 1FAMILY, IN|P, OUT|P,
8004 0 T!   1 E8 2 1FAMILY, CALL, JMP,
2204 00 EA 1PI JMPFAR,
40 0 EB 1PI JMPS,
0 0 T!
 1 F0 6 1FAMILY, LOCK, hole REP, REPZ, HLT, CMC,
 1 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD,
100 C701 T!
 800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
101 C700 T!
 800 83 8 2FAMILY, ADDSI, hole ADCSI, SBBSI, hole SUBSI, hole CMPSI,
0 2 0s 0 0s xFIR 1|   101 2 0s 2 0s xFIR V|
0 C703 T!
800 D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, hole RAR,
0 C701 T!
6 .
800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,
800 FE 2 2FAMILY, INC, DEC,
100 C701 00F6 2PI TESTI,
0 C700 008F 2PI POP,
0 C700 30FE 2PI PUSH,
0 C700 T!   800 10FF 4 2FAMILY, CALLO, CALLFARO, JMPO, JMPFARO,
7 .

( ############## 8086 ASSEMBLER PROPER END ############################ )
( You may always want to use these instead of (RB,)
    : RB, ISS @ - (RB,) ;      : RX, ISS @ - (RX,) ;                    )
    : RW, ISS @ - (RW,) ;      : RL, ISS @ - (RL,) ;                    )
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


