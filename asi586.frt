( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)

 ASSEMBLER DEFINITIONS HEX

( ############## 80386 ASSEMBLER ADDITIONS ############################ )
( The patch for the assembler doesn't belong in the generic part        )
( To be used when overruling, e.g. prefix)
: lsbyte, DUP C, 0008 RSHIFT ;
: W, lsbyte, lsbyte, DROP ;
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

( Al fixups-from-reverse are at most 0002 bytes, so they all end in     )
( 0000 To improve readability we use the ignore ``,'' from ciforth.     )

( ############## 80386 ASSEMBLER PROPER ############################### )
( The decreasing order means that a decompiler hits them in the         )
( right order                                                           )
( Fields: a disassembly XT,      LENGTH to comma, the BA BY information )
( and the XT that puts data in the dictionary.                          )
( Where there is a placeholder ``_'' the execution token is filled in   )
( later. )

0 2 0000 0100 ' W, COMMAER OW,    ( obligatory word     )
0 4 0000 0080 ' ,  COMMAER (RX,) ( cell relative to IP )
0 1 0000 0040 ' C, COMMAER (RB,) ( byte relative to IP )
0 2 0000 0020 ' W, COMMAER SG,   (  Segment: WORD      )
0 1 0000 0010 ' C, COMMAER P,    ( port number ; byte     )
0 1 0000 0008 ' C, COMMAER IS,    ( Single -obl-  byte )
0 4 0002 0004 ' ,  COMMAER IX,   ( immediate data : cell)
0 1 0001 0004 ' C, COMMAER IB,   ( immediate byte data)
0 4 0008 0002 ' ,  COMMAER X,    ( immediate data : address/offset )
0 1 0004 0002 ' C, COMMAER B,    ( immediate byte : address/offset )
_ 1 0000 0001 _    COMMAER SIB,, ( An instruction with in an instruction )


( Meaning of the bits in TALLY-BA :                                     )
( Inconsistent:  0001 OPERAND IS BYTE     0002 OPERAND IS CELL          )
(                0004 OFFSET   DB|        0008 ADDRESS      DW          )
( By setting 0020 an opcode can force a memory reference, e.g. CALLFARO )
(               0010 Register op         0020 Memory op                 )
(               0040 D0|                 0080 [BP]' {16} [BP]      {32} )
(  sib:       0100 no ..             0200 [AX +8*| DI]                  )
(  logical    0400 no ..             0800 Y| Y'| Z| Z'|                 )
(  segment    1000 no ..             2000 ES| ..                        )
( test/debug 4,0000 no ..            8,0000 CR0 ..DB0                   )

( Names *ending* in primes BP|' -- not BP'| the prime registers -- are  )
( only valid for 0016 bits real mode, in combination with an address    )
( overwite. Use W, L, and end the line in TALLY! to defeat checks.      )

( Like xFIR but without any checks and unfindable for the disassembler  )
( Use for 0016 bit mode instructions.                                     )
: xFIR16   CHECK31 CREATE-- , , , , DOES> FIXUP< ;
: xFAMILY|R16   0000 DO   DUP >R T@ R> xFIR16  OVER + LOOP DROP DROP ;

0200 0 3800,0000 T!
 0800,0000 0000 8 xFAMILY|R AX] CX] DX] BX] 0] BP] SI] DI]
0200 0 C000,0000 T!
 4000,0000 0000 4 xFAMILY|R  +1* +2* +4* +8*
0200 0 0700,0001 T!
 0100,0000 0000 8 xFAMILY|R [AX [CX [DX [BX [SP -- [SI [DI
0280 00 0700,0001 0500,0000 xFIR [BP   ( Fits in the hole, safe inconsistency check)
0240 02 0700,0001 0500,0000 xFIR [MEM  ( Fits in the hole, safe inconsistency check)

0120 0 0700,0000 T!
  0100,0000 0000 8
    xFAMILY|R16 [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]' [SI]' [DI]' -- [BX]'
00A0 0000 0720,0000 0600,0000 xFIR16 [BP]'  ( Fits in the hole, safe inconsistency check)
 0100,0000 0000,0000 0004 xFAMILY|R [AX] [CX] [DX] [BX]
0120 01 0700,0000 0400,0000 xFIR ~SIB|   ( Fits in the hole, requires also ~SIB, )
01A0 00 0700,0000 0500,0000 xFIR [BP]   ( Fits in the hole, safe inconsistency check)
 0100,0000 0600,0000 0002 xFAMILY|R [SI] [DI]

0111 0 0700,0000 T!
 0100,0000 0000 8 xFAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
0112 0 0700,0000 T!
 0100,0000 0000 8 xFAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
0160 00 C000,0000 0000,0000 xFIR      D0|
0124 02 C000,0000 4000,0000 xFIR      DB|
0128 02 C000,0000 8000,0000 xFIR      DW|
0110 00 C000,0000 C000,0000 xFIR      R|
0008 02 C700,0000 0600,0000 xFIR16    MEM|' ( Overrules D0| [BP]')
0108 02 C700,0000 0500,0000 xFIR      MEM| ( Overrules D0| [BP] )

04,1101 0000 3800,0000 T!
 0800,0000 0000 8 xFAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
04,1102 0000 3800,0000 T!
 0800,0000 0000 8 xFAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
04,2100 0000  3800,0000 T!   0800,0000 0,0000 0006 xFAMILY|R ES| CS| SS| DS| FS| GS|
08,0002 0000 3801,0000 T!   ( 3)
 0800,0000 0000,0000 0005 xFAMILY|R CR0| -- CR2| CR3| CR4|                 ( 3)
 0800,0000 0001,0000 0008 xFAMILY|R DR0| DR1| DR2| DR3| DR4| DR5| DR6| DR7| ( 3)

0000 0000 0002,0000 T!   0002,0000 0,0000 0002 xFAMILY|R F| T|
04,0401 0000 01,0000 0,0000 xFIR B|
04,0402 0000 01,0000 1,0000 xFIR X|

( --------- These must be found last -------)
0600 0 01FF 0000 1PI ~SIB,
( --------- two fixup operands ----------)
04,1000 0000 FF03 T!
 0008 0000 0008 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
04,1000 0000 FF01 T!
 0002 0084 0002 2FAMILY, TEST, XCHG,
04,1000 0000 FF03 0088 2PI MOV,
1022 0 FF00 008D 2PI LEA,
1022 0 FF00 T!   0001 00C4 0002 2FAMILY, LES, LDS,
1022 0 FF00 0062 2PI BOUND,  ( 3)
1002 0 FF00 0063 2PI ARPL,   ( 3)
1002 04 FF00 0069 2PI IMULI, ( 3)
1002 08 FF00 006B 2PI IMULSI, ( 3)
1002 0 FF,0000 T! 0100 00,020F 0002 3FAMILY, LAR, LSL, ( 3)
1002 0 FF,0000 T! 0800 00,A30F 0004 3FAMILY, BT, BTS, BTR, BTC, ( 3)
1002 0 FF,0000 T! 0800 00,A50F 0002 3FAMILY, SHLD|C, SHRD|C,    ( 3)
1002 0 FF,0000 T! 0100 00,BC0F 0002 3FAMILY, BSF, BSR,          ( 3)
1002 08 FF,0000 T! 0800 00,A40F 0002 3FAMILY, SHLDI, SHRDI,    ( 3)
1022 0 FF,0000 T! 0100 00,B20F 0004 3FAMILY, LSS, -- LFS, LGS, ( 3)
1501 0 FF,0000 T! 0800 00,B60F 0002 3FAMILY, MOVZX|B, MOVSX|B,  ( 3)
1502 0 FF,0000 T! 0800 00,B70F 0002 3FAMILY, MOVZX|W, MOVSX|W,  ( 3)
1002 0 FF,0000 00,AF0F 3PI IMUL,                     ( 3)
( --------- one fixup operands ----------)
0 04 C701 00C6 2PI MOVI,
0012 0 0007 T!   0008 40 0004 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
0012 0 0007 90 1PI XCHG|AX,
0011 04 0007 B0 1PI MOVI|BR,
0012 04 0007 B8 1PI MOVI|XR,
0 04 C701 T!
 0800 0080 0008 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
0002 08 C700 T!
 0800 0083 0008 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
0000 0 C701 T!
 0800 10F6 0006 2FAMILY, NOT, NEG, MUL|AD, IMUL|AD, DIV|AD, IDIV|AD,
 0800 00FE 0002 2FAMILY, INC, DEC,
0 04 C701 00F6 2PI TESTI,
0002 0 C700 008F 2PI POP,
0002 0 C700 30FF 2PI PUSH,
0002 0 C700 T!  1000 10FF 0002 2FAMILY, CALLO, JMPO,
0022 0 C700 T!  1000 18FF 0002 2FAMILY, CALLFARO, JMPFARO,
0002 08 C7,0000 T!  08,0000 20,BA0F 0004 3FAMILY, BTI, BTSI, BTRI, BTCI, ( 3)
0002 0 C7,0000 T! ( It says X but in fact W : descriptor mostly - ) ( 3)
  08,0000 00,000F 0006 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,  ( 3)
0022 0 C7,0000 T! ( It says X but in fact memory of different sizes) ( 3)
  08,0000 00,010F 0007 3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, -- LMSW,       ( 3)

( --------- no fixup operands ----------)
0001 0 0100,0002 0000,0000 xFIR B'|
0002 0 0100,0002 0100,0000 xFIR X'|
0008 02 0201 T!    0002 A0 0002 1FAMILY, MOV|TA, MOV|FA,
0 04 0201 T!
 0008 04 0008 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
0000 0 0201 00A8 1PI TESTI|A,
0000 0 0201 T!  0002 A4 0006 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
0 10 0201 T!   0002 E4 0002 1FAMILY, IN|P, OUT|P,
0 00 0201 T!   0002 EC 0002 1FAMILY, IN|D, OUT|D,
0 00 0201 T!   0002 6C 0002 1FAMILY, INS, OUTS,     ( 3)

( --------- special fixups ----------)

0800     0000 0100,0001 T!   0100,0000 0,0000 0002 xFAMILY|R Y| N|
0800     0000 0E00,0004 T!   0200,0000 0,0000 0008 xFAMILY|R O| C| Z| CZ| S| P| L| LE|
0800 40 050F 0070 1PI J,

2102 0 FF02 008C 2PI MOV|SG,

0000 0 0002,0002 00,0000 xFIR 1|   0000 0 0002,0002 02,0000 xFIR V|          ( 3)
0100 0 2,C703 T! ( 2,0000 is a lockin for 1| V|)                   ( 3)
 0800 00D0 0008 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,  ( 3)
8,0012 0000 3F,0300 C0,200F 3PI  MOV|CD,  ( 3)

0800 80 5,0F00 800F 2PI J|X,                                           ( 3)
0800 0 0001,0000 T!   01,0000 0000 2 xFAMILY|R Y'| N'|                          ( 3)
0800 0 000E,0000 T!   02,0000 0000 8 xFAMILY|R O'| C'| Z'| CZ'| S'| P'| L'| LE'| ( 3)
0901 0 C7,0F00 00,900F 3PI SET,  ( 3)

( --------- no fixups ---------------)

2000 0000 0 T!  0008 06 0004 1FAMILY, PUSH|ES, PUSH|CS, PUSH|SS, PUSH|DS,
2000 0000 0 T!  0008 07 0004 1FAMILY, POP|ES, -- POP|SS, POP|DS,

0001 04 0000 T!    0001 D4 0002 1FAMILY, AAM, AAD,
0001 04 0000 CD 1PI INT,
0008 22 0000 9A 1PI CALLFAR,
0008 22 0000 EA 1PI JMPFAR,
0 0100 0000 T!   0008 C2 0002 1FAMILY, RET+, RETFAR+,
0004 80 0000 T!   0001 E8 0002 1FAMILY, CALL, JMP,
0 40 0000 EB 1PI JMPS,
0 40 0000 T!   0001 E0 0004 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
0000 0 0000 T!
   0008   0026 0004 1FAMILY, ES:, CS:, SS:, DS:,
   0008   0027 0004 1FAMILY, DAA, DAS, AAA, AAS,
   0001   0098 0008 1FAMILY, CBW, CWD, -- WAIT, PUSHF, POPF, SAHF, LAHF,
   0008   00C3 0002 1FAMILY, RET,  RETFAR,
   0001   00CC 0004 1FAMILY, INT3, -- INTO, IRET,
   0001   00F0 0006 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
   0001   00F8 0006 1FAMILY, CLC, STC, CLI, STI, CLD, STD,
   0001   0060 0002 1FAMILY, PUSH|ALL, POP|ALL, ( 3)
   0001   0064 0004 1FAMILY, FS:, GS:, OS:, AS:, ( 3)
 0100 A00F 0003 2FAMILY, PUSH|FS, POP|FS, CPUID,
 0100 A80F 0002 2FAMILY, PUSH|GS, POP|GS, ( RSM,)
  0002 04 0000   0068 1PI PUSHI|X,  ( 3)
  0001 04 0000   006A 1PI PUSHI|B,  ( 3)
  0001 0104 0000   00C8 1PI ENTER, ( 3)
      0000 0 00   00C9 1PI LEAVE, ( 3)
      0000 0 00   00D7 1PI XLAT,  ( 3)
      0000 0 00 060F 2PI CLTS,  ( 3)

( ############## HANDLING THE SIB BYTE ################################ )

( Handle a `sib' bytes as an instruction-within-an-instruction )
( This is really straightforward, we say the sib commaer is a sib       )
( instruction. as per -- error checking omitted -- " 1,0000 ' ~SIB, >CFA )
( COMMAER SIB,,"                                                        )
( All the rest is to nest the state in this recursive situation:        )
( 0900 are the bad bits conflicting with ~SIB,                           )
( Leaving BY would flag commaers to be done after the sib byte as errors)
: (SIB),,
    TALLY-BA @   TALLY-BY @   !TALLY      ( . -- state1 state2 )
    ~SIB,
    TALLY-BY ! 0900 INVERT AND TALLY-BA @ OR TALLY-BA ! ;

 ' (SIB),,   % SIB,, >DATA !   ( Fill in deferred data creation action  )

( Disassemble the sib byte where the disassembler sits now.             )
( [ `FORCED-DISASSEMBLY' takes care itself of incrementing the          )
(   disassembly pointer. ]                                              )
: DIS-SIB [ % ~SIB, ] LITERAL FORCED-DISASSEMBLY ;
( Fill in deferred disassembler action.                                 )
 ' DIS-SIB    % SIB,, >DIS !    0000   % SIB,, >CNT !

( Redefine some fixups, such that the user may say                      )
( "[AX" instead of " ~SIB| SIB,, [AX"                                   )
( Note that the disassembly is made to look like the same. The ~SIB|    )
( and the ~SIB, inside the SIB,, are print-suppressed.                  )
: [AX   ~SIB| SIB,, [AX ;       : [SP   ~SIB| SIB,, [SP ;
: [CX   ~SIB| SIB,, [CX ;       : [BP   ~SIB| SIB,, [BP ;
: [DX   ~SIB| SIB,, [DX ;       : [SI   ~SIB| SIB,, [SI ;
: [BX   ~SIB| SIB,, [BX ;       : [DI   ~SIB| SIB,, [DI ;
: [MEM  ~SIB| SIB,, [MEM ;

( ############## 8086 ASSEMBLER PROPER END ############################ )
( You may want to use these always instead of (RB,)
    : RB, ISS @ - (RB,) ;      : RX, ISS @ - (RX,) ;

PREVIOUS DEFINITIONS DECIMAL
( ############## 8086 ASSEMBLER POST ################################## )
