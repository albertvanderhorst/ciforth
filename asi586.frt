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
0 2        00 01000000 'O W, CFAO COMMAER OW,    ( obligatory word     )
0 0 CELL+  00   800000 'O ,  CFAO COMMAER (RX,) ( cell relative to IP )
0 1        00   400000 'O C, CFAO COMMAER (RB,) ( byte relative to IP )
0 2        00   200000 'O W, CFAO COMMAER SG,   (  Segment: WORD      )
0 1        00   100000 'O C, CFAO COMMAER P,    ( port number ; byte     )
0 1        00   080000 'O C, CFAO COMMAER IS,    ( Single -obl-  byte )
0 0 CELL+  02   040000 'O ,  CFAO COMMAER IX,   ( immediate data : cell)
0 1        01   040000 'O C, CFAO COMMAER IB,   ( immediate byte data)
0 0 CELL+  08   020000 'O ,  CFAO COMMAER X,    ( immediate data : address/offset )
0 1        04   020000 'O C, CFAO COMMAER B,    ( immediate byte : address/offset )
0 1        00   010000 'O C, CFAO COMMAER SIB,,   ( Most bizarre     )


( #################### TO BE PHASED OUT ############################### )
( The `INCONSISTENCY-PAIRS' is a remnant from the time BY and BA where  )
( in one byte. It is now used to run assemblers that have still the     )
( old conventions. It identifies the `BA' part.                         )
3FFF  VARIABLE INCONSISTENCY-PAIRS 
: SPLIT
    DUP INCONSISTENCY-PAIRS @ AND SWAP INCONSISTENCY-PAIRS @ INVERT AND ;

( As `T!' but has BABY BI information, multiplexed as per               )
( `INCONSISTENSY-PAIRS'                                                 )
: T!' >R SPLIT R> T! ;
( As `1PI' but has BABY BI INST information. )
: 1PI' >R >R SPLIT R> R> 1PI ;
: 2PI' >R >R SPLIT R> R> 2PI ;
: 3PI' >R >R SPLIT R> R> 3PI ;
: xFIR' >R >R SPLIT R> R> xFIR ;
( #################### TO BE PHASED OUT  END ########################## )

( Meaning of the bits in TALLY-BA :                                     )
( Inconsistent:  1 OPERAND IS BYTE     2 OPERAND IS CELL                )
(                4 OFFSET   DB|        8 ADDRESS      DW|               )
( By setting 20 an opcode can force a memory reference, e.g. CALLFARO  )
(               10 Register op         20 Memory op                    )
(               40 D0|                 80 [BP]' {16} [BP]      {32}     )
(  sib:       0100 no ..             0200 [AX +8*| DI]               )
(  logical    0400 no ..             0800 Y| Y'| Z| Z'|              )
(  segment    1000 no ..             2000 ES| ..                        )
( test/debug 40000 no ..            80000 CR0 ..DB0

( Names *ending* in primes BP|' -- not BP'| the prime registers -- are  )
( only valid for 16 bits real mode, in combination with an address      )
( overwite. Use W, L, and end the line in TALLY! to defeat checks.      )

( Like xFIR but without any checks and unfindable for the disassembler  )
( Use for 16 bit mode instructions.                                     )
: xFIR16   CHECK31 <BUILDS , , , , DOES> FIXUP< ;
: xFAMILY|R16   0 DO   DUP >R T@ R> xFIR16  OVER + LOOP DROP DROP ;

0200 3800 0s T!'
 0800 0s 0 8 xFAMILY|R AX] CX] DX] BX] 0] BP] SI] DI]
0200 C000 0s T!'
 4000 0s 0 4 xFAMILY|R  +1* +2* +4* +8*
0200 10700 0s T!'
 0100 0s 0 8 xFAMILY|R [AX [CX [DX [BX [SP -- [SI [DI
000280 10700 0s 0500 0s xFIR' [BP   ( Fits in the hole, safe inconsistency check)
020240 10700 0s 0500 0s xFIR' [MEM  ( Fits in the hole, safe inconsistency check)

0120 0700 0s T!'
  0100 0s 0 8 
    xFAMILY|R16 [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]' [SI]' [DI]' -- [BX]'
A0 0 0720 0s 0600 0s xFIR16 [BP]'  ( Fits in the hole, safe inconsistency check)
 0100 0s 0000 0s 4 xFAMILY|R [AX] [CX] [DX] [BX]
010120 0700 0s 0400 0s xFIR' ~SIB|   ( Fits in the hole, requires also ~SIB, )
0001A0 0700 0s 0500 0s xFIR' [BP]   ( Fits in the hole, safe inconsistency check)
 0100 0s 0600 0s 2 xFAMILY|R [SI] [DI]

0111 0700 0s T!'
 0100 0s 0 8 xFAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
0112 0700 0s T!'
 0100 0s 0 8 xFAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
0160 000000 C000 0s 0000 0s xFIR      D0|
0124 020000 C000 0s 4000 0s xFIR      DB|
0128 020000 C000 0s 8000 0s xFIR      DW|
0110 000000 C000 0s C000 0s xFIR      R|
0008 020000 C700 0s 0600 0s xFIR16    MEM|' ( Overrules D0| [BP]')
0108 020000 C700 0s 0500 0s xFIR      MEM| ( Overrules D0| [BP] )

041101 0 3800 0s T!
 0800 0s 0 8 xFAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
041102 0 3800 0s T!
 0800 0s 0 8 xFAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
042100 0  3800 0s T!   0800 0s 0 0s 6 xFAMILY|R ES| CS| SS| DS| FS| GS|
080002 0 3801 0s T!   ( 3)
 0800 0s 0000 0s 5 xFAMILY|R CR0| -- CR2| CR3| CR4|                 ( 3)
 0800 0s 0001 0s 8 xFAMILY|R DR0| DR1| DR2| DR3| DR4| DR5| DR6| DR7| ( 3)

0000 0 0002 0s T!   0002 0s 0 0s 2 xFAMILY|R F| T|
040401 0 01 0s 0 0s xFIR B|
040402 0 01 0s 1 0s xFIR X|
( MODERNIZED TILL HERE )

( --------- These must be found last -------)
0600 1FF 00 1PI' ~SIB,
( --------- two fixup operands ----------)
041000 0 FF03 T!
 0008 0000 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
041000 0 FF01 T! 
 0002 0084 2 2FAMILY, TEST, XCHG,
041000 0 FF03 0088 2PI MOV,
1022 FF00 008D 2PI' LEA,
1022 FF00 T!'   0001 00C4 2 2FAMILY, L|ES, L|DS,
1022 FF00 0062 2PI' BOUND,  ( 3)
1002 FF00 0063 2PI' ARPL,   ( 3)
041002 FF00 0069 2PI' IMULI, ( 3)
081002 FF00 006B 2PI' IMULSI, ( 3)
1002 FF0000 T!' 100 00020F 2 3FAMILY, LAR, LSL, ( 3)
1002 FF0000 T!' 800 00A30F 4 3FAMILY, BT, BTS, BTR, BTC, ( 3)
1002 FF0000 T!' 800 00A50F 2 3FAMILY, SHLD|C, SHRD|C,    ( 3)
1002 FF0000 T!' 100 00BC0F 2 3FAMILY, BSF, BSR,          ( 3)
081002 FF0000 T!' 800 00A40F 2 3FAMILY, SHLDI, SHRDI,    ( 3)
1022 FF0000 T!' 100 00B20F 4 3FAMILY, L|SS, -- L|FS, L|GS, ( 3)
1501 FF0000 T!' 800 00B60F 2 3FAMILY, MOVZX|B, MOVSX|B,  ( 3)
1502 FF0000 T!' 800 00B70F 2 3FAMILY, MOVZX|W, MOVSX|W,  ( 3)
1002 FF0000 00AF0F 3PI' IMUL,                     ( 3)
( --------- one fixup operands ----------)
040000 C701 00C6 2PI' MOVI,
0012 07 T!'   08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
12 07 90 1PI' XCHG|AX,
040011 07 B0 1PI' MOVI|BR,
040012 07 B8 1PI' MOVI|XR,
040000 C701 T!'
 0800 0080 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
080002 C700 T!'
 0800 0083 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
0000 C701 T!'
 0800 10F6 6 2FAMILY, NOT, NEG, MUL|AD, IMUL|AD, DIV|AD, IDIV|AD,
 0800 00FE 2 2FAMILY, INC, DEC,
040000 C701 00F6 2PI' TESTI,
02 C700 008F 2PI' POP,
02 C700 30FF 2PI' PUSH,
02 C700 T!'  1000 10FF 2 2FAMILY, CALLO, JMPO,
22 C700 T!'  1000 18FF 2 2FAMILY, CALLFARO, JMPFARO,
080002 C70000 T!'  080000 20BA0F 4 3FAMILY, BTI, BTSI, BTRI, BTCI, ( 3) 
02 C70000 T!' ( It says X but in fact W : descriptor mostly - ) ( 3)  
  080000 00000F 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,  ( 3) 
22 C70000 T!' ( It says X but in fact memory of different sizes) ( 3)  
  080000 00010F 7 3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, -- LMSW,       ( 3) 

( --------- no fixup operands ----------)
01 20100 0s 0000 0s xFIR' B'|
02 20100 0s 0100 0s xFIR' X'|
020008 201 T!'    02 A0 2 1FAMILY, MOV|TA, MOV|FA,
040000 201 T!'
 08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
00 201 A8 1PI' TESTI|A,
00 201 T!'  02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
100000 0201 T!'   02 E4 2 1FAMILY, IN|P, OUT|P,
000000 0201 T!'   02 EC 2 1FAMILY, IN|D, OUT|D,
000000 0201 T!'   02 6C 2 1FAMILY, INS, OUTS,   ( 3)

( --------- special fixups ----------)

0800     10100 0s T!'   0100 0s 0 0s 2 xFAMILY|R Y| N|
0800     40E00 0s T!'   0200 0s 0 0s 8 xFAMILY|R O| C| Z| CZ| S| P| L| LE|
400800 50F 70 1PI' J,

2102 FF02 08C 2PI' MOV|SG,

00 20002 0s 00 0s xFIR' 1|   00 20002 0s 02 0s xFIR' V|          ( 3) 
0100 2C703 T!' ( 20000 is a lockin for 1| V|)                   ( 3) 
 0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,  ( 3) 
80012 0 3F0300 C0200F 3PI  MOV|CD,  ( 3)

800800 50F00 800F 2PI' J|X,                                           ( 3)
0800 0001 0s T!'   01 0s 0 2 xFAMILY|R Y'| N'|                          ( 3)
0800 000E 0s T!'   02 0s 0 8 xFAMILY|R O'| C'| Z'| CZ'| S'| P'| L'| LE'| ( 3)
0901 C70F00 00900F 3PI' SET,  ( 3)

( --------- no fixups ---------------)

2000 0 0 T!  08 06 4 1FAMILY, PUSH|ES, PUSH|CS, PUSH|SS, PUSH|DS,  
2000 0 0 T!  08 07 4 1FAMILY, POP|ES, -- POP|SS, POP|DS,  

040001 00 T!'    01 D4 2 1FAMILY, AAM, AAD, 
040001 00 CD 1PI' INT,
220008 00 9A 1PI' CALLFAR,
220008 00 EA 1PI' JMPFAR,
01000000 00 T!'   08 C2 2 1FAMILY, RET+, RETFAR+,
800004 00 T!'   01 E8 2 1FAMILY, CALL, JMP,
400000 00 EB 1PI' JMPS,
400000 00 T!'   01 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
00 00 T!'
   08   26 4 1FAMILY, ES:, CS:, SS:, DS:,
   08   27 4 1FAMILY, DAA, DAS, AAA, AAS,
   01   98 8 1FAMILY, CBW, CWD, -- WAIT, PUSHF, POPF, SAHF, LAHF,
   08   C3 2 1FAMILY, RET,  RETFAR,
   01   CC 4 1FAMILY, INT3, -- INTO, IRET,
   01   F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
   01   F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD,
   01   60 2 1FAMILY, PUSH|ALL, POP|ALL, ( 3)
   01   64 4 1FAMILY, FS:, GS:, OS:, AS:, ( 3)
 0100 A00F 3 2FAMILY, PUSH|FS, POP|FS, CPUID,
 0100 A80F 2 2FAMILY, PUSH|GS, POP|GS, ( RSM,)
  040002 00   68 1PI' PUSHI|X,  ( 3)
  040001 00   6A 1PI' PUSHI|B,  ( 3)
01040001 00   C8 1PI' ENTER, ( 3)
      00 00   C9 1PI' LEAVE, ( 3)
      00 00   D7 1PI' XLAT,  ( 3)
      00 00 060F 2PI' CLTS,  ( 3)

( ############## HANDLING THE SIB BYTE ################################ )

( Handle a `sib' bytes as an instruction-within-an-instruction )
( This is really straightforward, we say the sib commaer is a sib       )
( instruction. as per -- error checking omitted -- " 10000 'O ~SIB, CFAO  )
( COMMAER SIB,,"                                                        )
( All the rest is to nest the state in this recursive situation:        )
( 900 are the bad bits conflicting with ~SIB,                           )
( Leaving BY would flag commaers to be done after the sib byte as errors)
: (SIB),,   
    TALLY-BA @   TALLY-BY @   !TALLY      ( . -- state1 state2 )
    ~SIB,   
    TALLY-BY ! 900 INVERT AND TALLY-BA @ OR TALLY-BA ! ;

 'O (SIB),, CFAO   % SIB,, >DATA !   ( Not available during  generation)

( Disassemble the sib byte where the disassembler sits now.             )
( [ `F-D' takes care itself of incrementing the disassembly pointer. ]  )
: DIS-SIB [ % ~SIB, ] LITERAL F-D ;
( Disassembler was not available while creating the commaer. )
 'O DIS-SIB CFAO   % SIB,, >DIS !    0   % SIB,, >CNT !   

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
 ASSEMBLER DEFINITIONS
( ############## 8086 ASSEMBLER POST ################################## )

