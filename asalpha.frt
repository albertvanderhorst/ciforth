( $Id$)
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Reference Alpha Architecture Handbook Order Number EX-QD2KC-TE )
( Also down loadable as .pdf file from dec sites.                       )
( ############## ALPHA ASSEMBLER PROPER ################################ )
ASSEMBLER DEFINITIONS  HEX

(  General note. The order of this source matters, if only for          )
(  cosmetic reasons. Although all parts of an instruction can be        )
(  ordered arbitrarily, this source is organised in order to make the   )
(  disassembly pleasant to read.                                        )


( Note the decompiler hits them in the reverse order                    )
( ' CNT      BA BY DATA                                                )
( COMMA-ERS are missing from the ALPHA,                                 )
(   all data sits within the instructions                               )

( Meaning of the bits in TALLY-BA :                                     )
\     0000,0001 Interesting required       0000,0002 Instruction part isn't
\     0000,0004 b reg operand              0000,0008 8 bit data field
\     0000,0010 F-P IEEE                   0000,0020 F-P VAX
\     0000,0040 F-P Integer out            0000,0080 F-P Floating point out
\     0000,0100 F-P Opcode 14              0000,0200 F-P Opcode 16 / 15
\
\     0001,0000 AMASK 0 is clear           0002,0000 Instruction requires AMASK 0
\     0004,0000 AMASK 1 is clear           0008,0000 Instruction requires AMASK 1
\     0010,0000 AMASK 2 is clear           0020,0000 Instruction requires AMASK 2
\     0040,0000 AMASK 8 is clear           0080,0000 Instruction requires AMASK 8

( Transform the XX.XXX code such as used in appendix C into             )
( a proper BI mask.                                                     )
: BI: 0. &. (PARSE) >NUMBER 2DROP DROP 1A LSHIFT
      0. BL (PARSE) >NUMBER 2DROP DROP 5 LSHIFT OR ;

( Mask for opcode with function codes.)
: FUNCTION-MASK [ BI: 3F.FFF INVERT FFFF,FFFF AND ] LITERAL ;
( Mask for integer operate instruction opcodes.)
: F3I-MASK [ BI: 3F.7F INVERT FFFF,FFFF AND ] LITERAL ;
( Mask for floating operate instruction opcodes.)
: F3F-MASK [ BI: 3F.3F INVERT FFFF,FFFF AND ] LITERAL ;
( Mask for all other opcodes.)
: NORMAL-MASK [ BI: 3F.0 INVERT FFFF,FFFF AND ] LITERAL ;

( ***************************** 4.7 FP Modifiers ********************** )

(                                   TRP bits                            )
0000 0 E000 0000 xFI /I
0080 0 E000 2000 xFI /U
0040 0 E000 2000 xFI /V
0080 0 E000 4000 xFI --
0080 0 E000 6000 xFI --
0020 0 E000 8000 xFI /S
0080 0 E000 A000 xFI /SU
0040 0 E000 A000 xFI /SV
0090 0 E000 C000 xFI --
0090 0 E000 E000 xFI /SUI
0050 0 E000 E000 xFI /SVI

(                                   RND bits                            )
0000 0 1800 0000 xFI /C
0010 0 1800 0800 xFI /M
0000 0 1800 1000 xFI /N
0010 0 1800 1800 xFI /D

( ***************************** register fixups *********************** )
\ Note all registers are set to uninteresting by default.
\ Otherwise a ``SHOW:'' would generate 32^3 lines.
2  0  1F T!
1 0   20 xFAMILY|
  c0| c1| c2| c3| c4| c5| c6| c7| c8| c9| c10| c11| c12| c13| c14| c15| c16|
  c17| c18| c19| c20| c21| c22| c23| c24| c25| c26| c27| c28| c29| c30| cz|

6  0  001F,0000 T!
0001,0000 0  20 xFAMILY|
  b0| b1| b2| b3| b4| b5| b6| b7| b8| b9| b10| b11| b12| b13| b14| b15| b16|
  b17| b18| b19| b20| b21| b22| b23| b24| b25| b26| b27| b28| b29| b30| bz|


2  0  03E0,0000 T!
0020,0000 0   20 xFAMILY|
  a0| a1| a2| a3| a4| a5| a6| a7| a8| a9| a10| a11| a12| a13| a14| a15| a16|
  a17| a18| a19| a20| a21| a22| a23| a24| a25| a26| a27| a28| a29| a30| az|

( ***************************** 8 bit data field ********************** )

8 0 001F,E000 0D DFI b#|    ( 8 bit data field for b-operand)
4  0  0000,F000 0 xFI R|
8  0  0000,1000 1000 xFI #|

( ***************************** 4.4 ARITHMETIC ************************ )

( DEC calls this an operate instruction format.                          )
0 0 F3I-MASK T!
    BI: 0.20 BI: 10.00 4 4FAMILY, ADDL, ADDQ, ADDL/V, ADDQ/V,
    BI: 0.20 BI: 10.09 4 4FAMILY, SUBL, SUBQ, SUBL/V, SUBQ/V,
    BI: 0.20 BI: 13.00 4 4FAMILY, MULL, MULQ, MULL/V, MULQ/V,
0 0 F3I-MASK BI: 13.30 4PI UMULH

    BI: 0.10 BI: 10.02 4 4FAMILY, S4ADDL, S8ADDL, S4ADDQ, S8ADDQ,
    BI: 0.10 BI: 10.0B 4 4FAMILY, S4SUBL, S8SUBL, S4SUBQ, S8SUBQ,

    BI: 0.10 BI: 10.1D 6 4FAMILY, CMPULT, CMPEQ, CMPULE, CMPLT, -- CMPLE,

20,0000 0 F3I-MASK T!
    BI: 0.01 BI: 1C.30 4 4FAMILY, CTPOP, --    CTLZ, CTTZ,
80,000 0 F3I-MASK BI: 1C.31    4PI       PERR,

80,0000 0 F3I-MASK T!
    BI: 0.01 BI: 1C.34 0C 4FAMILY, UNPKBW, UNPKBL, PKWB, PKLB, MINSB8, MINSW4,
    MINUB8, MINUW4, MAXUB8, MAXUW4, MAXSB8, MAXSW4,

( ***************************** 4.5 LOGIC ***************************** )

0 0 F3I-MASK T!
    BI: 0.20 BI: 11.00 3 4FAMILY, AND, XOR, BIS,
    BI: 0.20 BI: 11.08 3 4FAMILY, BIC, ORNOT, EQV,

0 0 F3I-MASK BI: 11.14 4PI CMOVLBS,
    BI: 0.20 BI: 11.24 3 4FAMILY, CMOVEQ, CMOVLT, CMOVLE,

0 0 F3I-MASK BI: 11.16 4PI CMOVLBC,
    BI: 0.20 BI: 11.26 3 4FAMILY, CMOVNE, CMOVGE, CMOVGT,

0 0 F3I-MASK BI: 12.34 4PI SRL,
0 0 F3I-MASK BI: 12.39 4PI SLL,
0 0 F3I-MASK BI: 12.3C 4PI SRA,

( ***************************** 4.6 BYTE ****************************** )
0 0 F3I-MASK T!
    BI: 0.10 BI: 12.06 4 4FAMILY, EXTBL, EXTWL, EXTLL, EXTQL,
    BI: 0.10 BI: 12.5A 3 4FAMILY, EXTWH, EXTLH, EXTQH,

    BI: 0.10 BI: 12.0B 4 4FAMILY, INSBL, INSWL, INSLL, INSQL,
    BI: 0.10 BI: 12.57 3 4FAMILY, INSWH, INSLH, INSQH,

    BI: 0.10 BI: 12.02 8 4FAMILY, MSKBL, MSKWL, MSKLL, MSKQL, -- MSKWH, MSKLH, MSKQH,
    BI: 0.01 BI: 12.30 2 4FAMILY, ZAP, ZAPNOT,

0 0 F3I-MASK BI: 1C.00 4PI SEXTB,
0 0 F3I-MASK BI: 1C.01 4PI SEXTW,

( ***************************** 4.7 FP Formats ************************ )

(                                   SRC bits                            )
\ 0200 0 0600 0600 xFI Q|       ( Both)
\
\ 0010 0 0600 0000 xFI S|       ( IEEE)
\ 0010 0 0600 0400 xFI T|
\
\ 0020 0 0600 0000 xFI F|       ( VAX)
\ 0220 0 0600 0200 xFI D|
\ 0020 0 0600 0400 xFI G|


( ***************************** 4.10 FP Operate *********************** )

(     At Dec the /N bit is set in the 3 operand instruction, i.e.       )
(     normal rounding is built in in the assembler instruction for      )
(     normal calculations. This are the first two groups.               )
94 0 F3F-MASK T!
    BI: 0.001 BI: 16.000 4 4FAMILY, ADDS, SUBS, MULS, DIVS,
    BI: 0.001 BI: 16.020 4 4FAMILY, ADDT, SUBT, MULT, DIVT,
    BI: 0.001 BI: 16.024 4 4FAMILY, CMPTUN, CMPTEQ, CMPTLT, CMPTLE,
    BI: 0.020 BI: 14.00B 2 4FAMILY, SQRTS, SQRTT,

A4 0 F3F-MASK T!
    BI: 0.001 BI: 15.000 4 4FAMILY, ADDF, SUBF, MULF, DIVF,
    BI: 0.001 BI: 15.00A 4 4FAMILY, ADDG, SUBG, MULG, DIVG,
    BI: 0.001 BI: 15.025 3 4FAMILY, CMPGEQ, CMPGLT, CMPGLE,
    BI: 0.020 BI: 14.00A 2 4FAMILY, SQRTF, SQRTG,


04 0 F3F-MASK T!
A4 0 F3F-MASK BI: 15.01E 4PI CVTDG,
A4 0 F3F-MASK BI: 15.02C 4PI CVTGF,
A4 0 F3F-MASK BI: 15.02D 4PI CVTGD,
64 0 F3F-MASK BI: 15.02F 4PI CVTGQ,
A4 0 F3F-MASK BI: 15.03E 4PI CVTQG,
A4 0 F3F-MASK BI: 15.03C 4PI CVTQF,

94 0 F3F-MASK BI: 16.02C 4PI CVTTS,
54 0 F3F-MASK BI: 16.02F 4PI CVTTQ,
94 0 F3F-MASK BI: 16.03C 4PI CVTQS,
94 0 F3F-MASK BI: 16.03E 4PI CVTQT,
94 0 F3F-MASK BI: 16.00C 4PI CVTST,

( Instructions without the /N bit built in.                             )
04 0 F3F-MASK T!
    BI: 0.001 BI: 17.020 3 4FAMILY, CPYS, CPYSE, CPYSN,

44 0 F3F-MASK T!
    BI: 0.020 BI: 17.010 2 4FAMILY, CVTLQ, CVTQL,

84 0 F3F-MASK T!
    BI: 0.001 BI: 17.02A 6 4FAMILY, FCMOVEQ, FCMOVNE, FCMOVLT, FCMOVGE,
        FCMOVLE, FCMOVGT,
    BI: 0.001 BI: 17.024 2 4FAMILY, MT_FPCR, MF_FPCR,

54 0 F3F-MASK T!
    BI: 0.010 BI: 14.004 3 4FAMILY, ITOFS, ITOFF, ITOFT,

( At Dec the /C bit is set in the instruction, no /N. )
54 0 F3F-MASK T!
    BI: 0.008 BI: 1C.030 2 4FAMILY, FTOIT, FTOIS,

( **************************** Misc Opr ******************************  )

( ***************************** 16 bits displacement ****************** )

0 0 0000,FFFF 0 DFI D16|

( ***************************** 4.2 LOAD/STORE ************************ )

( DEC calls this a memory instruction format.                           )
0 0 NORMAL-MASK  T!
BI: 01.0 BI: 28.0 8 4FAMILY, LDL, LDQ, LDL_L, LDQ_L, STL, STQ, STL_C, STQ_C,
BI: 01.0 BI: 08.0 8 4FAMILY, --   --    LDBU, --     LDWU, STW, STB, --
02,0004 0 NORMAL-MASK  T!      \ Like above, however requires AMASK 0
BI: 01.0 BI: 08.0 8 4FAMILY, LDA, LDAH, --    LDQ_U, --    --   --   STQ_U,

( ***************************** 4.8 LOAD/STORE FP ********************* )

( DEC calls this a memory instruction format.                           )
0 0 NORMAL-MASK  T!
BI: 01.0 BI: 20.0 8 4FAMILY, LDF, LDG, LDS, LDT, STF, STG, STS, STT,

( ***************************** 21 bits displacement ****************** )

20,0000 0 001F,FFFF 0 DFI D21|

( ***************************** 4.3 CONTROL *************************** )

( DEC calls this a branch instruction format.                           )
8 0 NORMAL-MASK  T!
BI: 01.0 BI: 38.0 8 4FAMILY, BLBC, BEQ, BLT, BLE, BLBS, BNE, BGE, BGT,
BI: 04.0 BI: 30.0 2 4FAMILY, BR, BRS,

0 0 0000,3FFF 0 DFI h#|        \ 14 bits hint

0 0 3FF,3FFF T!
BI: 00.200 BI: 1A.000 4 4FAMILY, JMP, JSR, RET, JSR_COROUTINE,

( ***************************** 4.9 CONTROL FP************************* )

( DEC calls this a branch instruction format.                           )
8 0 NORMAL-MASK  T!
BI: 01.0 BI: 30.0 8 4FAMILY, -- FBEQ, FBLT, FBLE, -- FBNE, FBGE, FBGT,

( ***************************** 4.11 Misc. **************************** )

( This instruction requires no more Ra register                         )
0 0 1F,F01F 47E0,0C20 4PI AMASK,

( Dirty trick out board hanging bit, to prevent matches with other      )
( assembler parts. Doing this via the ``BAD'' mechanism is just too     )
( expensive.                                                            )
0 0 1,03FF,FFFF 0 DFI N25|    ( 26 bits number built in)
0 0 1,03FF,FFFF BI: 0.0     4PI CALL_PAL
0 0   001F,0000  6000,E800 4PI ECB
0 0   0000,0000  6000,0400 4PI EXCB
0 0 001F,0000 T!
      0000,2000  6000,8000 2 4FAMILY, FETCH, FETCH_M,
0 0   0000,001F BI: 11.6C 4PI IMPLVER,
0 0   0000,0000  6000,4000 4PI MB,
0 0   03E0,0000  6000,E000 4PI RC,
0 0   03E0,0000  6000,C000 4PI RPCC,
0 0   03E0,0000  6000,F000 4PI RS,
0 0   0000,0000  6000,0000 4PI TRAPB,
0 0   001F,0000  6000,F800 4PI WH64,
0 0   0000,0000  6000,4400 4PI WMB,


( ********************************************************************* )
00 00 NORMAL-MASK T!

( ********************************************************************* )
( ********************************************************************* )


( ---------------------------------------test -----------------         )

\   Toggle the bit that governs showing uninteresting instructions in the disassembly.
: TOGGLE-TRIM    BA-DEFAULT 1 TOGGLE ;

\ Define the instruction part DEA as interesting, i.e. make it show up
\   in disassembly.
: INTERESTING!   >BA 2 TOGGLE ;
'a0| INTERESTING!       'a7| INTERESTING!       'az| INTERESTING!
'b0| INTERESTING!       'b7| INTERESTING!       'bz| INTERESTING!
'c0| INTERESTING!       'c7| INTERESTING!       'cz| INTERESTING!

: SHOW:   TOGGLE-TRIM SHOW: TOGGLE-TRIM ;
: SHOW-ALL   TOGGLE-TRIM SHOW-ALL TOGGLE-TRIM ;




\ : NEXT CHECK26 ;
\     ." COMES JAN" CR
\         CODE JAN ADDQ, a30| R| b30| c30| NEXT ENDCODE
\
\ 'JAN >CFA @ DDD DROP
\
\         CODE KEES ADDQ, a30| #| 1E b#| c30| NEXT ENDCODE
\ 'KEES >CFA @ DDD DROP
\
\         CODE PIET LDA, a30| 0AAAA D16| NEXT ENDCODE
\ 'PIET  >CFA @ DDD DROP
\         CODE PIET LDA, a30| b30| NEXT ENDCODE
\ 'PIET  >CFA @ DDD DROP
\         "Expect ERROR" TYPE CR
\         CODE PIET LDA, a30| b30| 0AAAA D16| NEXT ENDCODE
\ 'PIET  >CFA @ DDD DROP

PREVIOUS DEFINITIONS
