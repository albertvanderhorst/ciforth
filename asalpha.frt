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
\     0000,0400 az| must not be used       0000,0800 az| is actually used
\     0000,1000 bz| must not be used       0000,2000 bz| is actually used
\     0000,4000 no #/R field               0000,8000 #/R possible
\
\     0001,0000 AMASK 0 is clear           0002,0000 Instruction requires AMASK 0
\     0004,0000 AMASK 1 is clear           0008,0000 Instruction requires AMASK 1
\     0010,0000 AMASK 2 is clear           0020,0000 Instruction requires AMASK 2
\     0040,0000 AMASK 8 is clear           0080,0000 Instruction requires AMASK 8

\    1,000,0000 Disallow /U/SU IEEE        2,0000,0000 /S or /SU trap.
\    4,000,0000 Disallow /U/SU/SUI         8,0000,0000 /U /SU /SUI trap.

\ For DEA set and individual MASK to bad (or back).
: !BAD    SWAP >BA SWAP TOGGLE ;
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
( Mask for opcodes with 16 bits data on board. No c-register.           )
: NORMAL-MASK [ BI: 3F.0 INVERT FFFF,FFFF AND ] LITERAL ;

( ***************************** 4.7 FP Modifiers ********************** )

(                                   TRP bits                            )
00,0000,4000 0 E000 0000 xFI /I
2A,0000,4080 0 E000 2000 xFI /U
2A,0000,4040 0 E000 2000 xFI /V
(                 4000                                                  )
(                 6000                                                  )
00,0000,4020 0 E000 8000 xFI /S        ( Only VAX )
2A,0000,4080 0 E000 A000 xFI /SU
0A,0000,4040 0 E000 A000 xFI /SV
(                 C000                                                  )
28,0000,4090 0 E000 E000 xFI /SUI      ( Only IEEE )
28,0000,4050 0 E000 E000 xFI /SVI

(                                   RND bits                            )
4000 0 1800 0000 xFI /C
4010 0 1800 0800 xFI /M
4000 0 1800 1000 xFI /N
4010 0 1800 1800 xFI /D

( ***************************** register fixups *********************** )
\ Note all registers are set to uninteresting by default.
\ Otherwise a ``SHOW:'' would generate 32^3 lines.
2  0  1F T!
1 0   20 xFAMILY|
  R0c R1c R2c R3c R4c R5c R6c R7c R8c R9c R10c R11c R12c R13c R14c R15c R16c
  R17c R18c R19c R20c R21c R22c R23c R24c R25c R26c R27c R28c R29c R30c Rzc

6  0  001F,0000 T!
0001,0000 0  20 xFAMILY|
  R0b R1b R2b R3b R4b R5b R6b R7b R8b R9b R10b R11b R12b R13b R14b R15b R16b
  R17b R18b R19b R20b R21b R22b R23b R24b R25b R26b R27b R28b R29b R30b Rzb

2  0  03E0,0000 T!
0020,0000 0   20 xFAMILY|
  R0a R1a R2a R3a R4a R5a R6a R7a R8a R9a R10a R11a R12a R13a R14a R15a R16a
  R17a R18a R19a R20a R21a R22a R23a R24a R25a R26a R27a R28a R29a R30a Rza

(   Toggle the a-register field in the MASK. Leave IT.                  )
(   This can be use to clear the register field in a BI-mask as well    )
(   as set it them in an instruction , i.e. the all zero register.      )
: NO-a  03E0,0000 XOR  ;
(   Toggle the a-register field in the MASK. Leave IT.                  )
: NO-b  001F,0000 XOR  ;

( Toggle some register fixup's back to interesting, i.e. make it show   )
( up in disassembly.                                                    )
'R0a 2 !BAD             'R7a 2 !BAD             'Rza 2 !BAD
'R0b 2 !BAD             'R7b 2 !BAD             'Rzb 2 !BAD
'R0c 2 !BAD             'R7c 2 !BAD             'Rzc 2 !BAD
\ Prohibit some instructions from using the zero register.
'Rza 0800 !BAD          'Rzb 2000 !BAD

( ***************************** 8 bit data field ********************** )

8008 0 001F,E000 0D DFI b#|    ( 8 bit data field for b-operand)
8004 0 0000,F000 0 xFI R|
8008 0 0000,1000 1000 xFI #|

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

( Have ``bz|'' and ``R|'' fixed in the instruction. )
20,0000 0 001F,001F T!
    BI: 0.01 BI: 1C.30 NO-a 4 4FAMILY, CTPOP, --    CTLZ, CTTZ,

( ***************************** 4.13 MULTIMEDIA *********************** )

80,0000 0 03FF,001F BI: 1C.31 4PI PERR,
80,0000 0 001F,001F T!
    BI: 0.01 BI: 1C.34 NO-a 4 4FAMILY, UNPKBW, UNPKBL, PKWB, PKLB,
80,0000 0 F3I-MASK T!
    BI: 0.01 BI: 1C.38
    8 4FAMILY, MINSB8, MINSW4, MINUB8, MINUW4, MAXUB8, MAXUW4, MAXSB8, MAXSW4,

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

2,0000 0   F3I-MASK NO-a   BI: 1C.00 NO-a   4PI SEXTB,
2,0000 0   F3I-MASK NO-a   BI: 1C.01 NO-a   4PI SEXTW,

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

(  The instructions as shown in Table C-8 are always /N /I .            )
(  If alternatives for /N are allowed, the /N bit is stripped in the    )
(  base instruction, i.e. subtract 80 from yyy in BI: xx.yyy            )

\ REORDER LATER IN TRAPPING AND NON TRAPPING. NORMAL ROUNDING OBLIGATORY.
4094 0 F3F-MASK T!
    BI: 0.001 BI: 16.000 4 4FAMILY, ADDS, SUBS, MULS, DIVS,
    BI: 0.001 BI: 16.020 4 4FAMILY, ADDT, SUBT, MULT, DIVT,
4094 0 03FF,001F  T!
    BI: 0.001 BI: 16.0A4 4 4FAMILY, CMPTUN, CMPTEQ, CMPTLT, CMPTLE,
8,4094 0 F3F-MASK NO-a T!
    BI: 0.020 BI: 14.00B NO-a 2 4FAMILY, SQRTS, SQRTT,

40A4 0 F3F-MASK T!
    BI: 0.001 BI: 15.000 4 4FAMILY, ADDF, SUBF, MULF, DIVF,
    BI: 0.001 BI: 15.020 4 4FAMILY, ADDG, SUBG, MULG, DIVG,
4,0000,40A4 0 03FF,E01F  T!
    BI: 0.001 BI: 15.0A5 3 4FAMILY, CMPGEQ, CMPGLT, CMPGLE,
8,40A4 0 F3F-MASK NO-a T!
    BI: 0.020   BI: 14.00A  NO-a 2 4FAMILY, SQRTF, SQRTG,

40A4 0 001F,F81F         BI: 15.01E NO-a 4PI CVTDG,

40A4 0 001F,F81F         BI: 15.02C NO-a 4PI CVTGF,
40A4 0 001F,F81F         BI: 15.02D NO-a 4PI CVTGD,
4064 0 001F,F81F         BI: 15.02F NO-a 4PI CVTGQ,

40A4 0 001F,181F         BI: 15.03E NO-a 4PI CVTQG,
40A4 0 001F,181F         BI: 15.03C NO-a 4PI CVTQF,

4094 0 001F,F81F         BI: 16.02C NO-a 4PI CVTTS,
4054 0 001F,F81F         BI: 16.02F NO-a 4PI CVTTQ,

1,0000,4094 0 001F,F81F         BI: 16.03C NO-a 4PI CVTQS,
1,0000,4094 0 001F,F81F         BI: 16.03E NO-a 4PI CVTQT,

( These instructions are irregular!                                     )
4,0000,4094 0 001F,001F          BI: 16.2AC NO-a 4PI CVTST,
4,0000,4094 0 001F,001F          BI: 16.6AC NO-a 4PI CVTST/S,

4084 0 03FF,001F T!
    BI: 0.001 BI: 17.020 3 4FAMILY, CPYS, CPYSE, CPYSN,

4044 0 001F,001F BI: 17.010 NO-a 4PI CVTLQ,
( Mark this instruction as VAX floating for trap modifiers.             )
4064 0 001F,E01F BI: 17.030 NO-a 4PI CVTQL,

4084 0 03FF,001F T!
    BI: 0.001 BI: 17.02A 6 4FAMILY, FCMOVEQ, FCMOVNE, FCMOVLT, FCMOVGE,
        FCMOVLE, FCMOVGT,
    BI: 0.001 BI: 17.024 2 4FAMILY, MT_FPCR, MF_FPCR,

8,4054 0 03E0,001F T!
    BI: 0.010 BI: 14.004 NO-b 3 4FAMILY, ITOFS, ITOFF, ITOFT,

8,5054 0 03E0,001F T!
    BI: 0.008 BI: 1C.070  NO-b 2 4FAMILY, FTOIT, FTOIS,

( **************************** Misc Opr ******************************  )

( ***************************** 16 bits displacement ****************** )

0 0 0000,FFFF 0 DFI D16|

( ***************************** 4.2 LOAD/STORE ************************ )

( DEC calls this a memory instruction format.                           )
8004 0 NORMAL-MASK  T!
BI: 01.0 BI: 28.0 8 4FAMILY, LDL, LDQ, LDL_L, LDQ_L, STL, STQ, STL_C, STQ_C,
BI: 01.0 BI: 08.0 8 4FAMILY, LDA, LDAH, --    LDQ_U, --    --   --   STQ_U,
02,8004 0 NORMAL-MASK  T!      \ Like above, however requires AMASK 0
BI: 01.0 BI: 08.0 8 4FAMILY, --   --    LDBU, --     LDWU, STW, STB, --

( ***************************** 4.8 LOAD/STORE FP ********************* )

( DEC calls this a memory instruction format.                           )
0 0 NORMAL-MASK  T!
BI: 01.0 BI: 20.0 8 4FAMILY, LDF, LDG, LDS, LDT, STF, STG, STS, STT,

( ***************************** 4.3 CONTROL *************************** )

0 0 0000,3FFF 0 DFI h#|        \ 14 bits hint

0 0 3FF,3FFF T!
BI: 00.200 BI: 1A.000 4 4FAMILY, JMP, JSR, RET, JSR_COROUTINE,

20,0000 0 001F,FFFF 0 DFI D21| \ 21 bits displacement

( DEC calls this a branch instruction format.                           )
8 0 NORMAL-MASK  T!
BI: 01.0 BI: 38.0 8 4FAMILY, BLBC, BEQ, BLT, BLE, BLBS, BNE, BGE, BGT,
BI: 04.0 BI: 30.0 2 4FAMILY, BR, BRS,

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
0 0   0000,001F BI: 11.6C NO-a NO-b 4PI IMPLVER,
0 0   0000,0000  6000,4000 4PI MB,
0 0   03E0,0000  6000,E000 4PI RC,
0 0   03E0,0000  6000,C000 4PI RPCC,
0 0   03E0,0000  6000,F000 4PI RS,
0 0   0000,0000  6000,0000 4PI TRAPB,
0 0   001F,0000  6000,F800 4PI WH64,
0 0   0000,0000  6000,4400 4PI WMB,

( ********************************************************************* )

( Any PALCODE can be added as in this example :                         )
(       0 0 0 BI: 00.0083 4PI callsys                                   )

( ***************************** POSTLUDE ****************************** )

\   Toggle the bit that governs showing uninteresting instructions in the disassembly.
: TOGGLE-TRIM    BA-DEFAULT 1 TOGGLE ;

4 I-ALIGNMENT !

: SHOW:   TOGGLE-TRIM SHOW: TOGGLE-TRIM ;
: SHOW-ALL   TOGGLE-TRIM SHOW-ALL TOGGLE-TRIM ;

\ Set the bad bits in the default according to an AMASK .
\ Failing bits in amask, give bad bits that are set.
: SET-AMASK
    DUP  1 0 LSHIFT AND 0= IF BA-DEFAULT 0001,0000 TOGGLE THEN
    DUP  1 1 LSHIFT AND 0= IF BA-DEFAULT 0004,0000 TOGGLE THEN
    DUP  1 2 LSHIFT AND 0= IF BA-DEFAULT 0010,0000 TOGGLE THEN
    DUP  1 8 LSHIFT AND 0= IF BA-DEFAULT 0040,0000 TOGGLE THEN
    DROP ;

PREVIOUS DEFINITIONS
