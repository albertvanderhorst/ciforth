( $Id$)
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Reference Alpha Architecture Handbook Order Number EX-QD2KC-TE )
( Also down loadable as .pdf file from dec sites.                       )
( ############## ALPHA ASSEMBLER PROPER ################################ )
ASSEMBLER DEFINITIONS  HEX
( Note the decompiler hits them in the reverse order                    )
( ' CNT      BA BY DATA                                                )
( COMMA-ERS are missing from the ALPHA,                                 )
(   all data sits within the instructions                               )

( Meaning of the bits in TALLY-BA :                                     )
\     0000,0001 Interesting required       0000,0002 Instruction part isn't
\     0000,0004 b reg operand              0000,0008 8 bit data field
\
\     0001,0000 AMASK 0 is clear           0002,0000 Instruction requires AMASK 0
\     0004,0000 AMASK 1 is clear           0008,0000 Instruction requires AMASK 1
\     0010,0000 AMASK 2 is clear           0020,0000 Instruction requires AMASK 2
\     0040,0000 AMASK 8 is clear           0800,0000 Instruction requires AMASK 8

( Transform the XX.XXX code such as used in appendix C into             )
( a proper BI mask.                                                     )
: BI: 0. &. (PARSE) >NUMBER 2DROP DROP 1A LSHIFT
      0. BL (PARSE) >NUMBER 2DROP DROP 5 LSHIFT OR ;

( Mask for opcode with function codes.)
: FUNCTION-MASK [ BI: 3F.FFF INVERT FFFF,FFFF AND ] LITERAL ;
( Mask for operate instruction opcodes.)
: F3-MASK [ BI: 3F.7F INVERT FFFF,FFFF AND ] LITERAL ;
( Mask for all other opcodes.)
: NORMAL-MASK [ BI: 3F.0 INVERT FFFF,FFFF AND ] LITERAL ;

( ***************************** FIXUP's ******************************* )
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
0 0 F3-MASK T!
    BI: 0.20 BI: 10.00 4 4FAMILY, ADDL, ADDQ, ADDL/V, ADDQ/V,
    BI: 0.20 BI: 10.09 4 4FAMILY, SUBL, SUBQ, SUBL/V, SUBQ/V,
    BI: 0.20 BI: 13.00 4 4FAMILY, MULL, MULQ, MULL/V, MULQ/V,

    BI: 0.10 BI: 10.02 4 4FAMILY, S4ADDL, S8ADDL, S4ADDQ, S8ADDQ,
    BI: 0.10 BI: 10.0B 4 4FAMILY, S4SUBL, S8SUBL, S4SUBQ, S8SUBQ,

    BI: 0.10 BI: 10.1D 6 4FAMILY, CMPULT, CMPEQ, CMPULE, CMPLT, -- CMPLE,

20,0000 0 F3-MASK T!
    BI: 0.01 BI: 1C.30 4 4FAMILY, CTPOP, --    CTLZ, CTTZ,
80,000 0 F3-MASK BI: 1C.31    4PI PERR,
80,0000 0 F3-MASK T!
    BI: 0.01 BI: 1C.34 0C 4FAMILY, UNPKBW, UNPKBL, PKWB, PKLB, MINSB8, MINSW4,
    MINUB8, MINUW4, MAXUB8, MAXUW4, MAXSB8, MAXSW4,

( ***************************** 4.5 LOGIC ***************************** )

0 0 F3-MASK T!
    BI: 0.20 BI: 11.00 3 4FAMILY, AND, XOR, BIS,
    BI: 0.20 BI: 11.08 3 4FAMILY, BIC, ORNOT, EQV,

0 0 F3-MASK BI: 11.14 4PI CMOVLBS,
    BI: 0.20 BI: 11.24 3 4FAMILY, CMOVEQ, CMOVLT, CMOVLE,

0 0 F3-MASK BI: 11.16 4PI CMOVLBC,
    BI: 0.20 BI: 11.26 3 4FAMILY, CMOVNE, CMOVGE, CMOVGT,

0 0 F3-MASK BI: 12.34 4PI SRL,
0 0 F3-MASK BI: 12.39 4PI SLL,
0 0 F3-MASK BI: 12.3C 4PI SRA,

( ***************************** 16 bits displacement ****************** )

0 0 0000,FFFF 0 DFI D16|

( ***************************** 4.2 LOAD/STORE ************************ )

( DEC calls this a memory instruction format.                           )
0 0 NORMAL-MASK  T!
BI: 01.0 BI: 28.0 8 4FAMILY, LDL, LDQ, LDL_L, LDQ_L, STL, STQ, STL_C, STQ_C,
BI: 01.0 BI: 08.0 8 4FAMILY, --   --    LDBU, --     LDWU, STW, STB, --
02,0004 0 NORMAL-MASK  T!      \ Like above, however requires AMASK 0
BI: 01.0 BI: 08.0 8 4FAMILY, LDA, LDAH, --    LDQ_U, --    --   --   STQ_U,

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

( ********************************************************************* )
00 00 NORMAL-MASK T!
\ BI: 1.0 BI: 0.0 8 4FAMILY, CALL_PAL OPC01 OPC02 OPC03 OPC04 OPC05 OPC06 OPC07

( ********************************************************************* )
80,0000 0 03FF,FFFF 0 DFI N25|    ( 26 bits number built in)
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
