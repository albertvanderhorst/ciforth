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

0  0  1F T!
1 0   20 xFAMILY|
 c0| c1| c2| c3| c4| c5| c6| c7| c8| c9| c10| c11| c12| c13|
 c14| c15| c16| c17| c18| c19| c20| c21| c22| c23| c24| c25|
 c26| c27| c28| c29| c30| c31|

1  0  001F,0000 T!
      0001,0000 0   20 xFAMILY|
 b0| b1| b2| b3| b4| b5| b6| b7| b8| b9| b10| b11| b12| b13|
 b14| b15| b16| b17| b18| b19| b20| b21| b22| b23| b24| b25|
 b26| b27| b28| b29| b30| b31|

2 0 001F,E000 0D DFI b#|    ( 8 bit data field for b-operand)
8 0 0000,FFFF 0 DFI D16|    ( 16 bits displacement)
20 0 001F,FFFF 0 DFI D21|    ( 21 bits displacement)
80 0 03FF,FFFF 0 DFI N25|    ( 26 bits number built in)

0  0  0000,F000 0 xFI R|
0  0  0000,1000 1000 xFI #|

0  0  03E0,0000 T!
0020,0000 0   20 xFAMILY|
 a0| a1| a2| a3| a4| a5| a6| a7| a8| a9| a10| a11| a12| a13|
 a14| a15| a16| a17| a18| a19| a20| a21| a22| a23| a24| a25|
 a26| a27| a28| a29| a30| a31|

00 00 NORMAL-MASK T!
BI: 1.0 BI: 0.0 8 4FAMILY, CALL_PAL OPC01 OPC02 OPC03 OPC04 OPC05 OPC06 OPC07

( DEC calls this a operate instruction format.                          )
0 0 F3-MASK BI: 10.20 .S 4PI ADDQ,

( DEC calls this a memory instruction format.                           )
51 0 NORMAL-MASK  BI: 08.0 .S 4PI LDA,

: NEXT CHECK26 ;
    ." COMES JAN" CR
        CODE JAN ADDQ, a30| R| b30| c30| NEXT C;

'JAN >CFA @ D-F-A

        CODE KEES ADDQ, a30| #| 1E b#| c30| NEXT C;
'KEES >CFA @ D-F-A

        CODE PIET LDA, a30| b30| 0AAAA D16| NEXT C;
.S
'PIET  >CFA @ D-F-A

PREVIOUS DEFINITIONS
