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

.S

BI: 3F.FFF INVERT CONSTANT FUNCTION-MASK ( Mask for opcode with function codes.)
BI: 3F.0 INVERT CONSTANT NORMAL-MASK     ( Mask for all other opcodes.)
.S

0  0  1F 15 LSHIFT T!
.S
1 15 LSHIFT   0   20 xFAMILY|
 a0| a1| a2| a3| a4| a5| a6| a7| a8| a9| a10| a11| a12| a13|
 a14| a15| a16| a17| a18| a19| a20| a21| a22| a23| a24| a25|
 a26| a27| a28| a29| a30| a31|
.S

0  0  1FF 0C LSHIFT T!
1 10 LSHIFT   0   20 xFAMILY|
 b0| b1| b2| b3| b4| b5| b6| b7| b8| b9| b10| b11| b12| b13|
 b14| b15| b16| b17| b18| b19| b20| b21| b22| b23| b24| b25|
 b26| b27| b28| b29| b30| b31|
.S

0  0  1F T!
1 0   20 xFAMILY|
 c0| c1| c2| c3| c4| c5| c6| c7| c8| c9| c10| c11| c12| c13|
 c14| c15| c16| c17| c18| c19| c20| c21| c22| c23| c24| c25|
 c26| c27| c28| c29| c30| c31|
.S

0 0 03FF,FFFF xBFI N25|    ( 26 bits number built in)
0 0 001F,FFFF xBFI D21|    ( 21 bits displacement)
0 0 0000,FFFF xBFI D16|    ( 16 bits displacement)
0 0 0001,FF00 xBFI (b#|)    ( 8 bit data field for b-operand)
: b#| 0D LSHIFT 1 OR (b#|) ;  ( probably wrong)
.S

00 00 NORMAL-MASK T!
BI: 1.0 BI: 0.0 8 4FAMILY, CALL_PAL OPC01 OPC02 OPC03 OPC04 OPC05 OPC06 OPC07
.S
00 00
BI: 3F.7F INVERT   FFFF,FFFF,0000,0000 XOR
BI: 10.20 .S 4PI ADDQ,
.S
PREVIOUS DEFINITIONS

: NEXT ;
    ." COMES JAN"
        CODE JAN ADDQ, a30| b30| c30| NEXT C;
(       CODE JAN ADDQ, a30| 4 b#| c30| NEXT C;                          )
(       ' JAN >CFA @ HERE DIS-RANGE                                     )
(   ' JAN @ D-F-A DDD DDD DDD                                           )
