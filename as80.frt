( $Id$)
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( ############## 8080 ASSEMBLER PROPER ################################ )
ASSEMBLER DEFINITIONS  HEX
( Note the decompiler hits them in the reverse order                    )
( ' CNT      BA BY DATA                                                )
  0 1        0 100 ' C, >CFA   COMMAER IB, ( immediate byte data)
  0 0 CELL+  0 200 ' ,  >CFA   COMMAER IX, ( immediate data : cell)
  0 0 CELL+  0 400 ' ,  >CFA   COMMAER X,  ( immediate data : address)
  0 1        0 800 ' C, >CFA   COMMAER P,  ( port number ; byte     )

00 00 00 T!
 08 07 8 1FAMILY, RLC RRC RAL RAR DAA CMA STC CMC
 08 E3 4 1FAMILY, XTHL XCHG DI EI
 10 E9 2 1FAMILY, PCHL SPHL
 08 C7 8 1FAMILY, RST0 RST1 RST2 RST3 RST4 RST5 RST6 RST7

00 00 07 T!
 01 00 8 xFAMILY| B| C| D| E| H| L| M| A| ( src)
 08 80 8 1FAMILY, ADD ADC SUB SBB ANA XRA ORA CMP ( B|)

00 00 30 T!
 10 00 4 xFAMILY| BC| DE| HL| SP|
 01 02 2 1FAMILY, STAX INX               ( BC|)
 01 09 3 1FAMILY, DAD LDAX DCX           ( BC|)
00 0200 30 01 1PI LXI ( BC| IX,)
00 00 30 30 xFI PSW|
00 00 30 T! 04 C1 2 1FAMILY, POP PUSH               ( BC|)
( With immediate data )
00 0800 00 T! 08 D3 2 1FAMILY, OUT IN     ( P,)
00 0100 00 T! 08 C6 8 1FAMILY, ADI ACI SUI SBI ANI XRI ORI CPI  ( I,)

( With an address)
00 0400 00 T!
 08 22 4 1FAMILY, SHLD LHLD STA LDA ( X,)
 08 C3 2 1FAMILY, JMP CALL ( X,)

00 00 38 T!
 08 00 8 xFAMILY| B'| C'| D'| E'| H'| L'| M'| A'| ( dst)
 01 04 2 1FAMILY, INR DCR       ( B'|)
00 00 3F 40 1PI MOV ( B'| B|)     00 0100 38 06 1PI MVI ( B'| I,)

00 00 130 T! 10 00 4 xFAMILY| ZR| CY| PE| LS|
00 00 08 T! 08 00 2 xFAMILY| N| Y|
00 00 138 C0 1PI RC, ( ZR| Y| )
00 0400 138 T! 02 C2 2 1FAMILY, JC, CC, ( ZR| Y| T, )
00 00 00 00 1PI NOP       00 00 00 C9 1PI RET       00 00 00 76 1PI HLT
( ############## 8080 ASSEMBLER PROPER END ############################ )
 ( Assume a NOOP written low level code.)
' NOOP >DFA @ CONSTANT (NEXT)
: NEXT JMP (NEXT) X, ;
: PSH1 JMP (NEXT) 1 - X, ;
: PSH2 JMP (NEXT) 2 - X, ;
: THEN, HERE SWAP ! ;         : HOLDPLACE HERE 0 X, ;
: IF, JC, HOLDPLACE ;  ( ZR| Y| )
: ELSE, JMP HOLDPLACE ;           : BEGIN, HERE ;
: UNTIL, IF, DROP ;                  : WHILE, IF, ;
: REPEAT, SWAP JMP X,  THEN, ;

PREVIOUS DEFINITIONS
    ." COMES JAN"
     CODE JAN MOV B| M'| LXI BC| 1223 IX, NEXT ENDCODE
    'JAN >CFA @ HERE D-R
    'JAN >CFA @ DDD DDD DDD
