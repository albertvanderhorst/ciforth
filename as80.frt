
( The second cell contains the instruction :
(   first byte : first instruction byte etc. byte 2..4 are optional)
 0 VARIABLE TALLY 0 CELL+ ALLOT  ( 4 BYTES FOR COMMAER 4 FOR INSTRUCTION)
 0 VARIABLE PRO-TALLY 0 CELL+ ALLOT  ( Prototype for TALLY)
 0 VARIABLE ISS  ( Start of current instruction)
 0 VARIABLE PREVIOUS ( Previous comma, or zero)
: <POST HERE ISS !  0 PREVIOUS ! ;
: @+ >R R CELL+ R> @ ;
: CHECK  TALLY @ 26 ?ERROR
         TALLY CELL+ @ -1 - 27 ?ERROR ;
: POST, CHECK @+ TALLY ! @+ TALLY CELL+ ! @ , ;
( Correct dictionary to have an instruction of N bytes, after
( post allocated a whole cell)
: CORRECT 0 CELL+ MINUS + ALLOT ;
: INVERT -1 XOR ;
( Assemble an 1..3 byte instruction and post what is missing.)
( Unused bits are considered filled in)
HEX
: 1PI <BUILDS INVERT , FFFFFF00 OR , , DOES> <POST POST, 1 CORRECT ;
: 2PI <BUILDS INVERT , FFFF0000 OR , , DOES> <POST POST, 2 CORRECT ;
: 3PI <BUILDS INVERT , FF000000 OR , , DOES> <POST POST, 3 CORRECT ;
DECIMAL
: OR! >R R @    2DUP AND 28 ?ERROR   OR R> ! ;
: AND! >R INVERT  R @ 2DUP OR 0= 29 ?ERROR   AND R> ! ;
: FIX| @+ TALLY OR! @+ TALLY CELL+ OR! @ ISS @ OR! ;

( One size fits all. )
: xFI <BUILDS , , , DOES> FIX| ;

: CHECK DUP PREVIOUS < 30 ?ERROR DUP PREVIOUS ! ;
: BOOKKEEPING CHECK TALLY AND! ;

: COMMAER <BUILDS  SWAP , , DOES> @+ BOOKKEEPING   @ EXECUTE ;

1 ' C, COMMAER IB,   ( immediate byte data )
2 ' ,  COMMAER IX,   ( immediate data : cell )
4 ' ,  COMMAER X,    ( immediate data : address )
8 ' C, COMMAER P,     ( port number ; byte       )

( Fill in the tally prototype with FIRST and SECOND data )
: T! PRO-TALLY CELL+ ! PRO-TALLY ! ;
( From `TALLY' and the  INSTRUCTION code)
( prepare THE THREE CELLS for an instruction )
: PREPARE >R PRO-TALLY @ PRO-TALLY CELL+ @ R> ;
( By INCREMENTing the OPCODE a NUMBER of times generate number
(  instructions)
( INCREMENT, OPCODE , COUNT -- )
: 1FAMILY, 0 DO DUP PREPARE 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP PREPARE 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP PREPARE 3PI OVER + LOOP DROP DROP ;

: xFAMILY| 0 DO DUP PREPARE xFI OVER + LOOP DROP DROP ;

CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >3<" HEX
HEX VOCABULARY ASSEMBLER IMMEDIATE
' ASSEMBLER CFA ' ;CODE 8 + !        ( PATCH ;CODE IN NUCLEUS )
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !CSP ; IMMEDIATE
: C; CURRENT @ CONTEXT ! ?EXEC ?CSP SMUDGE ; IMMEDIATE
: LABEL ?EXEC 0 VARIABLE SMUDGE -2 ALLOT [COMPILE] ASSEMBLER
    !CSP ; IMMEDIATE     ASSEMBLER DEFINITIONS
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >1<"
00 FF 00 1PI NOP       00 FF C9 1PI RET       00 FF 76 1PI HLT
00 FF T! 07 08 8 1FAMILY, RLC RRC RAL RAR DAA CMA STC CMC
00 FF T! E3 08 4 1FAMILY, XTHL XCHG DI EI
00 FF T! E9 08 2 1FAMILY, PCHL SPHL
00 FF T! C7 01 8 1FAMILY, RST0 RST1 RST2 RST3 RST4 RST5 RST6 RST7

00 07 T! 00 01 8 xFAMILY| B| C| D| E| H| L| M| A| ( src)
00 F8 T! 80 08 8 1FAMILY, ADD ADC SUB SBB ANA XRA ORA CMP ( B|)

00 3F T! 00 10 4 xFAMILY| BC| DE| HL| SP|
00 CF T! 02 01 2 1FAMILY, STAX INX               ( BC|)
00 CF T! 09 01 3 1FAMILY, DAD LDAX DCX           ( BC|)
02 CF 01 1PI LXI ( BC| IX,)
00 3F 28 xFI PSW|
00 CF T! C1 04 2 1FAMILY, POP PUSH               ( BC|)
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >2<"
( With immediate data )
08 FF T! D3 08 2 1FAMILY, OUT IN     ( P,)
01 FF T! C6 08 8 1FAMILY, ADI ACI SUI SBI ANI XRI ORI CPI  ( I,)

( With an address)
04 FF T! 22 08 4 1FAMILY, SHLD LHLD STA LDA ( X,)
04 FF T! C3 08 2 1FAMILY, JMP CALL ( X,)

00 07 T! 00 01 8 xFAMILY| B'| C'| D'| E'| H'| L'| M'| A'| ( dst)
00 C7 T! 04 01 2 1FAMILY, INR DCR       ( B'|)
00 C0 40 1PI MOV ( B'| B|)     01 C7 06 1PI MVI ( B'| I,)

00 30 T! 00 10 4 xFAMILY| ZR| CY| PE| LS|
00 08 T! 00 08 2 xFAMILY| N| Y|
00 C7 C0 1PI RC, ( ZR| Y| )
04 C7 T! C2 02 2 1FAMILY, JC, CC, ( ZR| Y| T, )
' ;S 0B + @ CONSTANT (NEXT)
: NEXT JMP (NEXT) X, ;
: PSH1 JMP (NEXT) 1 - X, ;
: PSH2 JMP (NEXT) 2 - X, ;
: THEN HERE SWAP ! ;         : HOLDPLACE HERE 0 X, ;
: IF JC, HOLDPLACE ;  ( ZR| Y| )
: ELSE JMP HOLDPLACE ;           : BEGIN HERE ;
: UNTIL IF DROP ;                  : WHILE IF ;
: REPEAT SWAP JMP X,  THEN ;
;S
