( $Id$)
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( ############## 8089 ASSEMBLER ADDITIONS ############################# )
ASSEMBLER DEFINITIONS  HEX
: W,   DUP 8 RSHIFT C,   C, ;
: DIS-C   POINTER @ COUNT U. POINTER !  ;
: DIS-W   POINTER @ COUNT 8 LSHIFT >R COUNT >R POINTER !  R> R> OR U. ;
( The standard disassembly for the commaer DEA for small endian.
: .COMMA-STANDARD-NEW   DUP >CNT @ 1 = IF DIS-C ELSE DIS-W THEN   %ID. ;
'.COMMA-STANDARD-NEW   '.COMMA-STANDARD 3 CELLS MOVE   \ Patch, one and for all.

( ############## 6809 ASSEMBLER PROPER ################################ )
( Note the decompiler hits them in the reverse order                    )
( ' CNT      BA BY DATA                                                )
  0 1        10 01 ' C, >CFA   COMMAER #, ( immediate byte data)
  0 2        20 01 ' W, >CFA   COMMAER ##, ( immediate data : cell)
  0 1       100 02 ' C, >CFA   COMMAER CO, ( address: byte offset)
  0 2       200 02 ' W, >CFA   COMMAER WO, ( cell: address or offset)
  0 1        00 04 ' C, >CFA   COMMAER DO, ( direct page offset )
\ Indexing is handled as an instruction within an instruction.
\ It is an Index byte, similar to the SIB of Intel.
  _ 0        00 08 ' C, >CFA   COMMAER SIB,, ( index byte, cff. sib)
  0 2        00 10 ' W, >CFA   COMMAER E,  ( extended address )
  _ 0        00 20 ' C, >CFA   COMMAER STACK, ( what to push or pop)

( Meaning of the bits in TALLY-BA :                                     )
\     0000,0001 A/B/[]/E instruction       0000,0002 #/P/[]/E instruction
\     0000,0004 Operand is register/##     0000,0008 Operand is memory
\     0000,0010 Operand is 8bit            0000,0020 Operand is 16 bits
\     0000,0040 Not sib.                   0000,0080 sib or fixup for sib.
\     0000,0100 address: byte offset       0000,0200 cell: address or offset)

\ Adressing modes go here
46 01 3000,0000 0000,0000 xFIR #|                  45 0 3000,0000 0000,0000 xFIR A|
4A 04 3000,0000 1000,0000 xFIR DP|                 45 00 3000,0000 1000,0000 xFIR B|
48 08 3000,0000 2000,0000 xFIR []
48 10 3000,0000 3000,0000 xFIR E|


( --------------- Handling of the index byte. ------------------------- )
    80 00 9F00,0000 T!
0100,0000 1000,0000 10 xFAMILY|R ,-10 ,-F ,-E ,-D ,-C ,-B ,-A ,-9 ,-8 ,-7 ,-6 ,-5 ,-4 ,-3 ,-2 ,-1
0100,0000 0000,0000 10 xFAMILY|R ,0 ,1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,A ,B ,C ,D ,E ,F
80 0 6000,0000 T!       2000,0000 0 4 xFAMILY|R X Y U S
280 02 FF00,0000 9F00,0000 xFIR [##]
    80 00 9F00,0000 T!
0100,0000 8000,0000 07 xFAMILY|R ,R+ ,R++ ,-R ,--R ,R B,R A,R
0100,0000 9100,0000 06 xFAMILY|R [,R++] -- [,--R] [,R] [B,R] [A,R]
1000,0000 8B00,0000 02 xFAMILY|R D,R [D,R]

\ The don't care bits translate to underscores
180 02 9F00,0000 T!     1000,0000 8800,0000 02 xFAMILY|R #,R [#,R]
280 02 9F00,0000 T!     1000,0000 8900,0000 02 xFAMILY|R ##,R [##,R]
    180 02 FF00,0000 T!
2000,0000 8C00,0000 04 xFAMILY|R #,PCR  #,PCR_  #,PCR__  #,PCR___
2000,0000 9C00,0000 04 xFAMILY|R [#,PCR] [#,PCR]_  [#,PCR]__ [#,PCR]___
    280 02 FF00,0000 T!
2000,0000 8D00,0000 04 xFAMILY|R ##,PCR  ##,PCR_  ##,PCR__  ##,PCR___
2000,0000 9D00,0000 04 xFAMILY|R [##,PCR] [##,PCR]_  [##,PCR]__ [##,PCR]___

\ The last instruction to be found in disassemby
80 0 FF 00 1PI ~SIB,        ' ~SIB, % SIB,, >DATA !
( Redefine [] ,  such that the user may say                             )
( "[]" instead of " [] ~SIB, [X"                                        )
( Note that the disassembly is made to look like the same.              )
( The ~SIB, are print-suppressed.                                       )
: []   [] SIB,, ;

( ---- Exchanges, has a slot to prevent too many matches.-------------- )
50 0 F000 T!    1000 8000 4 xFAMILY| =>A =>B =>CCR =>DPR
60 0 F000 T!    1000 0000 6 xFAMILY| =>D =>X =>Y =>US =>SP =>PC
50 0 10F00 T!   0100 0800 4 xFAMILY| A<= B<= CCR<= DPR<=
60 0 10F00 T!   0100 0000 6 xFAMILY| D<= X<= Y<= US<= SP<= PC<=
00 0 1FF00 T!   01 1E 2 2FAMILY, EXG, TFR,

\ One operand instruction
    01 00 30 T!
01 40 10 1FAMILY, NEG, -- -- COM, LSR, -- ROR, ASR, ASL, ROL, DEC, -- INC, TST, -- CLR,
    01 04 00 T!
01 00 8 1FAMILY, NEG|D, -- -- COM|D, LSR|D, -- ROR|D, ASR|D,
01 08 8 1FAMILY, ASL|D, ROL|D, DEC|D, -- INC|D, TST|D, JMP|D, CLR|D,

\ Two operand instruction
    12 00 30 T!
01 80 0C 1FAMILY, SUBA, CMPA, SBCA, -- ANDA, BITA, LDA, -- EORA, ADCA, ORA, ADDA,
01 C0 0C 1FAMILY, SUBB, CMPB, SBCB, -- ANDB, BITB, LDB, -- EORB, ADCB, ORB, ADDB,
    1A 00 30 T!
40 87 2 1FAMILY, STA, STB,
    2A 00 30 T!
40 8F 2 1FAMILY, STX, STU,
    22 00 30 T!
40 83 2 1FAMILY, SUBD, ADDD,
40 8C 2 1FAMILY, CMPX, LDD,
40 8E 2 1FAMILY, LDX, LDU,
2A 00 30 CD 1PI STD,
    22 00 3000 T!
0001 8310 2 2FAMILY, CMPD, CMPU,
0001 8C10 2 2FAMILY, CMPY, CMPS,
4000 8E10 2 2FAMILY, LDY, LDS,
    2A 00 3000 T!
4000 8F10 2 2FAMILY, STY, STS,

\ Branches, control flow
    0151 0 0F00,0000 T!
0100,0000 0000 10 xFAMILY|R Y| N| U>| U<=| U>=| U<| =| <>| VC| VS| 0>=| 0<| >=| <| >| <=|
29 00 30 4E 1PI JMP,
2A 00 30 8D 1PI JSR,
20 01 00 16 1PI LBRA,           20 01 00 17 1PI LBRS,
10 01 00 8D 1PI BSR,
10 01 0F 20 1PI BR,             20 01 0F00 2010 2PI LBR,

\ Miscellaneous, no operands
0 0 0 12 1PI NOP,       0 0 0 13 1PI SYNC,      0 0 0 19 1PI DAA,       0 0 0 1D 1PI SEX,
0 0 0 T!        01 39 7 1FAMILY, RTS, ABX, RTI, -- MUL, -- SWI,
0001 3F10 2 2FAMILY, SWI2, SWI3,

\ Miscellaneous, cc operand.
10 01 0 1A 1PI ORCC,    10 01 0 1C 1PI ANDCC,   10 01 0 3C 1PI CWAI,

\ The Index byte is integrated into those instruction
80 0 FF00 T!            01 30 4 2FAMILY, LEAX, LEAY, LEAS, LEAU,
\ Requiring extra byte with special syntax.
0 20 0 T!               01 34 4 1FAMILY, PSHS, PULS, PSHU, PULU,

\ Convenience for stack sets
\ Usage : PUSHS, (& B& C& ... X& )S,
: | CREATE DUP C, 1 LSHIFT DOES> C@ OR ;
1 | CCR&  | A&   | B&   | DPR&   | X&   | Y&   | U&   | PC&   DROP
'| HIDDEN    : (& DSP@ 0 ;    : )S, STACK, ?CSP ;
\ For DEA and MASK print name if mask applicable.
: PRINT-STACK 80 AND IF %ID. _ THEN DROP ;
\ For DEA and MASK leave next DEA and MASK.
: NEXT-STACK 7F AND 1 LSHIFT SWAP >NEXT% SWAP ;
: DIS-STACK   'PC&   POINTER @ @ 1 FIRSTBYTES    1 POINTER +!
   ." (& " BEGIN 2DUP PRINT-STACK NEXT-STACK DUP 0= UNTIL ." )S, "
   2DROP ;
' DIS-STACK    % STACK, >DIS !

 ' ~SIB,   % SIB,, >DATA !   ( Fill in deferred data creation action  )
( Disassemble the sib byte where the disassembler sits now.             )
( [ `FORCED-DISASSEMBLY' takes care itself of incrementing the          )
(   disassembly pointer. ]                                              )
: DIS-SIB [ % ~SIB, ] LITERAL FORCED-DISASSEMBLY ;
( Fill in deferred disassembler action.                                 )
' DIS-SIB    % SIB,, >DIS !

\ None of the essential
: LSL,   ASL, ;         \ Do not use an alias, then the disassembler will use it! l
