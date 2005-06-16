( $Id: )
( Copyright{2005}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Minimal POSTIT/FIXUP 8086 ASSEMBLER AvdH HCC HOLLAND)
( This replace the FIG-Forth version. )
( 5 screens follow here, this is an excerpt of the Pentium              )
( assembler in the blocks of ciforth after 4.0.6 and                    )
( compatible with the great assembler                                   )

                        HEX

( --assembler_generic SPLIT 1PI FIR 1FAMILY, )  \ A4sep27 AvdH
: SPLIT 0 100 UM/MOD SWAP   ; \ Split X : ls BYTE and REMAINDER
\ Post INSTRUCTION of LENGTH.  Big endian specific!
: POST  SWAP , 1 CELLS - ALLOT ;
\ Fixup with ms byte of FIX below ADDR, leave next FIX ADDR
: FIX| 1- >R   SPLIT R@ SWAP TOGGLE   R> ;
: 1PI CREATE , DOES>  @ 1 POST  ;   \ 1 byte post-it opcode
: 2PI CREATE , DOES>  @ 2 POST  ;   \ 2 byte post-it opcode
: 3PI CREATE , DOES>  @ 3 POST  ;   \ 3 byte post-it opcode
\ Fixup from behind starting with ls byte.
: FIR CREATE , DOES> @ HERE BEGIN FIX| OVER 0= UNTIL 2DROP ;
\ Create a family adding INC to OPCODE with COUNT members
: 1FAMILY, 0 DO DUP 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP 3PI OVER + LOOP DROP DROP ;
: FAMILY|R 0 DO DUP FIR OVER + LOOP DROP DROP ;
( --assembler_commaers ) \ A4sep27 AvdH
: lsbyte, SPLIT C, ;
: (W,) lsbyte, lsbyte, DROP ;
: (L,) lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

( O=obligatory R=Relative I=Immediate )
' (W,) ALIAS OW,        ' (L,) ALIAS IL,
' (L,) ALIAS (RL,)      ' (W,) ALIAS IW,
' (W,) ALIAS (RW,)      ' C,   ALIAS IB,
' C,   ALIAS (RB,)      ' (L,) ALIAS L,
' (W,) ALIAS SG,        ' (W,) ALIAS W,
' C,   ALIAS P,         ' C,   ALIAS B,
' C,   ALIAS IS,



( --assembler_i86_opcodes_1 )                  \ A4sep27 AvdH
08 06 4 1FAMILY, PUSH|ES, PUSH|CS, PUSH|SS, PUSH|DS,
08 07 4 1FAMILY, POP|ES, -- POP|SS, POP|DS,
08 26 4 1FAMILY, ES:, CS:, SS:, DS:,
08 27 4 1FAMILY, DAA, DAS, AAA, AAS,
01 00 2 FAMILY|R B'| X'|
08 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A,
                 XORI|A, CMPI|A,
02 A0 2 1FAMILY, MOV|TA, MOV|FA,

70 1PI J,  ( As in J, L| Y| <CALC> S, )
    01 00 2 FAMILY|R Y| N|
    02 00 8 FAMILY|R O| C| Z| CZ| S| P| L| LE|

08 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
90 1PI XCHG|AX,
( --assembler_i86_opcodes_2 )                  \ A4sep27 AvdH
08 00 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
02 84 2 2FAMILY, TEST, XCHG,
01 98 8 1FAMILY, CBW, CWD, IR2, WAIT, PUSHF, POPF, SAHF, LAHF,
02 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
08 B0 2 1FAMILY, MOVI|BR, MOVI|XR,
08 C3 2 1FAMILY, RET,  RETFAR,  08 C2 2 1FAMILY, RET+, RETFAR+,
01 C4 2 2FAMILY, LES, LDS,  00C6 2PI MOVI,   0CD 1PI INT,
01 CC 4 1FAMILY, INT3, -- INTO, IRET,
01 D4 4 1FAMILY, AAM, AAD, -- XLAT,
01 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
02 E4 2 1FAMILY, IN|P, OUT|P,  2 EC 2 1FAMILY, IN|D, OUT|D,
01 E8 2 1FAMILY, CALL, JMP,

0088 2PI MOV,           008C 2PI MOV|SG,        008D 2PI LEA,
EA 1PI JMPFAR,  EB 1PI JMPS,    9A 1PI CALLFAR, A8 1PI TESTI|A,
( --assembler_i86_opcodes_3 ) \ A2oct21 AvdH
01 F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
01 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD, ( 38FE)
800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI,
    CMPI,
0800 83 8 2FAMILY, ADDSI, -- ADCSI, SBBSI, -- SUBSI, -- CMPSI,
800 10F6 6 2FAMILY, NOT, NEG, MUL|AD, IMUL|AD, DIV|AD, IDIV|AD,
0800 00FE 2 2FAMILY, INC, DEC,
0800 10FF 4 2FAMILY, CALLO, CALLFARO, JMPO, JMPFARO,

0200 0000 2 FAMILY|R 1| V|
0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,
0800 C0 8 2FAMILY, ROLI, RORI, RCLI, RCRI, SHLI, SHRI, -- SARI,

00F6 2PI TESTI,         008F 2PI POP,           30FF 2PI PUSH,
00,AF0F 3PI IMUL,
