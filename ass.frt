
( POSTIT/FIXUP 8086 ASSEMBLER LOAD SCREEN AvdH HCC HOLLAND)         
  92 LOAD                                                      
		
( POSTIT/FIXUP 8086 ASSEMBLER SYSDEPENDANT AvdH HCC HOLLAND)   
									       
( AUXILIARY DEFINITIONS ) DECIMAL                               
0 VARIABLE IDP   : <FIX HERE IDP ! ; : IHERE IDP @ ;            
: C| -1 IDP +! ^ IHERE ^ C@ ^ OR ^ IHERE ^ ! ;  ( c.f. C, )     
: C@+ COUNT ;  : C@- 1 - DUP C@ ; ( : C!+ >R R ! R> 1+ ;)       
: MEM, , ; : R, HERE - 2 + , ; : S, C, ;  : W, , ; : SEG, , ;   
: POST, C@+ C, ;       : FIX| C@- C| ;                          
: 1PI <BUILDS C, DOES> POST, DROP ;                             
: 2PI <BUILDS C, C, DOES> POST, POST, DROP ;                    
: 1FI <BUILDS C, DOES> <FIX 1+ FIX| DROP ;                      
: 2FI <BUILDS C, C, DOES> <FIX 2+ FIX| FIX| DROP ;              
: 1FAMILY,  ( INCREMENT, OPCODE , COUNT -- )                    
   0 DO DUP 1PI OVER + LOOP DROP DROP ;                         
: 1FAMILY| 0 DO DUP 1FI OVER + LOOP DROP DROP ;                 
: SPLIT 256 /MOD SWAP ; ( To handle two bytes at once )         
: 2FAMILY, 0 DO DUP SPLIT 2PI OVER + LOOP DROP DROP ;           
: 2FAMILY| 0 DO DUP SPLIT 2FI OVER + LOOP DROP DROP ;           

( POST-IT/FIX-UP 8086 ASSEMBLER , OPCODES AvdH HCCFIG HOLLAND)  
 8 0 4 1FAMILY| ES| CS| SS| DS|    1 4 2 1FAMILY, PUSHS, POPS,  
 HEX 8 26 4 1FAMILY, ES:, CS:, SS:, DS:,                        
 8 27 4 1FAMILY, DAA, DAS, AAA, AAS,                            
 1 0 2 1FAMILY| B1| W1|   08 04 8 1FAMILY, ADDAI, ORAI, ADCAI,  
SBBAI, ANDAI, SUBAI, XORAI, CMPAI, 2 A0 2 1FAMILY, MOVTA, MOVFA,
 1 0 2 1FAMILY| Y| N|   2 0 8 1FAMILY| O| C| Z| CZ| S| P| L| LE|
 70 1PI J,  ( As in J, L| Y| <CALC> S, )                        
 1 0 8 1FAMILY| AX| CX| DX| BX| SP| BP| SI| DI|                 
 08 40 4 1FAMILY, INCX, DECX, PUSHX, POPX,    90 1PI XCHGX,     
 ( C7) 6 1FI MEM|   ( C0) 40 00 4 1FAMILY| D0| DB| DDW| R|      
 ( 38) 08 C0 8 1FAMILY| AX1| CX1| DX1| BX1| SP1| BP1| SI1| DI1| 
 ( 07) 1 0 8 1FAMILY| [BX+SI] [BX+DI] [BP+SI] [BP+DI]           
[SI] [DI] [BP] [BX]                                             
 1 0 8 1FAMILY| AL| CL| DL| BL| AH| CH| DH| BH|                 
								
( POST-IT/FIX-UP 8086 ASSEMBLER , OPCODES AvdH HCCFIG HOLLAND)  
1 0 2 2FAMILY| B| W|   2 0 2 2FAMILY| F| T|                     
8 0 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,           
2 84 2 2FAMILY, TEST, XCHG,   0 88 2PI MOV,                     
( 00FD) 0 8C 2PI MOVSW,   ( 00FE) 0 8D 2PI LEA,                 
( IRR,egular)  ( FF) 9A 1PI CALLFAR,  ( FE) A8 1PI TESTAI, ( FF)
1 98 8 1FAMILY, CBW, CWD, IR2, WAIT, PUSHF, POPF, SAHF, LAHF,   
( FE) 2 A4 6 1FAMILY, MOVS, COMPS, IR3, STOS, LODS, SCAS,       
08 B0 2 1FAMILY, MOVRI, MOVXI,                                  
8 C2 2 1FAMILY, RET+, RETFAR+,  8 C3 2 1FAMILY, RET,  RETFAR,   
1 C4 2 1FAMILY, LES, LDS,  0 C6 2PI MOVI,  0 CD 2PI INTI,       
1 CA 4 1FAMILY, INT3, IRR, INTO, IRET,                          
1 D4 4 1FAMILY, AAM, AAD, IL3, XLAT,                            
1 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,                      
2 E4 2 1FAMILY, INAI, OUTAI,  2 EC 2 1FAMILY, INAD, OUTADI,     
1 E8 2 1FAMILY, CALL, JMP,  E9 1PI JMPFAR,  EA 1PI JMPS,        

( POST-IT/FIX-UP 8086 ASSEMBLER , OPCODES AvdH HCCFIG HOLLAND)  
1 F0 6 2FAMILY, LOCK, ILL, REP, REPZ, HLT, CMC,                 
1 F8 6 2FAMILY, CLC, STC, CLI, STI, CLD, STD, ( 38FE)           
800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI,                  
SUBI, XORI, CMPI,                                               
800 83 8 2FAMILY, ADDSI, IL4, ADCSI, SBBSI, IL5,                
SUBSI, IL6, CMPSI,                                              
800 D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, IL6, RAR,       
800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,             
00 F6 2PI TESTI, 800 FE 2 2FAMILY, INC, DEC,                    
( 38FF) 00 8F 2PI POP,  30 FE 2PI PUSH,                         
800 10FF 4 2FAMILY, CALLO, CALLFARO, JMPO, JMPFARO,             
								
( POST-IT/FIX-UP 8086 ASSEMBLER , POSTLUDE AvdH HCCFIG HOLLAND) 
HEX VOCABULARY ASSEMBLER IMMEDIATE                              
' ASSEMBLER CFA ' ;CODE 8 + !        ( PATCH ;CODE IN NUCLEUS ) 
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !CSP ; IMMEDIATE        
: C; CURRENT @ CONTEXT ! ?EXEC ?CSP SMUDGE ; IMMEDIATE          
: NEXT                                                          
     LODS, W1|                                                  
     MOV, W| F| R| BX| AX1|                                     
     MOV, W| F| BX1| DX|                                        
     INCX, DX|                                                  
     JMPO, D0| [BX]                                             
 ;                                                              
 CODE TEST NEXT  C;                                             
                                                                

