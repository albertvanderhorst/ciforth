

( First cell : contains bits down for each COMMAER still needed)
( Second cell contains bits up to be filled by FIX| )
 0 VARIABLE TALLY 0 CELL+ ALLOT  ( 4 BYTES FOR COMMAER 4 FOR INSTRUCTION)
 0 VARIABLE PRO-TALLY 0 CELL+ ALLOT  ( Prototype for TALLY)
 0 VARIABLE ISS  ( Start of current instruction)
 0 VARIABLE PREVIOUS ( Previous comma, or zero)
: <POST HERE ISS !  0 PREVIOUS ! ;
: @+ >R R CELL+ R> @ ;
: !TALLY -1 TALLY ! -1 TALLY CELL+ ! ;
( Return: instruction IS complete, or not started)
: AT-REST? TALLY @ -1 =   TALLY CELL+ @ -1 = AND ;
: ?TALLY AT-REST? 0= 26 ?ERROR ;
(   Based on PFA of a postit POST into tally and leave the INSTRUCTION  )
: POST, ?TALLY @+ SWAP @+ TALLY CELL+ ! @ TALLY ! ;
( Correct dictionary to have an instruction of N bytes, after
( `POST,' allocated a whole cell)
: CORRECT 0 CELL+ MINUS + ALLOT ;
: INVERT -1 XOR ;
HEX
0 VARIABLE TEMP ( Should be passed via the stack )
( Build a word that tests whether it of same type as stored )
( in `TEMP'. Execution: Leave for DEA : it IS of same type )
: IS-A <BUILDS TEMP @ , DOES> @ SWAP PFA CFA CELL+ @ = ;
( Generate error if data for postit defining word was inconsistent)
: CHECK1 HERE 4 CELLS - DUP @ SWAP CELL+ @ INVERT  AND 31 ?ERROR ;
( Accept a MASK with a bit up for each commaer, a MASK indicating
( which bits are missing from postitted, and the INSTRUCTION )
( Assemble an 1..3 byte instruction and post what is missing.)
( The last masks are for convenience in disassembly                     )
: 1PI <BUILDS  , INVERT , INVERT , 1 , CHECK1
DOES> [ HERE TEMP ! ] <POST POST, , 1 CORRECT ;
( Return for DEA : it IS of type 1PI                                  )
IS-A IS-1PI
: 2PI <BUILDS  , INVERT , INVERT , 2 , CHECK1 DOES>
DOES> [ HERE TEMP ! ] <POST POST, , 2 CORRECT ;
IS-A IS-2PI
: 3PI <BUILDS  , INVERT , INVERT , 3 , CHECK1 DOES>
DOES> [ HERE TEMP ! ] <POST POST, , 3 CORRECT ;
IS-A IS-3PI
DECIMAL
( Or DATA into ADDRESS. If bits were already up its wrong.)
: OR! >R R @    2DUP AND 28 ?ERROR   OR R> ! ;
: AND! >R R @ 2DUP OR -1 - 29 ?ERROR   AND R> ! ;
(   Based on PFA of a fixup fix into tally and leave the FIXUi  )
: FIX| @+ SWAP @+ TALLY CELL+ OR! @ TALLY AND! ;

( Accept a MASK with a bit up for each commaer, a MASK indicating
( which bits are fixupped, and the FIXUP )
( One size fits all. )
: xFI <BUILDS , , INVERT , CHECK1 DOES> [ HERE TEMP ! ] FIX| ISS @ OR! ;
IS-A IS-xFI

: >BODY PFA CELL+ ; ( From % X to the data field of X )
: >INST >BODY @ ;  ( Get you at fixup too)
: >MASK >BODY CELL+ @ ;
: >COMMA >BODY CELL+ CELL+ @ ;
HEX
0 VARIABLE TABLE FF , FFFF , FFFFFF , FFFFFFFF ,
DECIMAL
: >CNT >BODY CELL+ CELL+ CELL+ @ ;
: >IMASK >CNT CELLS TABLE + @ ;

: CHECK DUP PREVIOUS @ < 30 ?ERROR DUP PREVIOUS ! ;
: BOOKKEEPING CHECK TALLY OR! ;
( Build with the LENGTH to comma the ADDRESS that is executint the comm )
( and a MASK with the bit for this commaer.                             )
: COMMAER <BUILDS  SWAP , , DUP , ,
DOES> [ HERE TEMP ! ] @+ BOOKKEEPING   @ EXECUTE ;
IS-A IS-COMMA

( Fill in the tally prototype with COMMAMASK and INSTRUCTIONMASK )
: T! PRO-TALLY CELL+ ! PRO-TALLY ! ;
( From `TALLY' and the  INSTRUCTION code)
( prepare THE THREE CELLS for an instruction )
: PREPARE >R PRO-TALLY @ PRO-TALLY CELL+ @ R> ;
( By INCREMENTing the OPCODE a NUMBER of times generate number
(  instructions)
: 1FAMILY, 0 DO DUP PREPARE 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP PREPARE 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP PREPARE 3PI OVER + LOOP DROP DROP ;

: xFAMILY| 0 DO DUP PREPARE xFI OVER + LOOP DROP DROP ;

CR ." CASSADY'S 8080 ASSEMBLER 81AUG  >3<" HEX
HEX VOCABULARY ASSEMBLER IMMEDIATE
' ASSEMBLER CFA ' ;CODE 4 CELLS + !        ( PATCH ;CODE IN NUCLEUS )
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !TALLY !CSP ; IMMEDIATE
: C; CURRENT @ CONTEXT ! ?EXEC ?TALLY SMUDGE ; IMMEDIATE
: LABEL ?EXEC 0 VARIABLE SMUDGE -2 ALLOT [COMPILE] ASSEMBLER
    !CSP ; IMMEDIATE     ASSEMBLER DEFINITIONS
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >1<"

( The increasing order means that a decompiler hits them in the         )
( right order                                                           )
1        1   ' C, CFA   COMMAER IB, ( immediate byte data)
0 CELL+  2   ' ,  CFA   COMMAER IX, ( immediate data : cell)
0 CELL+  4   ' ,  CFA   COMMAER X,  ( immediate data : address)
1        8   ' C, CFA   COMMAER P,  ( port number ; byte     )

00 00 T! 08 07 8 1FAMILY, RLC RRC RAL RAR DAA CMA STC CMC
00 00 T! 08 E3 4 1FAMILY, XTHL XCHG DI EI
00 00 T! 10 E9 2 1FAMILY, PCHL SPHL
00 00 T! 08 C7 8 1FAMILY, RST0 RST1 RST2 RST3 RST4 RST5 RST6 RST7

00 07 T! 01 00 8 xFAMILY| B| C| D| E| H| L| M| A| ( src)
00 07 T! 08 80 8 1FAMILY, ADD ADC SUB SBB ANA XRA ORA CMP ( B|)

00 30 T! 10 00 4 xFAMILY| BC| DE| HL| SP|
00 30 T! 01 02 2 1FAMILY, STAX INX               ( BC|)
00 30 T! 01 09 3 1FAMILY, DAD LDAX DCX           ( BC|)
02 30 01 1PI LXI ( BC| IX,)
00 30 30 xFI PSW|
00 30 T! 04 C1 2 1FAMILY, POP PUSH               ( BC|)
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >2<"
( With immediate data )
08 00 T! 08 D3 2 1FAMILY, OUT IN     ( P,)
01 00 T! 08 C6 8 1FAMILY, ADI ACI SUI SBI ANI XRI ORI CPI  ( I,)

( With an address)
04 00 T! 08 22 4 1FAMILY, SHLD LHLD STA LDA ( X,)
04 00 T! 08 C3 2 1FAMILY, JMP CALL ( X,)

00 38 T! 08 00 8 xFAMILY| B'| C'| D'| E'| H'| L'| M'| A'| ( dst)
00 38 T! 01 04 2 1FAMILY, INR DCR       ( B'|)
00 3F 40 1PI MOV ( B'| B|)     01 38 06 1PI MVI ( B'| I,)

00 30 T! 10 00 4 xFAMILY| ZR| CY| PE| LS|
00 08 T! 08 00 2 xFAMILY| N| Y|
00 38 C0 1PI RC, ( ZR| Y| )
04 38 T! 02 C2 2 1FAMILY, JC, CC, ( ZR| Y| T, )
00 00 00 1PI NOP       00 00 C9 1PI RET       00 00 76 1PI HLT
' ;S 0B + @ CONSTANT (NEXT)
: NEXT JMP (NEXT) X, ;
: PSH1 JMP (NEXT) 1 - X, ;
: PSH2 JMP (NEXT) 2 - X, ;
: THEN, HERE SWAP ! ;         : HOLDPLACE HERE 0 X, ;
: IF, JC, HOLDPLACE ;  ( ZR| Y| )
: ELSE, JMP HOLDPLACE ;           : BEGIN, HERE ;
: UNTIL, IF, DROP ;                  : WHILE, IF, ;
: REPEAT, SWAP JMP X,  THEN, ;

(   Given a DEA, return the next DEA)
: >NEXT% PFA LFA @ ;
: % [COMPILE] ' NFA ;
( The DEA is in fact not a dea, leave: it IS the endmarker             )
: DICTEND? @ $FFFF AND $A081 = ;
: %EXECUTE PFA CFA EXECUTE ;
( Leave the first DEA of the assembler vocabulary.                    )
: START ' ASSEMBLER 2 +  CELL+ @ ;


(   The FIRST set is contained in the SECOND set, leaving IT            )
: CONTAINED-IN OVER AND = ;

: SET <BUILDS HERE CELL+ , CELLS ALLOT DOES> ;
( Add ITEM to the SET )
: SET+! DUP >R @ ! 0 CELL+ R> +! ;
( Make the SET empty )
: -SET DUP CELL+ SWAP ! ;
( Print the SET )
: .SET DUP @ SWAP DO I . 0 CELL+ +LOOP ;
( For the SET : it IS non-empty )
: SET? DUP @ SWAP CELL+ = 0= ;
12 SET DISS

: -DISS DISS -SET ;
: .DISS DISS DUP @ SWAP CELL+ DO
    I @ DUP IS-COMMA IF I DISS - . THEN ID.
 0 CELL+ +LOOP CR ;
: +DISS DISS SET+! ;
: DISS? DISS SET? ;

( These dissassemblers are quite similar:                               )
( if the DEA on the stack is of the right type and if the precondition  )
( is fullfilled it does the reassuring actions toward the tally as with )
( assembling and add the fixup/posti/commaer to the disassembly struct. )
( Leave the DEA.                                                        )
: DIS-PI
    0   OVER IS-1PI OR   OVER IS-2PI OR   OVER IS-3PI OR   IF
    AT-REST? IF
        DUP >BODY POST, DROP
        DUP +DISS
    THEN
    THEN
;

: DIS-xFI
   DUP IS-xFI IF
   DUP >MASK TALLY CELL+ @ INVERT CONTAINED-IN IF
       DUP >BODY FIX| DROP
       DUP +DISS
   THEN
   THEN
;
: DIS-COMMA
   DUP IS-COMMA IF
   DUP >BODY @ TALLY @ INVERT CONTAINED-IN IF
       DUP >BODY @ TALLY OR!
       DUP +DISS
   THEN
   THEN
;

( Generate the situation back, with one less item in `DISS')
: REBUILD
    0 CELL+ MINUS DISS +!
    !TALLY
    DISS? IF
        DISS @+ SWAP -DISS
        DO
            I @ DIS-PI DIS-xFI DIS-COMMA DROP
        0 CELL+ +LOOP
    THEN
;

( Replace DEA with the next DEA                                         )
( Discard the last item of the disassembly that is either               )
( used up or incorrect                                                  )
: BACKTRACK
(   ." BACKTRACKING"                                                    )
    DROP DISS @ 0 CELL+ - @
    >NEXT%
    REBUILD
;

: RESULT
    AT-REST? DISS? AND IF
        .DISS
        REBUILD
    THEN
;

: DOIT
    -DISS
    !TALLY
    START
    BEGIN
        DIS-PI DIS-xFI DIS-COMMA
        RESULT
        >NEXT%
(       DUP ID.                                                         )
        BEGIN DUP DICTEND? DISS? AND WHILE BACKTRACK REPEAT
    DUP DICTEND? UNTIL
    DROP
;

0 VARIABLE POINTER
0 VARIABLE NEW-POINTER
HERE POINTER !
( Disassemble the instruction at `POINTER' and accumulated into         )
( `DISS'. `POINTER' has advanced to the commadata                       )
: .DISS2 DISS DUP @ SWAP CELL+ DO
    I @ DUP IS-COMMA IF
       POINTER @ .
       DUP >BODY CELL+ POINTER +!
    THEN ID.
 0 CELL+ +LOOP CR ;

( These dissassemblers are quite similar:                               )
( if the DEA on the stack is of the right type and if the               )
( precondition is fullfilled and if the dissassembly fits,              )
( it does the reassuring actions toward the tally as with               )
( assembling and add the fixup/posti/commaer to the                     )
( disassembly struct.
( Leave the DEA.                                                        )
: dis-1PI
    0   OVER IS-1PI OR   OVER IS-2PI OR   OVER IS-3PI OR   IF
    AT-REST? IF
    DUP >MASK OVER >IMASK AND POINTER @ @ AND OVER >INST = IF
        DUP >BODY POST, DROP
        DUP +DISS
        POINTER @ 1+ NEW-POINTER !
(       DUP ID.                                                         )
(       ." BINGA"                                                       )
    THEN
    THEN
    THEN
;

: dis-xFI
   DUP IS-xFI IF
   DUP >MASK TALLY CELL+ @ INVERT CONTAINED-IN IF
   DUP >MASK  POINTER @ @ AND OVER >INST = IF
       DUP >BODY FIX| DROP
       DUP +DISS
(       DUP ID.                                                         )
(       ." BINGU"                                                       )
   THEN
   THEN
   THEN
;

: dis-COMMA
   DUP IS-COMMA IF
   DUP >BODY @ TALLY @ INVERT CONTAINED-IN IF
       DUP >BODY @ TALLY OR!
       DUP +DISS
(       DUP ID.                                                         )
(       ." BINGIIIIIIIIIIIII"                                           )
   THEN
   THEN
;

: VL. CELLS TABLE + @ AND U. ;
( Print the DEA in an appropriate way, it must be a comma-er   )
: .COMMA 
    DUP >IMASK NEW-POINTER @ @ AND U.
    DUP >CNT NEW-POINTER +!              
    ID.
;
: .DISS' DISS DUP @ SWAP CELL+ DO
    I @ DUP IS-COMMA IF 
       .COMMA       ( DEA -- )
    ELSE 
        ID.
    THEN 
 0 CELL+ +LOOP CR ;

( If the disassembly contains something: `AT-REST?' means
( we have gone full cycle rest->postits->fixups->commaers               )
( so the disassembly contains a result.                                 )
: RESULT? AT-REST? DISS? AND  ;
( Dissassemble one instruction from ADDRESS. )
( Leave `POINTER' pointing after that instruction. )
: DOIT2
    -DISS
    !TALLY
    START
    BEGIN
        dis-1PI dis-xFI  dis-COMMA 
        >NEXT%
(       DUP ID.                                                         )
    DUP DICTEND? RESULT? OR UNTIL
    DROP
    RESULT? IF
      .DISS' 
      NEW-POINTER @ POINTER !
    ELSE
      POINTER @ C@ . ."  C,"
      1 POINTER +!
    THEN
;

: D-F-A POINTER ! DOIT2 ;
: DIS-RANGE
    SWAP POINTER !
    BEGIN DOIT2 POINTER @ OVER < 0= UNTIL 
    DROP
;
." COMES JAN"
    CODE JAN MOV B| M'| LXI BC| 1223 IX, NEXT C;                        
    ' JAN HERE DIS-RANGE
' JAN CFA @ D-F-A DOIT2 DOIT2 

CODE JAN
CC, Y| LS| 3 X,
CC, A'| 3 X,
NEXT C;
' JAN CFA @ 10 DUMP
' JAN CFA @ D-F-A DOIT2 DOIT2 

