

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
: CHECK1 HERE 3 CELLS - DUP @ SWAP CELL+ @ INVERT  AND 31 ?ERROR ;
( Accept a MASK with a bit up for each commaer, a MASK indicating
( which bits are postitted, and the INSTRUCTION )
( Assemble an 1..3 byte instruction and post what is missing.)
: 1PI <BUILDS  , FFFFFF00 OR , INVERT , CHECK1
DOES> [ HERE TEMP ! ] <POST POST, , 1 CORRECT ;
( Return for DEA : it IS of type 1PI                                  )
IS-A IS-1PI
: 2PI <BUILDS  , FFFF0000 OR , INVERT , CHECK1 DOES>
DOES> [ HERE TEMP ! ] <POST POST, , 2 CORRECT ;
IS-A IS-2PI
: 3PI <BUILDS  , FF000000 OR , INVERT , CHECK1 DOES>
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

: CHECK DUP PREVIOUS @ < 30 ?ERROR DUP PREVIOUS ! ;
: BOOKKEEPING CHECK TALLY OR! ;
( Build with the LENGTH to comma the ADDRESS that is executint the comm )
( and a MASK with the bit for this commaer.                             )
: COMMAER <BUILDS  SWAP , , ,
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

00 FF 00 1PI NOP       00 FF C9 1PI RET       00 FF 76 1PI HLT
00 FF T! 08 07 8 1FAMILY, RLC RRC RAL RAR DAA CMA STC CMC
00 FF T! 08 E3 4 1FAMILY, XTHL XCHG DI EI
00 FF T! 08 E9 2 1FAMILY, PCHL SPHL
00 FF T! 01 C7 8 1FAMILY, RST0 RST1 RST2 RST3 RST4 RST5 RST6 RST7

00 07 T! 01 00 8 xFAMILY| B| C| D| E| H| L| M| A| ( src)
00 F8 T! 08 80 8 1FAMILY, ADD ADC SUB SBB ANA XRA ORA CMP ( B|)

00 30 T! 10 00 4 xFAMILY| BC| DE| HL| SP|
00 CF T! 01 02 2 1FAMILY, STAX INX               ( BC|)
00 CF T! 01 09 3 1FAMILY, DAD LDAX DCX           ( BC|)
02 CF 01 1PI LXI ( BC| IX,)
00 3F 28 xFI PSW|
00 CF T! 04 C1 2 1FAMILY, POP PUSH               ( BC|)
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >2<"
( With immediate data )
08 FF T! 08 D3 2 1FAMILY, OUT IN     ( P,)
01 FF T! 08 C6 8 1FAMILY, ADI ACI SUI SBI ANI XRI ORI CPI  ( I,)

( With an address)
04 FF T! 08 22 4 1FAMILY, SHLD LHLD STA LDA ( X,)
04 FF T! 08 C3 2 1FAMILY, JMP CALL ( X,)

00 38 T! 08 00 8 xFAMILY| B'| C'| D'| E'| H'| L'| M'| A'| ( dst)
00 C7 T! 01 04 2 1FAMILY, INR DCR       ( B'|)
00 C0 40 1PI MOV ( B'| B|)     01 C7 06 1PI MVI ( B'| I,)

00 30 T! 10 00 4 xFAMILY| ZR| CY| PE| LS|
00 08 T! 08 00 2 xFAMILY| N| Y|
00 C7 C0 1PI RC, ( ZR| Y| )
04 C7 T! 02 C2 2 1FAMILY, JC, CC, ( ZR| Y| T, )
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
( Execute the DEA with as data the                                        )
( NAMEFIELD that is given plus for all other words in                     )
(   the same vocabulary.,                                               )
: FOR-REMAINING-AS
BEGIN
2DUP SWAP %EXECUTE
 >NEXT%
DUP DICTEND? UNTIL
DROP DROP
;
( Leave the first DEA of the assembler vocabulary.                    )
: START ' ASSEMBLER 2 +  CELL+ @ ;
( Execute the DEA with as data each time                              )
( the namefield of the assembler vocabulary.                          )
( a dea can be found using % )
: FOR-ALL-AS START FOR-REMAINING-AS ;
% ID. FOR-ALL-AS

% LXI IS-1PI ." LXI: " . CR
% B| IS-1PI ." B|: " . CR
% LXI IS-xFI ." LXI: " . CR
% B| IS-xFI ." B|: " . CR
( print name if tos is a postit )
: PIFPOST DUP IS-1PI IF ID. CR ELSE DROP THEN ;
( print name if tos is a fixup )
: PIFFIX DUP IS-xFI IF ID. CR ELSE DROP THEN ;


( %MYSELF %EXECUTE in a definition is the same as recurse)
: %MYSELF LATEST [COMPILE] LITERAL ; IMMEDIATE

." There comes the posts"
 % PIFPOST FOR-ALL-AS
." There comes the fixs"
%  PIFFIX FOR-ALL-AS
: >BODY PFA CELL+ ;
: >INST >BODY @ ;
: >MASK >BODY CELL+ @ ;
: >COMMA >BODY CELL+ CELL+ @ ;
(   The FIRST set is contained in the SECOND set, leaving IT            )
: CONTAINED-IN OVER AND = ;
% MOV >INST  H.
% MOV >MASK  H.
% MOV >COMMA H.

: SET+! DUP >R @ ! 0 CELL+ R> +! ;
: SET <BUILDS HERE CELL+ , CELLS ALLOT DOES> ;
: -SET DUP CELL+ SWAP ! ;
: .SET DUP @ SWAP DO I . 0 CELL+ +LOOP ;
12 SET DISS
DISS 10 DUMP
123456 DISS SET+!
DISS 10 DUMP
DISS .SET

: -DISS DISS -SET ;
: .DISS DISS DUP @ SWAP CELL+ DO
    I @ DUP IS-COMMA IF I DISS - . THEN ID.
 0 CELL+ +LOOP CR ;
: +DISS DISS SET+! ;
87654 +DISS
DISS .SET
-DISS

." TO HIER?"
^
: DO-FIX
    DUP IS-xFI IF
        DUP >MASK ( DUP H.) TALLY CELL+ @ INVERT  ( DUP H.)
        CONTAINED-IN IF
            DUP >BODY FIX| DROP
            +DISS
        ELSE DROP THEN
    ELSE DUP IS-COMMA IF
        DROP
    ELSE
        DROP
    THEN THEN ;
% MOV >MASK TALLY !
% MOV >COMMA TALLY CELL+ !
% DO-FIX % MOV FOR-REMAINING-AS
% LXI >MASK TALLY !
% LXI >COMMA TALLY CELL+ !
(   % DO-FIX % LXI FOR-REMAINING-AS                                     )

( Reconstruct from DEA an instruction with fixups. )
: DO-INST
    !TALLY
    DUP >BODY POST, DROP
    DUP +DISS
    [ % DO-FIX ] LITERAL SWAP FOR-REMAINING-AS
    .DISS
    -DISS
;
: ONLY-DO-INST
   DUP IS-1PI IF DO-INST ELSE DROP THEN
;
    % MOV DO-INST
    % LXI DO-INST



    % ONLY-DO-INST FOR-ALL-AS

: BACKTRACK
    AT-REST? IF
        .DISS
        !TALLY
(       DISS @ 1 CELLS - @                                                  )
    THEN
;

( These dissassemblers are quite similar:
  If the precondition is fullfilled it does the reassuring
  actions toward the tally as with assembling and add the 
  fixup/posti/commaer to the disassembly struct.  )
: DIS-1PI
    AT-REST? IF
        DUP >BODY POST, DROP
        DUP +DISS
        BACKTRACK                                                   
    THEN
;      

: DIS-xFI
   DUP >MASK TALLY CELL+ @ INVERT CONTAINED-IN IF
       DUP >BODY FIX| DROP
       DUP +DISS
   BACKTRACK                                                         
   THEN
;          
: DIS-COMMA
   DUP >BODY @ TALLY @ INVERT CONTAINED-IN IF
       DUP >BODY @ TALLY OR!
       DUP +DISS
       BACKTRACK                                                         
   THEN
;          
: DOIT
    -DISS 
    !TALLY
    START 
    BEGIN ^
        DUP IS-1PI   IF DUP ID. DIS-1PI THEN 
        DUP IS-xFI   IF DUP ID. DIS-xFI THEN 
        DUP IS-COMMA IF DUP ID. DIS-COMMA THEN 
    >NEXT% DUP DICTEND? UNTIL
    DROP
;

." COMES JAN"
(   CODE JAN MOV B| M'| LXI BC| 1223 IX, NEXT C;                        )
