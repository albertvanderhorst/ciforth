( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
VOCABULARY ASSEMBLER IMMEDIATE
( This file `asgen.frt' contains generic tools and is usable at least   )
( for making assemblers for e.g. 8080 8086 80386 Pentium 6809 68000     )
( 6502 8051.                                                            )
( Most instruction set follow this basic idea that it contains of three )
( distinct parts:                                                       )
(   1. the opcode that identifies the operation                         )
(   2. modifiers such as the register working on                        )
(   3. data, including addresses or offsets.                            )
( This assembler goes through three stages for each instruction:        )
(   1. postit: assemblers the opcode with holes for the modifiers.      )
(      This has a fixed length. Also posts requirements for commaers.   )
(   2. fixup: fill up the holes, either from the beginning or the       )
(     end of the post. These can also post required commaers            )
(   3. The commaers. Any user supplied data in addition to opcode.      )
(      Each has a separate command, where checks are built in.          )
( Keeping track of this is done by a bit array called TALLY, similar    )
( to the a.i. blackboard concept.                                       )
( This setup allows a complete check of validity of code and complete   )
( control over what code is generated. Even so all checks can be        )
( defeated if need be.                                                  )
( The generic tools include:                                            )
(   - the defining words for 1 2 3 byte postits,                        )
(   -  for fixups from front and behind and                             )
(   -  for comma-ers,                                                   )
(   - showing a list of possible instructions, for all opcodes or       )
(   -  for a single one.                                                )
(   -  disassembly of a single instruction or a range                   )
( To write an assembler, make the tables, generate the complete list    )
( of instructions, assemble it and disassemble it again. If equal, you  )
( have a starting point for confidence in your work.                    )

( THIS CODE IS UTTERLY BIG-ENDIAN DEPENDANT!                            )
( AT PLACES A 32 BIT MACHINE IS ASSUMED!                                )
( IT USES THE VOCABULARY AS A LINKED LIST OF STRUCTS: FIGFORTH  

( Returns the DEA from the inline word. Like an xt in ans. )
( A subsequent `ID.' must print the name of that word      )
: % [COMPILE] ' NFA ;
: >BODY PFA CELL+ ; ( From DEA to the DATA field of `X' *FOR A <BUILDS WORD!!*)
: INVERT -1 XOR ;
1 VARIABLE TABLE 1 , ( I TABLE + @ yields $100^[-x mod 4] ) 
( Rotate X by I bytes left leaving X' Left i.e. such as it appears in ) 
( memory! Not as printed on a big endian machine! ) 
: ROTLEFT TABLE + @ U* OR ; 

: & CURRENT @ @ ID. ; ." TESTING STUFF: &"
( First cell : l.s. byte 4 pairs of bits that are mutually exclusive    )
( remainder contains bits down for each COMMAER still needed. The       )
( actual commaer must match bits in l.s. byte too. This allows to       )
( force consistencies in particular operands sizes.                     )
( Second cell contains bits up to be filled by TALLY:| )
 0 VARIABLE TALLY 0 CELL+ ALLOT  ( 4 BYTES FOR COMMAER 4 FOR INSTRUCTION)
 0 VARIABLE PRO-TALLY 0 CELL+ ALLOT  ( Prototype for TALLY)
 0 VARIABLE ISS  ( Start of current instruction)
 0 VARIABLE ISL  ( Lenghth of current instruction)
 0 VARIABLE PREVIOUS ( Previous comma, or zero)
: !POST HERE ISS !  0 PREVIOUS ! ;
: @+ >R R CELL+ R> @ ;
: !TALLY -1 TALLY ! -1 TALLY CELL+ ! ;
HEX
( The `INCONSISTENCY-PAIRS' only serve to detect mutually exclusive     )
( instrcution pairs. All of other bits of the `TALLY' are to be filled  )
( up before an instruction is considered completed. It is a             )
( configuration item because 4 pairs may sometimes not be enough.       )
FF VARIABLE INCONSISTENCY-PAIRS
( Return: instruction IS complete, or not started)
: AT-REST? TALLY @ INCONSISTENCY-PAIRS @ OR -1 = TALLY CELL+ @ -1 = AND ;
( For N: it CONTAINS badpairs)
: BADPAIRS? INVERT DUP 2 * AND AAAAAAAA AND INCONSISTENCY-PAIRS @ AND ; 
( Return : there IS an inconsistency, i.e. even and subsequent ) 
( odd bit in the inverted `TALLY' byte are both up. ) 
: INCONSISTENT? TALLY @ BADPAIRS? ; 
( For the `>COMMA' FIELD return : there would not BE an inconsistency   )
: COMPATIBLE? TALLY @ AND BADPAIRS? 0= ; 
DECIMAL
: CHECK26 AT-REST? 0= 26 ?ERROR ;
: CHECK32 INCONSISTENT? 32 ?ERROR ;
( Based on DATAFIELD of a postit, tally it)
: TALLY:, CELL+ @+ TALLY CELL+ ! @+ TALLY ! @ ISL ! ;
( Correct dictionary to have an instruction of N bytes, after           )
( `,' allocated a whole cell)
: CORRECT 0 CELL+ MINUS + ALLOT ;
: DO-POST CHECK26 CHECK32 !POST DUP TALLY:, @ , ISL @ CORRECT ;
HEX
0 VARIABLE TEMP ( Should be passed via the stack )
( Build a word that tests for same type compared to stored ) 
( in `TEMP'. Execution: Leave for DEA : it IS of same type ) 
: IS-A <BUILDS TEMP @ , DOES> @ SWAP PFA CFA CELL+ @ = ;
( Generate error on data for postit/fixup, if the bits to fill in     )
( stick out of the mask.                                                )
: CHECK31 HERE 4 CELLS - DUP @ SWAP CELL+ @ INVERT  AND 31 ?ERROR ;

( Apply to 1PI ..3PI 1FI ..3FI . Defines data layout. All work from DEA )
: >INST >BODY @ ;  ( Get INSTRUCTION/FIXUP )
: >MASK >BODY CELL+ @ ; ( Get the BITS in the code that become valid.)
: >COMMA >BODY CELL+ CELL+ @ ; ( WHICH comma-actions are expected? )
( How MANY bytes is the valid part of the postit/ valid part of fixup ) 
( Bits may be up outside this part and are useful for forcing consistency ) 
: >CNT >BODY CELL+ CELL+ CELL+ @ ;
( Dissassembly routine for commaer, kicks in of >CNT is zero. )
: >DIS >BODY CELL+ CELL+ CELL+ CELL+ @ ;
( Accept a MASK with a bit up for each commaer, a MASK indicating       )
( which bits are missing from postitted, and the INSTRUCTION )
( Assemble an 1..3 byte instruction and post what is missing.)
( Data : instruction, instruction mask, comma mask, length. See >INST ..)
: 1PI <BUILDS  , INVERT , INVERT , 1 , CHECK31
DOES> [ HERE TEMP ! ] DO-POST ;
( Return for DEA : it IS of type 1PI                                  )
IS-A IS-1PI
: 2PI <BUILDS  , INVERT , INVERT , 2 , CHECK31 
DOES> [ HERE TEMP ! ] DO-POST ;
IS-A IS-2PI
: 3PI <BUILDS  , INVERT , INVERT , 3 , CHECK31 
DOES> [ HERE TEMP ! ] DO-POST ;
IS-A IS-3PI
: IS-PI  >R R IS-1PI R IS-2PI R IS-3PI OR OR R> DROP ;
DECIMAL
: CHECK28 2DUP AND 28 ?ERROR ;
( Or DATA into ADDRESS. If bits were already up its wrong.)
: OR! >R R @  CHECK28 OR R> ! ;
( And DATA into ADDRESS. If bits - apart from inconsistency - were      )
( already down its wrong.                                               )
: CHECK29 2DUP OR INCONSISTENCY-PAIRS @ OR INVERT 29 ?ERROR   ;
: AND! >R R @ CHECK29 AND R> ! ;
(   Based on PFA of a fixup fix into tally)
: TALLY:| CELL+ @+ TALLY CELL+ OR! @ TALLY AND! ;

( Note: the mask is inverted compared to postit, such that a            )
( postit and a fixup that go together have the same mask                )
( Accept a MASK with a bit up for each commaer, a MASK indicating       )
( which bits are fixupped, and the FIXUP )
( The fixup is a string of 0 CELL+ byte that is masked in this order    )
( from the beginning of the instruction.                                )
( One size fits all. Due to the mask )
: xFI <BUILDS , , INVERT , 0 , CHECK31
DOES> [ HERE TEMP ! ] DUP TALLY:| @ ISS @ OR! ;
IS-A IS-xFI

( Rotate the MASK etc from a fixup-from-reverse into a NEW mask fit ) 
( for using for a straight instruction. ) 
: CORRECT-I 0 CELL+ ISL @ - ROTLEFT ;
(   Based on PFA of a fixup: fix into tally )
: TALLY:|R CELL+ @+ CORRECT-I TALLY CELL+ OR! @ TALLY AND! ;

( Generate a reverse fixup: a string of `0 CELL+' bytes that is masked  )
( in from behind from the end of the instruction.                       )
( Accept a MASK with a bit up for each commaer, a MASK indicating       )
( which bits are fixupped, and the FIXUP .                              )
( One size fits all. Due to the mask                                    )
: xFIR <BUILDS , , INVERT , 0 , CHECK31
DOES> [ HERE TEMP ! ] DUP TALLY:|R @ ISS @ ISL @ + 0 CELL+ - OR! ;
IS-A IS-xFIR

( Note: the mask is inverted for a fixup compared to postit, such that  )
( a postit and a fixup that go together have a masks differing in a     )
( shift over whole byte.                                                )

HEX  0 VARIABLE TABLE FF , FFFF , FFFFFF , FFFFFFFF ,  DECIMAL
( From a MASK leave only SOME first bytes up, return IT ) 
( First means lower in memory, this looks different if printed on b.e.) 
: FIRSTBYTES CELLS TABLE + @ AND ;
: >IMASK >CNT FIRSTBYTES ; 

: CHECK30 DUP PREVIOUS @ < 30 ?ERROR DUP PREVIOUS ! ;
: BOOKKEEPING CHECK32 CHECK30 TALLY OR! ;
( Build with the LENGTH to comma the ADDRESS that is executing the comm )
( and a MASK with the bit for this commaer, and a disassembly routine   )
: COMMAER <BUILDS  , , DUP , , ,
DOES> [ HERE TEMP ! ] @+ BOOKKEEPING   @ EXECUTE ;
IS-A IS-COMMA

( Fill in the tally prototype with COMMAMASK and INSTRUCTIONMASK )
: T! PRO-TALLY CELL+ ! PRO-TALLY ! ;

( prepare THE THREE CELLS for an instruction )
: PREPARE >R PRO-TALLY @ PRO-TALLY CELL+ @ R> ;
( From `PRO-TALLY' and the  INSTRUCTION code)
( By INCREMENTing the OPCODE a NUMBER of times generate as much )
(  instructions)
: 1FAMILY, 0 DO DUP PREPARE 1PI OVER + LOOP DROP DROP ;
: 2FAMILY, 0 DO DUP PREPARE 2PI OVER + LOOP DROP DROP ;
: 3FAMILY, 0 DO DUP PREPARE 3PI OVER + LOOP DROP DROP ;

: xFAMILY| 0 DO DUP PREPARE xFI OVER + LOOP DROP DROP ;
: xFAMILY|R 0 DO DUP PREPARE xFIR OVER + LOOP DROP DROP ;

HEX 
(   Given a DEA, return the next DEA. Skip holes, with name -xxx )
: >NEXT% BEGIN  PFA LFA @   DUP 1+ C@ &- - UNTIL ;
( The CONTENT of a linkfield is not a dea, leave: it IS the endmarker   )
: VOCEND? @ FFFF AND A081 = ;
( Leave the first DEA of the assembler vocabulary.                    )
: STARTVOC ' ASSEMBLER 2 +  CELL+ @ ;
(   The FIRST set is contained in the SECOND set, leaving it IS       )
: CONTAINED-IN OVER AND = ;
: SET <BUILDS HERE CELL+ , CELLS ALLOT DOES> ;
( Add ITEM to the SET )
: SET+! DUP >R @ ! 0 CELL+ R> +! ;
( Make the SET empty )
: !SET DUP CELL+ SWAP ! ;
( Print the SET )
: .SET @+ SWAP DO I . 0 CELL+ +LOOP ;
( For the SET : it IS non-empty )
: SET? @+ = 0= ;
12 SET DISS

: !DISS DISS !SET ;
: .DISS DISS @+ SWAP DO
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
    DUP IS-PI IF
    AT-REST? IF
        DUP >BODY TALLY:,
        DUP +DISS
    THEN
    THEN
;

: DIS-xFI
   DUP IS-xFI IF
   DUP >MASK TALLY CELL+ @ INVERT CONTAINED-IN IF
       DUP >BODY TALLY:|
       DUP +DISS
   THEN
   THEN
;
: DIS-xFIR
   DUP IS-xFIR IF     
   DUP >MASK CORRECT-I TALLY CELL+ @ INVERT CONTAINED-IN IF
       DUP >BODY TALLY:|R
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
        DISS @+ SWAP !DISS
        DO
            I @ DIS-PI DIS-xFI DIS-xFIR DIS-COMMA DROP
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

( If the disassembly contains something: `AT-REST?' means               )
( we have gone full cycle rest->postits->fixups->commaers               )
( Return: the disassembly CONTAINS a result.                             )
: RESULT? AT-REST? DISS? AND   INCONSISTENT? 0= AND ;

: RESULT
    RESULT? IF
        .DISS
        REBUILD
    THEN
;

     % RESULT +DISS                                                      
( Try to expand the current instruction in `DISS' by looking whether    )
( DEA fits. Leave the NEXT dea.                                         )
: SHOW-STEP    
        DIS-PI DIS-xFI DIS-xFIR DIS-COMMA
        RESULT
        >NEXT%
(       DUP ID.                                                         )
        INCONSISTENT? IF BACKTRACK THEN
        BEGIN DUP VOCEND? DISS? AND WHILE BACKTRACK REPEAT
;

( Show all the instructions present in the assembler vocabulary )
: SHOW-ALL
    !DISS
    !TALLY
    STARTVOC BEGIN
       SHOW-STEP       
    DUP VOCEND? UNTIL DROP
;

( Show at least all instructions valid for the "OPCODE" given. )
: SHOW:
    !DISS
    !TALLY
    % DUP BEGIN
        SHOW-STEP
     OVER DISS CELL+ @ - OVER VOCEND? OR UNTIL DROP DROP
;

0 VARIABLE POINTER
HERE POINTER !

( Get the valid part of the INSTRUCTION under examination               )
: INSTRUCTION  ISS @ @   ISL @   FIRSTBYTES ;

( These disassemblers are quite similar:                                )
( if the DEA on the stack is of the right type and if the               )
( precondition is fullfilled and if the dissassembly fits,              )
( it does the reassuring actions toward the tally as with               )
( assembling and add the fixup/posti/commaer to the                     )
( disassembly struct.                                                   )
( Leave the DEA.                                                        )
: dis-PI
    DUP IS-PI IF
    AT-REST? IF
    DUP >MASK OVER >IMASK POINTER @ @ AND OVER >INST = IF
        DUP >BODY TALLY:,
        DUP +DISS
        POINTER @ ISS !
        DUP >CNT POINTER +!   
    THEN
    THEN
    THEN
;
: dis-xFI
   DUP IS-xFI IF
   DUP >MASK   TALLY CELL+ @ INVERT   CONTAINED-IN IF
   DUP >MASK   INSTRUCTION AND   OVER >INST = IF
   DUP >COMMA  COMPATIBLE? IF
       DUP >BODY TALLY:|
       DUP +DISS
   THEN
   THEN
   THEN
   THEN
;
: dis-xFIR
   DUP IS-xFIR IF     
   DUP >MASK CORRECT-I   TALLY CELL+ @ INVERT   CONTAINED-IN IF
   DUP >MASK CORRECT-I   INSTRUCTION AND   OVER >INST CORRECT-I = IF
   DUP >COMMA  COMPATIBLE? IF
       DUP >BODY TALLY:|R
       DUP +DISS
   THEN
   THEN
   THEN
   THEN
;

: dis-COMMA
   DUP IS-COMMA IF
   DUP >BODY @ TALLY @ INVERT CONTAINED-IN IF
       DUP >BODY @ TALLY OR!
       DUP +DISS
   THEN
   THEN
;

( Print the disassembly for the DEA , it must be a comma-er   )
: .COMMA
    POINTER @ @ OVER >IMASK U.
    DUP >CNT POINTER +!
    ID.
;
: .DISS' DISS @+ SWAP DO
    I @ DUP IS-COMMA 0= IF
        ID.
    ELSE DUP >CNT  IF
       .COMMA       ( DEA -- )
    ELSE
       POINTER @ OVER >DIS EXECUTE POINTER !
    THEN
    THEN
 0 CELL+ +LOOP CR ;

( Dissassemble one instruction from `POINTER'. )
( Based on what is currently left in `TALLY!' )
( Leave `POINTER' pointing after that instruction. )
: ((DISASSEMBLE)) 
    POINTER @ >R
    STARTVOC BEGIN
        dis-PI dis-xFI dis-xFIR dis-COMMA
        >NEXT%
(       DUP ID. ." : " .DISS                                            )
    DUP VOCEND? RESULT? OR UNTIL DROP
    RESULT? IF
      R> DROP
      .DISS'
    ELSE
      R> COUNT . POINTER ! ."  C," CR
    THEN
;

( As `((DISASSEMBLE))' but starting with a clean slate.)
: (DISASSEMBLE) !DISS !TALLY ((DISASSEMBLE)) ;

( Forced dissassembly of one instruction from `POINTER'. )
( Force interpretation as DEA instruction. )
( This is useful for instructions otherwise hidden in the dictionary. )
: F-D  !DISS   !TALLY   dis-PI DROP   ((DISASSEMBLE)) ;

: DDD (DISASSEMBLE) ;

( Dissassemble one instruction from ADDRESS. )
: D-F-A POINTER ! (DISASSEMBLE) ;
: DIS-RANGE
    SWAP POINTER !
    BEGIN (DISASSEMBLE) POINTER @ OVER < 0= UNTIL
    DROP
;
(   : M| ' xxx  REJECT M| ;  To forbid M| xxx  in combination      )
( xxx must be PI or FI not FIR )
: REJECT> NFA DUP >MASK ISS @ @ AND SWAP >INST = 27 ?ERROR ;

( ************************* )
' ASSEMBLER CFA ' ;CODE 4 CELLS + !        ( PATCH ;CODE IN NUCLEUS )
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !TALLY !CSP ; IMMEDIATE
: C; CURRENT @ CONTEXT ! ?EXEC CHECK26 CHECK32 SMUDGE ; IMMEDIATE
: LABEL ?EXEC 0 VARIABLE SMUDGE -2 ALLOT [COMPILE] ASSEMBLER
    !CSP ; IMMEDIATE     ASSEMBLER DEFINITIONS


