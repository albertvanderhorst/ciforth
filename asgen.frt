 ( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

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
( Keeping track of this is done by bit arrays, similar to the a.i.      )
( blackboard concept. This is ONLY to notify the user of mistakes,      )
( they are NOT needed for the assembler proper.                         )
( This setup allows a complete check of validity of code and complete   )
( control over what code is generated. Even so all checks can be        )
( defeated if need be.                                                  )

( The generic tools include:                                            )
(   - the defining words for 1 2 3 byte postits,                        )
(   -                    for fixups from front and behind               )
(   -                    for comma-ers,                                 )
(   - showing a list of possible instructions, for all opcodes or       )
(   -  for a single one.                                                )
(   -  disassembly of a single instruction or a range                   )
(   - hooks for more tools, e.g. print the opcode map as postscript.    )
( To write an assembler, make the tables, generate the complete list    )
( of instructions, assemble it and disassemble it again. If equal, you  )
( have a starting point for confidence in your work.                    )

( THIS CODE IS UTTERLY BIG-ENDIAN DEPENDANT!                            )
( AT PLACES A 32 BIT MACHINE IS ASSUMED!                                )
( IT USES THE VOCABULARY AS A LINKED LIST OF STRUCTS: FIGFORTH!         )
( IT USED KNOWLEDGE OF THE INTERPRETER AND THE HEADERS!                 )
( Now if you think that this makes this code non-portable, think again. )
( You have to change about 8 lines to adapt. Now if you only have to    )
( adapt 8 lines in a 40k lines c-program with the same functionality,   )
( it would smack portable. Wouldn't it? WOULDN'T IT?                    )

( The blackboard consist of three bit arrays. At the start of an        )
( instruction they are all zero. `TALLY-BI' `TALLY-BY' `TALLY-BA' keep  )
( track of instruction bits, instruction byte and bad things            )
( respectively.                                                         )

( An instructions generally has a single postit that defines the        )
( opcode. It assembles the opcode, advancing `HERE' leaving zero bits   )
( that needs still filling in by fixups. It sets these bits in          )
( `TALLY-BI'. It may also post that commaers are required by setting a  )
( bit in `TALLY-BY'.                                                    )

( Then comes the fixups. They fill up the holes left in the             )
( instruction -- before `HERE' -- by or-ing and maintain `TALLY-BI' ,   )
( resetting bits. They end in `|' where the other assembly actions end  )
( in `,'. They may require more commaers, posting to `TALLY-BY'. The    )
( commaers advance `HERE' by a whole number of bytes assembling user    )
( supplied information and reset the corresponding bits in `TALLY-BY'.  )

( All parts of an instruction can add bits to `TALLY-BA'. If any two    )
( consecutive bits are up this is bad. Its bits can never be reset but  )
( `TALLY-BA' is reset as a whole at the start of an instruction.        )

( An example: load an index register with a 16 bit value, 8080.         )
(    TALLY-BI  TALLY-BY   TALLY-BA     HERE     8A43 4  DUMP            )
(    0000      0000       0801         8A43     .. .. .. ..    LXI,     )
(    0030      0002       0002         8A44     01 .. .. ..    SP|      )
(    0000      0002       0002         8A44     31 .. .. ..    SP0 @ X, )
(    0000      0000       0002         8A46     31 00 FE ..    HLT,     )
(    0000      0000       0000         8A47     31 00 FE 76    ...      )
( The bit in `TALLY-BA' means a 16 bit operation. Now if `TALLY-BA'     )
( contains 3 it would mean that it is at the same time an 8 bit and 16  )
( bit operation. Bad!                                                   )

( The following problems can be detected:                               )
( - postit when `TALLY-BI' or `TALLY-BY' contains bits up               )
( - setting or resetting bits for the second time in `TALLY-BI' or      )
(     `TALLY-BY'                                                        )
( - commaing when `TALLY-BI' still contains bits up                     )
( - setting `TALLY-BA' bad                                              )

( ############### PART I ASSEMBLER #################################### )
( MAYBE NOT PRESENT UTILITIES                                           )
: INVERT -1 XOR ;
: @+ >R R CELL+ R> @ ; ( Fetch from ADDRES. Leave incremented ADDRESS and DATA )
: !+ >R R ! R> CELL+ ; ( Store DATA to ADDRES. Leave incremented ADDRESS)
( Fetch from decremented ADDRES. Leave DATA and ADDRESS)
: @- 0 CELL+ - >R R @ R>  ; 
( CHAR - CONSTANT &-                                                    )
1 VARIABLE TABLE 1 , ( x TABLE + @ yields $100^[-x mod 4] )
( Rotate X by I bytes left leaving X' Left i.e. such as it appears in )
( memory! Not as printed on a big endian machine! )
: ROTLEFT TABLE + @ U* OR ;   ( aqa " 8 * LSHIFT" on bigendian. )

( ------------- UTILITIES, SYSTEM DEPENDANT ----------------------------) 
VOCABULARY ASSEMBLER IMMEDIATE DEFINITIONS HEX
( We use the abstraction of a dea "dictionary entry address". aqa "xt" )
: % [COMPILE] ' NFA ;   ( Return the DEA from "word". )
: %ID. ID. ;   ( Print a definitions name from its DEA.)
: %>BODY PFA CELL+ ; ( From DEA to the DATA field of a created word )
: %BODY> 0 CELL+ - NFA ; ( Reverse of above)
: %>CODE PFA CFA CELL+ ; ( From DEA to the DOES> pointer )

: (>NEXT%) PFA LFA @ ; ( Given a DEA, return the next DEA. )
( For a DEA as returned from (>NEXT%} : it IS the end, not a real dea.  )
: VOCEND? @ FFFF AND A081 = ;
( As (>NEXT%} but skip holes, i.e. words with names starting in ``-''   )
: >NEXT% BEGIN  (>NEXT%) DUP 1+ C@ &- - UNTIL ;
( Leave the first DEA of the assembler vocabulary.                      )
: STARTVOC ' ASSEMBLER 2 +  CELL+ @ ;

( Build: for "word" remember type -- creation class -- exemplified by   )
( DOES> address of the code to be executed.                             )
( Execution: Leave for DEA : it IS of same type. )
: IS-A <BUILDS 0 ( To be patched) , DOES> @ SWAP %>CODE @ = ;
( Patch up the data field of a preceeding word defined by `IS-A'        )
( To be called when sitting at the address wanted                       )
: REMEMBER HERE LATEST (>NEXT%) %>BODY ! ; IMMEDIATE

( Also needed : ?ERROR                                                  )
(   `` : ?ERROR DROP DROP ; '' defeats all checks.                      )

( ------------- UTILITIES, SYSTEM INDEPENDANT ----------------------------) 
(   The FIRST bitset is contained in the SECOND one, leaving it IS      )
: CONTAINED-IN OVER AND = ;
0 VARIABLE TABLE FF , FFFF , FFFFFF , FFFFFFFF ,  
( From a MASK leave only SOME first bytes up, return IT ) 
( First means lower in memory, this looks different if printed on b.e.) 
: FIRSTBYTES CELLS TABLE + @ AND ;

( ------------- ASSEMBLER, BOOKKEEPING ----------------------------) 
( The bookkeeping is needed for error detection and disassembly.        )
0 VARIABLE TALLY-BI  ( Bits that needs fixed up)
0 VARIABLE TALLY-BY  ( Bits represent a commaer that is to be supplied)
0 VARIABLE TALLY-BA  ( State bits, bad if two consequitive bits are up)
0 VARIABLE PREVIOUS ( Previous comma, or zero)
0 VARIABLE ISS  ( Start of current instruction)
0 VARIABLE ISL  ( Lenghth of current instruction)

( Initialise ``TALLY''                                                  )
: !TALLY   0 TALLY-BI !   0 TALLY-BY !   0 TALLY-BA !  0 PREVIOUS ! ;
( Return: instruction IS complete, or not started)
: AT-REST? TALLY-BI @ 0=   TALLY-BY @ 0=  AND ;
: BADPAIRS? DUP 2 * AND AAAAAAAA AND ; ( For N : it CONTAINS bad pairs)
: BAD? TALLY-BA @ BADPAIRS? ;  ( The state of assembling IS inconsistent)
( If STATUS were added to `TALLY-BA' would that CREATE a bad situation? )
: COMPATIBLE? TALLY-BA @ OR BADPAIRS? 0= ;
DECIMAL
( Generate errors. None have net stack effects, such that they may be   )
( replaced by NULL definitions.                                         )
: CHECK26 AT-REST? 0= 26 ?ERROR ;  ( Error at postit time )             
: CHECK32 BAD? 32 ?ERROR ; ( Always an error )                 
( Generate error for fixup, if for the BI, some of the the     )
( BITS would stick out it. Leave MASK and BITS . Programming error!     )
: CHECK31 2DUP SWAP CONTAINED-IN 0= 31 ?ERROR ;                            
( Generate error for postit, if for the inverted BI , some of the the   )
( BITS would stick out it. Leave MASK and BITS . Programming error!     )
: CHECK33 2DUP SWAP INVERT CONTAINED-IN 0= 31 ?ERROR ;                            
( BITS would stick out it. Leave MASK and BITS . Programming error!     )
( Generate error on data for postit/fixup, if some BITS to fill in      )
( are already in the MASK. Leave BITS and MASK.                         )
: CHECK28 2DUP AND 28 ?ERROR ;                                          )
( Generate error on data for commaer, if the BITS to reset are not      )
( present in the MASK. Leave BITS and MASK.                             )
: CHECK29 2DUP OR -1 - 29 ?ERROR   ;
( Generate error if COMMAMASK is not in ascending order. Leave IT.      )
: CHECK30 DUP PREVIOUS @ < 30 ?ERROR DUP PREVIOUS ! ;
HEX
( Or DATA into ADDRESS. If bits were already up its wrong.)
: OR! >R R @  CHECK28 OR R> ! ;
: OR!U >R R @  OR R> ! ; ( Or DATA into ADDRESS. Unchecked.)
( Reset bits of DATA into ADDRESS. If bits were already down it's wrong )
: AND! >R INVERT R @ CHECK29 AND R> ! ;

( ------------- ASSEMBLER, DEFINING WORDS    ----------------------------) 

( Common fields in the defining words for posits fixups and commaers.   )
( All leave a single ADDRESS.                                           )

( The first data field for a postit/fixup contains instruction bits,    )
( for a commaer it contains the xt of the coma action                   )
: >DATA  %>BODY  ;
( Work on TALLY-BI etc.      Effects  for posits fixups and commaers.   )
(                                          |||    |||       |||         )
: >BI %>BODY CELL+ ;                     ( OR!    AND!      --        )
: >BY %>BODY 2 CELLS + ;                 ( OR!    OR!       AND!      )
: >BA %>BODY  3 CELLS + ;                ( OR!U   OR!U      OR!U      )
: >CNT %>BODY 4 CELLS + ;   ( `HERE' advances with count )
: >DIS %>BODY 5 CELLS + ;   ( disassembler only for COMMA)

( Adjust `HERE' for actual instruction length after `DO-POST' did `,' ) 
: CORRECT,- ISL @   1 CELLS -  ALLOT ; 
: !POSTIT  HERE ISS !  0 PREVIOUS ! ;  ( Initialise in behalf of postit )
( Bookkeeping for a postit using a pointer to the BIBYBA )
( information, can fake a postit in disassembling too                   )
: TALLY:,   @+ TALLY-BI !    @+ TALLY-BY !    @+ TALLY-BA ! @ ISL ! ;
( Post the instruction using DATA. )
: POSTIT   CHECK26   !POSTIT  HERE ISS !
    @+ ,   TALLY:,   CORRECT,- ;
( Define an instruction by BA BY BI and the OPCODE                      )
( The `BI' information is what is left to be filled in cf. fixups       )
IS-A IS-1PI : 1PI  CHECK33 <BUILDS , , , , 1 , DOES> REMEMBER POSTIT ; 
IS-A IS-2PI : 2PI  CHECK33 <BUILDS , , , , 2 , DOES> REMEMBER POSTIT ;
IS-A IS-3PI : 3PI  CHECK33 <BUILDS , , , , 3 , DOES> REMEMBER POSTIT ;
: IS-PI  >R R IS-1PI R IS-2PI R IS-3PI OR OR R> DROP ;

( Bookkeeping for a fixup using a pointer to the BIBYBA information,    )
( can fake a fixup in disassembling too.                                )
: TALLY:|   @+ TALLY-BI AND!   @+ TALLY-BY OR!   @ TALLY-BA OR!U ;  
( Fix up the instruction using a pointer to DATA. )
: FIXUP>   @+ ISS @ OR!   TALLY:|   CHECK32 ;
( Define an fixup by BA BY BI and the FIXUP bits )
( One size fits all, because of the or character of the operations. )
IS-A IS-xFI   : xFI   CHECK31 <BUILDS , , , , DOES> REMEMBER FIXUP> ;

( Rotate the MASK etc from a fixup-from-reverse into a NEW mask fit ) 
( for using from the start of the instruction. We know the length!  )
: CORRECT-R 0 CELL+ ISL @ - ROTLEFT ;
( Bookkeeping for a fixup-from-reverse using a pointer to the BIBYBA    )
( information, can fake a fixup in disassembling too.                   )
: TALLY:|R  @+ CORRECT-R TALLY-BI AND!   @+ TALLY-BY OR!   @ TALLY-BA OR!U ; 
( Fix up the instruction from reverse using a pointer to DATA. )
: FIXUP<   @+ CORRECT-R ISS @ OR!   TALLY:|R  CHECK32 ;
( Define a fixup-from-reverse by BA BY BI and the FIXUP bits )
( One size fits all, because of the character of the or-operations. )
IS-A IS-xFIR   : xFIR   CHECK31 <BUILDS , , , , DOES> REMEMBER FIXUP< ;

( Bookkeeping for a commaer using a pointer to the BIBYBA information.  )
( Not used by the disassembler.                                         )
: TALLY:,, CELL+   @+ CHECK30 TALLY-BY AND!   @ TALLY-BA OR!U ;  
: COMMA @+ >R  TALLY:,,  CHECK32   R> EXECUTE ;
( Build with an disassembly ROUTINE, with the LENGTH to comma, the BA   )
( BY information and the ADDRESS that is executing the commaer          )
IS-A  IS-COMMA   : COMMAER <BUILDS  , 0 , , , , , DOES> REMEMBER COMMA ;

( ------------- ASSEMBLER, SUPER DEFINING WORDS ----------------------) 

 0 VARIABLE PRO-TALLY 2 CELLS ALLOT  ( Prototype for TALLY-BI BY BA )
( Fill in the tally prototype with BA BY BI information )           
: T! PRO-TALLY !+ !+ !+ DROP ;
( Get the data from the tally prototype back BA BY BI )
: T@ PRO-TALLY 3 CELLS +  @- @- @- DROP ;
( Add INCREMENT to the OPCODE a NUMBER of times generate as much        )
( instructions                                                          )
: 1FAMILY,    0 DO   DUP >R T@ R> 1PI   OVER + LOOP DROP DROP ;
: 2FAMILY,    0 DO   DUP >R T@ R> 2PI   OVER + LOOP DROP DROP ;
: 3FAMILY,    0 DO   DUP >R T@ R> 3PI   OVER + LOOP DROP DROP ;
: xFAMILY|    0 DO   DUP >R T@ R> xFI   OVER + LOOP DROP DROP ;
: xFAMILY|R   0 DO   DUP >R T@ R> xFIR  OVER + LOOP DROP DROP ;

( ############### PART II DISASSEMBLER #################################### )

( Tryers try to construct an instruction from current bookkeeping.      )
( They can backtrack to show all possibilities.                         )
( Disassemblers try to reconstruct an instruction from current          )
( bookkeeping. They are similar but disassemblers take one more aspect  )
( into account, a piece of actual code. They do not backtrack but fail. )

( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------) 
( Build a set "x" with X items. )
: SET   <BUILDS HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP DO I . 0 CELL+ +LOOP ;   ( Print non-empty SET )

( ------------- DATA STRUCTURES -----------------------------------------) 
12 SET DISS          ( A row of dea's representing a disassembly. )
: !DISS DISS !SET ;
: .DISS DISS @+ SWAP DO
    I @ DUP IS-COMMA IF I DISS - . THEN ID.
 0 CELL+ +LOOP CR ;
: +DISS DISS SET+! ;
: DISS? DISS SET? ;
: DISS- 0 CELL+ MINUS DISS +! ; ( Discard last item of `DISS' )

( ------------- TRYERS --------------------------------------------------) 
 
( These tryers are quite similar:                               
( if the DEA on the stack is of the right type and if the precondition  )
( is fullfilled it does the reassuring actions toward the tally as with )
( assembling and add the fixup/posti/commaer to the disassembly struct. )
( as if this instruction were assembled.                                )
( Leave the DEA.                                                        )
: TRY-PI
    DUP IS-PI IF
    AT-REST? IF
        DUP >BI TALLY:,
        DUP +DISS
    THEN
    THEN
;

: TRY-xFI
   DUP IS-xFI IF
   DUP >BI @ TALLY-BI @ CONTAINED-IN IF
       DUP >BI TALLY:|
       DUP +DISS
   THEN
   THEN
;
: TRY-xFIR
   DUP IS-xFIR IF     
   DUP >BI @ CORRECT-R TALLY-BI @ CONTAINED-IN IF
       DUP >BI TALLY:|R
       DUP +DISS
   THEN
   THEN
;
: TRY-COMMA
   DUP IS-COMMA IF
   DUP >BY @ TALLY-BY @ CONTAINED-IN IF
       DUP >BI TALLY:,, 
       DUP +DISS
   THEN
   THEN
;

( Generate bookkeeping such as to correspond with `DISS'.               )
: REBUILD              
    !TALLY
    DISS? IF
        DISS @+ SWAP !DISS DO  ( Get bounds before clearing)
            I @ TRY-PI TRY-xFI TRY-xFIR TRY-COMMA DROP
        0 CELL+ +LOOP
    THEN
;

( Discard the last item of the disassembly -- it is either used up or   )
( incorrect --. Replace DEA with the proper DEA to inspect from here.   )
: BACKTRACK
(   ." BACKTRACKING"                                                    )
    DROP DISS @ @- DISS ! 
(   DROP DISS @ 0 CELL+ - @                                             )
    >NEXT%
(   DISS-                                                               )
    REBUILD
;

( If the disassembly contains something: `AT-REST?' means               )
( we have gone full cycle rest->postits->fixups->commaers               )
( Return: the disassembly CONTAINS a result.                             )
: RESULT? AT-REST? DISS? AND   BAD? 0= AND ;

( If present, print a result and continue searching for a new last item )
: RESULT
    RESULT? IF
        .DISS
        DISS-
        REBUILD
    THEN
;

     % RESULT +DISS                                                      
( Try to expand the current instruction in `DISS' by looking whether    )
( DEA fits. Leave the NEXT dea.                                         )
: SHOW-STEP    
        TRY-PI TRY-xFI TRY-xFIR TRY-COMMA
        RESULT
        >NEXT%
(       DUP ID.                                                         )
        BAD? IF BACKTRACK THEN
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

( ------------- DISASSEMBLERS ------------------------------------------------) 
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
: DIS-PI
    DUP IS-PI IF
    AT-REST? IF
    DUP >BI @ INVERT OVER >CNT @ FIRSTBYTES POINTER @ @ AND OVER >DATA @ = IF
        DUP >BI TALLY:,
        DUP +DISS
        POINTER @ ISS !
        DUP >CNT @ POINTER +!   
    THEN
    THEN
    THEN
;
: DIS-xFI
   DUP IS-xFI IF
   DUP >BI @ TALLY-BI @ CONTAINED-IN IF
   DUP >BI @ INSTRUCTION AND   OVER >DATA @ = IF
   DUP >BA @  COMPATIBLE? IF
       DUP >BI TALLY:|
       DUP +DISS
   THEN
   THEN
   THEN
   THEN
;
: DIS-xFIR
   DUP IS-xFIR IF     
   DUP >BI @ CORRECT-R   TALLY-BI @ CONTAINED-IN IF
   DUP >BI @ CORRECT-R   INSTRUCTION AND   OVER >DATA @ CORRECT-R = IF
   DUP >BA @  COMPATIBLE? IF
       DUP >BI TALLY:|R
       DUP +DISS
   THEN
   THEN
   THEN
   THEN
;

: DIS-COMMA
   DUP IS-COMMA IF
   DUP >BY @ TALLY-BY @ CONTAINED-IN IF
   DUP >BA @  COMPATIBLE? IF
       DUP >BI TALLY:,,     
       DUP +DISS
   THEN
   THEN
   THEN
;

( Print the disassembly for the DEA , it must be a comma-er   )
: .COMMA
    POINTER @ @ OVER >CNT @ FIRSTBYTES U.
    DUP >CNT @ POINTER +!
    ID.
;
: .DISS' DISS @+ SWAP DO
    I @ DUP IS-COMMA 0= IF
        ID.
    ELSE DUP >CNT @ IF
       .COMMA       ( DEA -- )
    ELSE
       POINTER @ OVER >DIS @ EXECUTE POINTER !
    THEN
    THEN
 0 CELL+ +LOOP CR ;

( Dissassemble one instruction from `POINTER'. )
( Based on what is currently left in `TALLY!' )
( Leave `POINTER' pointing after that instruction. )
: ((DISASSEMBLE)) 
    POINTER @ >R
    STARTVOC BEGIN
        DIS-PI DIS-xFI DIS-xFIR DIS-COMMA
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

( As `((DISASSEMBLE}}' but starting with a clean slate.)
: (DISASSEMBLE) !DISS !TALLY ((DISASSEMBLE)) ;

( Forced dissassembly of one instruction from `POINTER'. )
( Force interpretation as DEA instruction. )
( This is useful for instructions otherwise hidden in the dictionary. )
: F-D  !DISS   !TALLY   DIS-PI DROP   ((DISASSEMBLE)) ;

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
: REJECT> NFA DUP >BI ISS @ @ AND SWAP >DATA @ = 27 ?ERROR ;

( ************************* )
' ASSEMBLER CFA ' ;CODE 4 CELLS + !        ( PATCH ;CODE IN NUCLEUS )
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !TALLY !CSP ; IMMEDIATE
: C; CURRENT @ CONTEXT ! ?EXEC CHECK26 CHECK32 SMUDGE ; IMMEDIATE
: LABEL ?EXEC 0 VARIABLE SMUDGE -2 ALLOT [COMPILE] ASSEMBLER
    !CSP ; IMMEDIATE     ASSEMBLER DEFINITIONS



