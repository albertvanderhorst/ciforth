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
(   3. data, as a bit field in the instruction.                         )
(   4. data, including addresses or offsets.                            )
( This assembler goes through three stages for each instruction:        )
(   1. postit: assemblers the opcode with holes for the modifiers.      )
(      This has a fixed length. Also posts requirements for commaers.   )
(   2. fixup: fill up the holes, either from the beginning or the       )
(     end of the post. These can also post required commaers            )
(   3. fixup's with data. It has user supplied data in addition to      )
(      opcode bits. Both together fill up bits left by a postit.        )
(   4. The commaers. Any user supplied data in addition to              )
(      opcode, that can be added as separate bytes. Each has a          )
(      separate command, where checks are built in.                     )
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
REQUIRE @+ ( Fetch from ADDRES. Leave incremented ADDRESS and DATA )
: !+ >R R@ ! R> CELL+ ; ( Store DATA to ADDRES. Leave incremented ADDRESS)
( Fetch from decremented ADDRES. Leave DATA and ADDRESS)
: @- 0 CELL+ - >R R@ @ R>  ;
( CHAR - CONSTANT &-     CHAR ~ CONSTANT &~                             )
CREATE TABLE 1 , 1 , ( x TABLE + @ yields $100^[-x mod 4] )
( Rotate X by I bytes left leaving X' Left i.e. such as it appears in )
( memory! Not as printed on a big endian machine! )
: ROTLEFT TABLE + @ UM* OR ;   ( aqa " 8 * LSHIFT" on bigendian. )
'TABLE HIDDEN

( ------------- UTILITIES, SYSTEM DEPENDANT ----------------------------)
VOCABULARY ASSEMBLER IMMEDIATE   ASSEMBLER DEFINITIONS HEX
( We use the abstraction of a dea "dictionary entry address". aqa "xt" )
( Return the DEA from "word". 1]                                         )
DENOTATION : % POSTPONE ' ; PREVIOUS
: %ID. ID. ;   ( Print a definitions name from its DEA.)
: %>BODY >CFA >BODY ; ( From DEA to the DATA field of a created word )
: %BODY> BODY> CFA> ; ( Reverse of above)
: %>DOES >DFA @ ; ( From DEA to the DOES> pointer for a ``DOES>'' word )
( Leave for DEA : it IS to be ignored in disassemblies. This is used    )
( for supressing the bare bones of the sib mechanism in i586.           )
: IGNORE? >NFA @ CELL+ C@ &~ = ;

: (>NEXT%) >LFA @ ; ( Given a DEA, return the next DEA. )
( For a DEA as returned from (>NEXT%} : it IS the end, not a real dea.  )
: VOCEND? >LFA @ 0= ;
( As (>NEXT%} but skip holes, i.e. words with names starting in ``-''   )
: >NEXT% BEGIN  (>NEXT%) DUP >NFA @ CELL+ C@ &- - UNTIL ;
( Leave the first DEA of the assembler vocabulary.                      )
: STARTVOC ['] ASSEMBLER >WID >LFA @ ;

( Build: allocate place to remember a DOES> address of a `CREATE'd word )
( Leave that ADDRESS  to be filled in by ``REMEMBER''                   )
( Execution: Leave for DEA : it IS of same type as the remembered DOES> )
: IS-A CREATE HERE 1 CELLS ALLOT DOES> @ SWAP %>DOES @ = ;
( Patch up the data field of a preceeding word defined by `IS-A'        )
( To be called when sitting at the DOES> address                        )
( The DSP@ / ?CSP detects stack changes. Now split it into 2 checks.    )
: REMEMBER ?CSP HERE SWAP ! DSP@ ; IMMEDIATE

( Also needed : ?ERROR                                                  )
(   `` : ?ERROR DROP DROP ; '' defeats all checks.                      )

( Behaves as ``CREATE'' except, if the word to be created has name "--" )
( it is ignored, by making the header unfindable. Not strictly needed.  )
REQUIRE POSTFIX
: CREATE--   (WORD) 2DUP POSTFIX CREATE
    2 = SWAP "--" CORA 0= AND IF LATEST HIDDEN THEN ;

( ------------- UTILITIES, SYSTEM INDEPENDANT ------------------------- )
(   The FIRST bitset is contained in the SECOND one, leaving it IS      )
: CONTAINED-IN OVER AND = ;
CREATE TABLE 0 , FF , FFFF , FFFFFF , FFFFFFFF ,
( From a MASK leave only SOME first bytes up, return IT                 )
( First means lower in memory, this relies on big endian.               )
: FIRSTBYTES CELLS TABLE + @ AND ;

( ------------- ASSEMBLER, BOOKKEEPING -------------------------------- )
( The bookkeeping is needed for error detection and disassembly.        )
VARIABLE TALLY-BI  ( Bits that needs fixed up)
VARIABLE TALLY-BY  ( Bits represent a commaer that is to be supplied)
VARIABLE TALLY-BA  ( State bits, bad if two consequitive bits are up)
VARIABLE BA-DEFAULT    0 BA-DEFAULT ! ( Default not implemented. )
VARIABLE OLDCOMMA ( Previous comma, or zero)
VARIABLE ISS  ( Start of current instruction)
VARIABLE ISL  ( Lenghth of current instruction)

( Initialise ``TALLY''                                                  )
: !TALLY   0 TALLY-BI !   0 TALLY-BY !   BA-DEFAULT @ TALLY-BA !  0 OLDCOMMA ! ;
( Return: instruction IS complete, or not started)
: AT-REST? TALLY-BI @ 0=   TALLY-BY @ 0=  AND ;
( For N : it CONTAINS bad pairs)
: BADPAIRS? DUP 1 LSHIFT AND AAAAAAAAAAAAAAAA AND ;
: BAD? TALLY-BA @ BADPAIRS? ;  ( The state of assembling IS inconsistent)
( If STATUS were added to `TALLY-BA' would that CREATE a bad situation? )
: COMPATIBLE? TALLY-BA @ OR BADPAIRS? 0= ;
DECIMAL
( Generate errors. None have net stack effects, such that they may be   )
( replaced by NULL definitions.                                         )
: CHECK26 AT-REST? 0= 26 ?ERROR ;  ( Error at postit time )
: CHECK32 BAD? 32 ?ERROR ; ( Always an error )
( Generate error for fixup, if for the BI, some of the BITS would       )
( stick out it. Leave MASK and BITS . Programming error!                )
: CHECK31 2DUP SWAP CONTAINED-IN 0= 31 ?ERROR ;
( Generate error for ``FIXUP-DATA'' , if the BI and the LEN             )
( are not compatible. Leave BI and LEN . Programming error!             )
: CHECK31A 2DUP OVER >R RSHIFT 1 OR OVER LSHIFT R> <> 31 ?ERROR ;
( Generate error for postit, if for the inverted BI , some of the the   )
( BITS would stick out it. Leave MASK and BITS . Programming error!     )
: CHECK33 2DUP SWAP INVERT CONTAINED-IN 0= 31 ?ERROR ;
( BITS would stick out it. Leave MASK and BITS . Programming error!     )
( Generate error on data for postit/fixup, if some BITS to fill in      )
( are already in the MASK. Leave BITS and MASK.                         )
: CHECK28 2DUP AND 28 ?ERROR ;
( Generate error on data for commaer, if the BITS to reset are not      )
( present in the MASK. Leave BITS and MASK.                             )
: CHECK29 2DUP OR -1 - 29 ?ERROR   ;
( Generate error if COMMAMASK is not in ascending order. Leave IT.      )
: CHECK30 DUP OLDCOMMA @ < 30 ?ERROR DUP OLDCOMMA ! ;
HEX
( Or DATA into ADDRESS. If bits were already up its wrong.)
: OR! >R R@ @  CHECK28 OR R> ! ;
: OR!U >R R@ @  OR R> ! ; ( Or DATA into ADDRESS. Unchecked.)
( Reset bits of DATA into ADDRESS. If bits were already down it's wrong )
: AND! >R INVERT R@ @ CHECK29 AND R> ! ;

( ------------- ASSEMBLER, DEFINING WORDS    ----------------------------)

( Common fields in the defining words for posits fixups and commaers.   )
( All leave a single ADDRESS.                                           )

( The first data field for a postit/fixup contains instruction bits,    )
( for a commaer it contains the xt of the coma action                   )
( for a data fixup it contains the position of the bits                 )
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
: !POSTIT  HERE ISS !  0 OLDCOMMA ! ;  ( Initialise in behalf of postit )
( Bookkeeping for a postit using a pointer to the BIBYBA )
( information, can fake a postit in disassembling too                   )
: TALLY:,   @+ TALLY-BI !  @+ TALLY-BY !   @+ TALLY-BA OR!U   @ ISL ! ;
( Post the instruction using DATA. )
: POSTIT   CHECK26   !TALLY !POSTIT  HERE ISS !
    @+ ,   TALLY:,   CORRECT,- ;
( Define an instruction by BA BY BI and the OPCODE                      )
( For 1 2 3 and 4 byte opcodes.                                         )
IS-A IS-1PI : 1PI  CHECK33 CREATE-- , , , , 1 , DOES> REMEMBER POSTIT ;
IS-A IS-2PI : 2PI  CHECK33 CREATE-- , , , , 2 , DOES> REMEMBER POSTIT ;
IS-A IS-3PI : 3PI  CHECK33 CREATE-- , , , , 3 , DOES> REMEMBER POSTIT ;
IS-A IS-4PI : 4PI  CHECK33 CREATE-- , , , , 4 , DOES> REMEMBER POSTIT ;
( For DEA : it REPRESENTS some kind of opcode.                          )
: IS-PI  >R 0
    R@ IS-1PI OR  R@ IS-2PI OR  R@ IS-3PI OR   R@ IS-4PI OR
R> DROP ;

( Bookkeeping for a fixup using a pointer to the BIBYBA information,    )
( can fake a fixup in disassembling too.                                )
: TALLY:|   @+ TALLY-BI AND!   @+ TALLY-BY OR!   @ TALLY-BA OR!U ;
( Fix up the instruction using a pointer to DATA. )
: FIXUP>   @+ ISS @ OR!   TALLY:|   CHECK32 ;
( Define a fixup by BA BY BI and the FIXUP bits )
( One size fits all, because of the or character of the operations. )
IS-A IS-xFI   : xFI   CHECK31 CREATE-- , , , , DOES> REMEMBER FIXUP> ;

( Fix up the instruction using DATA and a pointer to the bit POSITION. )
: FIXUP-DATA @+ ROT SWAP LSHIFT ISS @ OR! TALLY:| CHECK32 ;
( Define a data fixup by BA BY BI, and LEN the bit position.            )
( At assembly time: expect DATA that is shifted before use              )
( One size fits all, because of the or character of the operations.     )
IS-A IS-DFI  : DFI   CHECK31A CREATE-- , , , , DOES> REMEMBER FIXUP-DATA ;

( *************** OBSOLESCENT ***********************************       )
\ Reverses bytes in a WORD. Return IT.
: REVERSE-BYTES     1 CELLS 0 DO DUP  FF AND SWAP 8 RSHIFT   LOOP
                    8 CELLS 0 DO SWAP I LSHIFT OR       8 +LOOP ;

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
( bi and fixup are specified that last byte is lsb, such as you read it )
IS-A IS-FIR   : FIR   CHECK31 CREATE-- REVERSE-BYTES , REVERSE-BYTES , , ,
    DOES> REMEMBER FIXUP< ;

( *************** PREFERRED NOT YET USED ************************       )
( If bits were already down it is wrong. For next two words.)
( Reset bits of DATA into ADDRESS bytewise. )
: (AND!BYTE) >R 0FF AND INVERT R@ C@ CHECK29 AND R> C! ;
( Reset bits of DATA byte by byte into ADDRESS )
: AND!BYTE BEGIN 2DUP (AND!BYTE) SWAP 8 RSHIFT DUP WHILE SWAP 1+ REPEAT 2DROP ;
( If bits were already up its wrong. for next two words.)
( Or DATA into ADDRESS bytewise. )
: (OR!BYTE) >R R@ C@  CHECK28 OR R> C! ;
( Or DATA byte by byte from behind into ADDRESS )
: OR!BYTE BEGIN 1- 2DUP (OR!BYTE) SWAP 8 RSHIFT DUP WHILE SWAP REPEAT 2DROP ;
( Bookkeeping for a fixup-from-reverse using a pointer to the BIBYBA    )
( information, can fake a fixup in disassembling too.                   )
: TALLY:|R'  @+ TALLY-BI AND!BYTE   @+ TALLY-BY OR!   @ TALLY-BA OR!U ;
( Fix up the instruction from reverse using a pointer to DATA. )
: FIXUP<'   @+ ISS @ ISL @ + OR!BYTE   TALLY:|R'  CHECK32 ;

( *************** END PREFERRED ****************************       )

( Bookkeeping for a commaer using a pointer to the BIBYBA information.  )
( Not used by the disassembler.                                         )
: TALLY:,, CELL+   @+ CHECK30 TALLY-BY AND!   @ TALLY-BA OR!U ;
: COMMA @+ >R  TALLY:,,  CHECK32   R> EXECUTE ;
( Build with an disassembly ROUTINE, with the LENGTH to comma, the BA   )
( BY information and the ADDRESS that is executing the commaer          )
IS-A  IS-COMMA   : COMMAER CREATE  , 0 , , , , , DOES> REMEMBER COMMA ;

( ------------- ASSEMBLER, SUPER DEFINING WORDS ----------------------)

CREATE PRO-TALLY 3 CELLS ALLOT  ( Prototype for TALLY-BI BY BA )
: T! PRO-TALLY !+ !+ !+ DROP ;
( Fill in the tally prototype with BA BY, reversed BI information )
: T!R   REVERSE-BYTES T! ;
( Get the data from the tally prototype back BA BY BI )
: T@ PRO-TALLY 3 CELLS +  @- @- @- DROP ;
( Add INCREMENT to the OPCODE a NUMBER of times, and generate as much   )
( instructions, all with the same BI-BA-BY from ``PRO-TALLY''           )
( For each assembler defining word there is a corresponding family word )
( Words named "--" are mere placeholders. )
: 1FAMILY,    0 DO   DUP >R T@ R> 1PI   OVER + LOOP DROP DROP ;
: 2FAMILY,    0 DO   DUP >R T@ R> 2PI   OVER + LOOP DROP DROP ;
: 3FAMILY,    0 DO   DUP >R T@ R> 3PI   OVER + LOOP DROP DROP ;
: 4FAMILY,    0 DO   DUP >R T@ R> 4PI   OVER + LOOP DROP DROP ;
: xFAMILY|    0 DO   DUP >R T@ R> xFI   OVER + LOOP DROP DROP ;
: FAMILY|R    0 DO   DUP >R T@ REVERSE-BYTES  R> FIR   OVER + LOOP DROP DROP ;
: xFAMILY|F   0 DO   DUP >R T@ R> DFI   OVER + LOOP DROP DROP ;

( ############### PART II DISASSEMBLER #################################### )

( Tryers try to construct an instruction from current bookkeeping.      )
( They can backtrack to show all possibilities.                         )
( Disassemblers try to reconstruct an instruction from current          )
( bookkeeping. They are similar but disassemblers take one more aspect  )
( into account, a piece of actual code. They do not backtrack but fail. )

( ------------- SYSTEM INDEPENDANT UTILITIES ----------------------------)
( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP DO I . 0 CELL+ +LOOP ;   ( Print non-empty SET )

( ------------- DATA STRUCTURES -----------------------------------------)
12 SET DISS          ( A row of dea's representing a disassembly. )
: !DISS DISS !SET ;
: .DISS-AUX DISS @+ SWAP DO
    I @ DUP IS-COMMA OVER IS-DFI OR IF I DISS - . THEN ID.
 0 CELL+ +LOOP CR ;
( DISS-VECTOR can be redefined to generate testsets)
VARIABLE DISS-VECTOR    ['] .DISS-AUX DISS-VECTOR !
: +DISS DISS SET+! ;
: DISS? DISS SET? ;
: DISS- 0 CELL+ NEGATE DISS +! ; ( Discard last item of `DISS' )

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
: TRY-DFI
   DUP IS-DFI IF
   DUP >BI @ TALLY-BI @ CONTAINED-IN IF
       DUP >BI TALLY:|
       DUP +DISS
   THEN
   THEN
;
: TRY-FIR
   DUP IS-FIR IF
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
            I @ TRY-PI TRY-xFI TRY-DFI TRY-FIR TRY-COMMA DROP
        0 CELL+ +LOOP
    THEN
;

( Discard the last item of the disassembly -- it is either used up or   )
( incorrect --. Replace DEA with the proper DEA to inspect from here.   )
: BACKTRACK
(   ." BACKTRACKING"                                                    )
    DROP DISS @ @- DISS !
(   DROP DISS @ 0 CELL+ - @                                             )
(   "Failed at :" TYPE DUP ID. CR                                       )
    >NEXT%
(   DISS-                                                               )
    REBUILD
;

( If the disassembly contains something: `AT-REST?' means               )
( we have gone full cycle rest->postits->fixups->commaers               )
( Return: the disassembly CONTAINS a result.                             )
: RESULT? AT-REST? DISS? AND   BAD? 0= AND ;

( If present, print a result and continue searching for a new last item )
: .RESULT
    RESULT? IF
        DISS-VECTOR @ EXECUTE
        DISS-
        REBUILD
    THEN
;

\     % RESULT +DISS Spurious? Remove after next total test.
( Try to expand the current instruction in `DISS' by looking whether    )
( DEA fits. Leave the NEXT dea.                                         )
: SHOW-STEP
        TRY-PI TRY-DFI TRY-xFI TRY-FIR TRY-COMMA
        .RESULT
        >NEXT%
(       DUP ID.                                                         )
        BAD? IF BACKTRACK THEN
        BEGIN DUP VOCEND? DISS? AND WHILE BACKTRACK REPEAT
;

( Show all the instructions present in the assembler vocabulary )
: SHOW-ALL
    !DISS   !TALLY
    STARTVOC BEGIN
       SHOW-STEP
    DUP VOCEND? UNTIL DROP
;

( Show all the opcodes present in the assembler vocabulary )
: SHOW-OPCODES
    !DISS   !TALLY
    STARTVOC BEGIN
       DUP IS-PI IF DUP %ID. THEN >NEXT%
    DUP VOCEND? UNTIL DROP
;

( Show at least all instructions valid for the "OPCODE" given. )
: SHOW:
    !DISS   !TALLY
    % DUP BEGIN
        SHOW-STEP
     OVER DISS CELL+ @ - OVER VOCEND? OR UNTIL DROP DROP
;

( ------------- DISASSEMBLERS ------------------------------------------------)

( Contains the position that is being disassembled                      )
VARIABLE POINTER       HERE POINTER !

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
: DIS-DFI
   DUP IS-DFI IF
   DUP >BI @ TALLY-BI @ CONTAINED-IN IF
   DUP >BA @  COMPATIBLE? IF
       DUP >BI TALLY:|
       DUP +DISS
   THEN
   THEN
   THEN
;
: DIS-FIR
   DUP IS-FIR IF
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

( Print a disassembly for the data-fixup DEA.                           )
: .DFI
    INSTRUCTION   OVER >BI @ AND   OVER >DATA @ RSHIFT   U.
    %ID.                         ( DEA -- )
;

( Print a standard disassembly for the commaer DEA.                     )
: .COMMA-STANDARD
    POINTER @ @ OVER >CNT @ FIRSTBYTES U.
    DUP >CNT @ POINTER +!
    %ID.                         ( DEA -- )
;

( Print the disassembly for the commaer DEA, advancing `POINTER' past   )
( the comma-content                                                     )
: .COMMA   DUP >CNT @ IF   .COMMA-STANDARD   ELSE   >DIS @ EXECUTE   THEN ;

( Print the DEA but with suppression, i.e. ignore those starting in '~' )
: %~ID. DUP IGNORE? IF DROP ELSE %ID. THEN  ;

( Print the disassembly `DISS'                                          )
: .DISS   DISS @+ SWAP DO
    I @
    DUP IS-COMMA IF
        .COMMA
    ELSE DUP IS-DFI IF
        .DFI
    ELSE
        %~ID.
    THEN THEN
 0 CELL+ +LOOP
;

VARIABLE I-ALIGNMENT    1 I-ALIGNMENT !   ( Instruction alignment )

( From POINTER show memory because the code there can't be              )
( disassembled. Leave incremented POINTER.                              )
: SHOW-MEMORY  BEGIN COUNT . ."  C, " DUP I-ALIGNMENT @ MOD WHILE REPEAT ;

( Dissassemble one instruction from POINTER starting at DEA. )
( Based on what is currently left in `TALLY!' )
( Leave a POINTER pointing after that instruction. )
: ((DISASSEMBLE))
    SWAP
    >R R@ POINTER !
    ( startdea -- ) BEGIN
        DIS-PI DIS-xFI DIS-DFI DIS-FIR DIS-COMMA
        >NEXT%
(       DUP ID. ." : "  DISS-VECTOR @ EXECUTE                                 )
    DUP VOCEND? RESULT? OR UNTIL DROP
    RESULT? IF
      .DISS     \ Advances pointer past commaers
      RDROP POINTER @
    ELSE
      R> SHOW-MEMORY
    THEN
;

( Dissassemble one instruction from ADDRESS using the whole instruction set )
( and starting with a clean slate. )
( Leave an ADDRESS pointing after that instruction.                     )
: (DISASSEMBLE)   !DISS !TALLY STARTVOC ((DISASSEMBLE)) ;

( Forced dissassembly of one instruction from `POINTER'. )
( Force interpretation as DEA instruction. )
( This is useful for instructions that are known or hidden by an other  )
( instruction that is found first.                             )
: FORCED-DISASSEMBLY
    !DISS   !TALLY   POINTER @ SWAP ((DISASSEMBLE)) DROP ;

( Dissassemble one instruction from address ONE to address TWO. )
: DISASSEMBLE-RANGE
    SWAP   BEGIN (DISASSEMBLE) CR 2DUP > 0= UNTIL   2DROP
;
(   : M| ['] xxx  REJECT M| ;  To forbid M| xxx  in combination      )
( xxx must be PI or FI not FIR )
: REJECT> DUP >BI ISS @ @ AND SWAP >DATA @ = 27 ?ERROR ;

( ********************* DEFINING WORDS FRAMEWORK ********************** )
( Close an assembly definition: restore and check.)
: END-CODE
    ?CSP ?EXEC CHECK26 CHECK32 PREVIOUS
; IMMEDIATE

( FIXME : we must get rid of this one )
: ;C POSTPONE END-CODE "WARNING: get rid of C;" TYPE CR ; IMMEDIATE

\ The following two definitions must *NOT* be in the assembler wordlist.
PREVIOUS DEFINITIONS DECIMAL

ASSEMBLER
( Define "word" using assembly instructions up till END-CODE )
( One could put a ``SMUDGE'' in both. )
: CODE
    ?EXEC (WORD) (CREATE) POSTPONE ASSEMBLER !TALLY DSP@
; IMMEDIATE

( Like ``DOES>'' but assembly code follows, closed by END-CODE )
: ;CODE
    ?CSP   POSTPONE (;CODE)   POSTPONE [   POSTPONE ASSEMBLER
; IMMEDIATE

( ************************* CONVENIENCES ****************************** )

( Abbreviations for interactive use. In the current dictionary. )
    : DDD (DISASSEMBLE) ;
    : D-R DISASSEMBLE-RANGE ;

( *********************************** NOTES *************************** )
( 1. A DEA is an address that allows to get at header data like flags   )
(     and names. In ciforth an xt will do.                              )

PREVIOUS
