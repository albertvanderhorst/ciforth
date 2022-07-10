( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_tools.frt,v 5.37 2021/05/07 13:19:03 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.
\
\ This file contains the datastructures and tools.
\
INCLUDE tooltje

100 CONSTANT MAX-SIZE          \ For diverse buffers.

\ Append   bag  to the end of  bag2 .
: APPEND-BAG  SWAP  DO-BAG I @ OVER BAG+! LOOP-BAG  DROP ;
\ Copy   bag  to replace  bag2 .
: COPY-BAG DUP !BAG APPEND-BAG ;

REGRESS  next$ NIP S: 4

REGRESS 'DROP code$ NIP S: 1
REGRESS 'NOOP code$ NIP S: 0
REGRESS 'ROT code$ NIP S: 6

ASSEMBLER
\ -----------------------------------------------------------------------
\                      Bit sets
\ -----------------------------------------------------------------------

'VARIABLE ALIAS register-set
8 ENUM-MASK |AX| |CX| |DX| |BX| |SP| |BP| |SI| |DI|
|SP| |BP| |SI| OR OR CONSTANT reserved-registers  \ For ciforth.
\ Later add : |BP| 8 LSHIFT OR  for frame pointer.

\ Contains the registers that have a value throughout a
\ set of basic blocks that are optimised.
register-set permanent
: !permanent   reserved-registers permanent ! ;

\ For   #reg  return :"It  is  permanent."
: permanent?  1 SWAP LSHIFT permanent @ AND 0<> ;

\ Return next free register, for now from the ghost set.
\ Allocate it.
: get-permanent
    permanent @  17 8 DO DUP I RSHIFT 1 AND 0= IF I LEAVE THEN LOOP
    DUP 17 = IF DROP NONE ELSE 1 OVER LSHIFT permanent OR!U THEN NIP ;
REGRESS !permanent get-permanent get-permanent S: 8 9

\ CREATE BIT-MASK-TABLE 1   8 0 DO DUP C, 1 LSHIFT LOOP DROP
\ : BIT-MASK   CHARS BIT-MASK-TABLE + C@ ;
\
\ \ For BIT and BITSET return ADDRESS  and MASK.
\ : BIT-WHERE SWAP 8 /MOD SWAP BIT-MASK >R + R> ;
\
\ \ Set BIT in BITSET
\ : SET-BIT BIT-WHERE OVER C@ OR SWAP C! ;
\
\ \ Clear  BIT in BITSET
\ : CLEAR-BIT BIT-WHERE INVERT OVER C@ AND SWAP C! ;
\
\ \ For BIT in BITSET , return "it IS set".
\ : BIT? BIT-WHERE SWAP C@ AND 0= 0= ;
\
\ A register-used bitset contains
\  16 bits for regular registers used as input and one for the carry flag.
\  16 bits for regular registers used as output and one for the carry flag.
\  A CISC instruction like REPZ LODS can have as much as 3 register
\  as input and output as well as the carry flag.

\ From   addr  get the  register  (normal/"primed", from byte 2/3)
\ From   addr  get the   normal  register. Still without Q:
:I get-reg-N    C@ 7 AND ;
\ From   addr  get the  secondary  "primed" register. Still without Q:
:I get-reg-'    C@ 3 RSHIFT 7 AND ;
: get1-reg-QN   >R R@ C@ 1 AND 3 LSHIFT   R> 1 + get-reg-N OR ;
: get2-reg-QN   >R R@ C@ 1 AND 3 LSHIFT   R> 2 + get-reg-N OR ;
: get2-reg-Q'   >R R@ C@ 4 AND 1 LSHIFT   R> 2 + get-reg-' OR ;

:I getbit-QN C@ 1 AND IF 8 LSHIFT THEN ;
:I getbit-Q' C@ 2 AND IF 8 LSHIFT THEN ;
\ -msk- thingies work with bit-mask instead of reg nummber " 1 reg LSHIFT"
: get1-msk-QN   >R  1  R@ 1+ get-reg-N LSHIFT   R> getbit-QN ;
: get1-msk-Q'   >R  1  R@ 1+ get-reg-' LSHIFT   R> getbit-Q' ;

: get2-msk-QN   >R  1  R@ 2 + get-reg-N LSHIFT   R> getbit-QN ;
: get2-msk-Q'   >R  1  R@ 2 + get-reg-' LSHIFT   R> getbit-Q' ;

' get1-msk-QN ALIAS  get-mod-mask

\ For   addr1  addr2  give  mod-mask  rm-mask .
\ `addr1 points to q-prefix  `addr2 to the modrm byte of a SZ2.
\ : get-qmodrm   hold ;
\ For code at  addr  return  normal  secondary  registers for all
\ regular instruction (with 0,1,2 operands).
\ NONE is used to indicate a missing register.
: find-reg-RGL >R R@ Qp@
   DUP OP0 AND IF DROP NONE NONE
   ELSE DUP OP1 AND SZ2 AND IF DROP R@ get2-reg-QN NONE
   ELSE DUP   OP1 SZ1 OR   AND   OP1 SZ1 OR  = IF DROP  R@ get1-reg-QN NONE
   ELSE DUP   OP2 SZ2 OR   AND   OP2 SZ2 OR  = IF DROP
        R@ get2-reg-QN R@ get2-reg-Q'
   ELSE 3011 THROW
   THEN THEN THEN THEN
   RDROP ;
\ REGRESS HERE Q: MOV, X| T| DX'| R| AX|  find-reg-RGL S: 0 2
REGRESS HERE Q: MOV, X| F| DX'| R| AX|  find-reg-RGL S: 0 2
REGRESS HERE QN': MOV, X| F| DX'| R| AX|  find-reg-RGL S: 8 10
REGRESS HERE Q': MOV, X| F| DX'| ZO| [AX] find-reg-RGL S: 0 10
REGRESS HERE Q: MOV, X| F| AX'| ZO| [BX]   find-reg-RGL S: 3 0
REGRESS HERE Q: MOVI|X, BX| 1234 IL, find-reg-RGL S: 3 NONE
REGRESS  HEX HERE Q: PUSH|X, SI|  get-mod-mask S: 0000,0040
REGRESS  HEX HERE QN: PUSH|X, SI|  get-mod-mask S: 0000,4000

\ For  regN  reg'  instr  fill in the r/m byte at instr.
: put-regs  >R   2DUP 7 AND 3 LSHIFT SWAP 7 AND OR  R@ 2 + OR!U
    3 RSHIFT 2 LSHIFT   SWAP 3 RSHIFT OR   R> OR!U ;
REGRESS HERE Q: XCHG, X| R| AX| AX'|  >R 15 15 R@ put-regs R> DUP 10 + D-R S:

\ For  regN  instr  fill in the r/m byte at instr of 2 bytes.
: put2-reg  >R   DUP 3 RSHIFT R@ OR!U  7 AND R> 2 + OR!U ;
\ For  regN  instr  fill in the r/m byte at instr of 1 bytes.
: put1-reg-QN  >R   DUP 3 RSHIFT R@ OR!U  7 AND R> 1 + OR!U ;
\ For  reg'  instr  fill in the r/m byte at instr of 2 bytes.
: put2-reg-Q'  >R   DUP 3 RSHIFT 2 RSHIFT R@ OR!U  7 AND R> 2 + OR!U ;
\ 'put1-reg ALIAS put1-reg-QN
'put2-reg ALIAS put2-reg-QN
: put2-reg-QN' >R
   OVER 3 RSHIFT 2 LSHIFT  OVER 3 RSHIFT OR    R@ OR!U
   OVER 7 AND 3 LSHIFT     OVER 7 AND OR       R> 2 + OR!U
   2DROP ;

\ Collects the offsets of instruction in the pattern to be matched,
\ which is the same as in the code matched.
10 BAG ins-offsets     HERE CONSTANT reg-end

\ Remember that at `HERE an instruction starts.
: remember-start ins-offsets @ reg-end = 3018 ?ERROR HERE ins-offsets BAG+! ;
REGRESS ins-offsets !BAG HERE remember-start ins-offsets CELL+ @ = S: TRUE

\ Replace addresses with relative addresses.
: change-to-rel-addr ins-offsets CELL+ @ NEGATE
    ins-offsets DO-BAG DUP I +! LOOP-BAG DROP ;
REGRESS remember-start change-to-rel-addr ins-offsets DUP $@ - SWAP CELL+ @ S: -2 CELLS 0

\ A row of dea's representing the disassembly of one instruction, including
\ instruction prefixes.
20 BAG PDISS
: !PDISS PDISS !BAG ;   !PDISS

\ The last as-element analysed: It  was  a prefix. A flag but not proper.
: WAS-PREFIX? LATEST-INSTRUCTION @ >PRF @ ;
: SILENT 2DROP ;
: ~~DISASSEMBLE
     'SILENT 'TYPE 2 CELLS MOVE
          WAS-PREFIX? >R
          (DISASSEMBLE) DISS PDISS
          R> IF APPEND-BAG ELSE COPY-BAG THEN
          !DISS
     'TYPE RESTORED ;
: ~DISASSEMBLE BEGIN ~~DISASSEMBLE WAS-PREFIX? NOT? UNTIL ;
REGRESS '+ >CFA @ DUP ~DISASSEMBLE - S: -1

\ While assembling `TALLY-BI bytes are collected in `valid-mask$ . The
\ later operation is to compare this to actual code by XOR, ignoring
\ bits outside of the mask.
DATA valid-mask$ MAX-SIZE ALLOT
\ To collect the actual code. String and straight.
DATA bufc$ MAX-SIZE ALLOT      bufc$ CELL+ CONSTANT bufc
\ To collect non-matched stuff, variant data.
DATA bufv$ MAX-SIZE ALLOT      bufv$ CELL+ CONSTANT bufv
: <!  !CSP 0 valid-mask$ ! ALIGN HERE DUP   ins-offsets !BAG remember-start ;
\ From  there  to `HERE is marked as must-match according to the disassembler.
\ Only the bits up need to match. There can be holes, hence the tally reset.
: !!T   >R TALLY-BI @ INVERT HERE R> ?DO
        DUP valid-mask$ $C+ 8 RSHIFT
    LOOP DROP HERE !TALLY ;
\ From  there  to `HERE is marked as no-need-to-match.
: ~!!T   HERE SWAP ?DO  0 valid-mask$ $C+ LOOP HERE !TALLY ;
HEX  ASSEMBLER GET-CURRENT (pifulist) @
DEFINITIONS  \ See asi386.frt
\ Define a don't care operation which matches ADD, .. CMP,
0004,1400 0000 FF03 38 OR 0000 2PI XXX,
\ Define a don't care operation which matches ADDI, .. CMPI,
0400 04 C701 3800 OR 0080  2PI XXXI,
\ Add a prefix indicating 64bit but otherwise don't care.
8000,0000 0 7 48 1PI QX:
8000,0000 0 7 40 1PI EX:
\ The 64 bit MOVI|X, was conveniently short. MOVI|L, replaces it.
: MOVI|L,  MOVI, X| R| ;

(pifulist) !  SET-CURRENT
\ Do not link them in, disassembly will fail!
DECIMAL PREVIOUS \ DEFINITIONS

\ Create a constant mask pertaining to the code analysed.
: !> remember-start DROP valid-mask$ $@ $,  change-to-rel-addr ;
\ Must be used in pairs <A A>. Leave   start-addres .
\ Create an exexutable anonymous assembler code snippet.
\ Within <A A> it makes no sense to use !!T ~!!T !
\ The code contains holes that must be filled, resulting in
\ controlled changes. That requires !TALLY or the code
\ would not compile.
: <A ALIGN HERE ?EXEC   ASSEMBLER   !TALLY   !CSP ;
: A> NEXT ?CSP   ?EXEC   CHECK26   CHECK32   PREVIOUS ;

\ Compare pointers to   code  pattern  where only bits of  mask  count.
\ The code may not extend beyond `stopper. Then there is no match.
\ The mask is a string variable. Return:"It  is  a match."
\ Also collect the variant part into  `bufv$
     VARIABLE stopper
: matched   0 bufv$ !
    $@ BOUNDS DO
        \ Somewhat tricky code. `pattern is also used as a flag.
        OVER stopper @ = IF 0 AND LEAVE THEN
        SWAP C@+ ROT C@+ ROT XOR   DUP bufv$ $C+
        I C@ AND   IF 0 AND LEAVE THEN
    LOOP  0<> NIP ;
REGRESS DSP@ stopper ! S:


\ ------------- class optimisation -----------------------------------------
\ This later has to replaced by manipulation of the register bits.
\ !Q marks a place where an instruction starts, not just the Q prefix.
\ The optimiser embellies this with all kind of stuff, not all realised yet.
: !Q! remember-start Q: !!T ;
: !Q  remember-start QX: !!T ;
\ : !E! remember-start E: !!T ;
\ : !E  remember-start EX: !!T ;

\ Note `replace leaves the result in `bufc$
<! !Q! LEA, BP'| BO| [BP]  !!T   $00 C, ~!!T !>  \ replacee and its mask
<A Q: LEA, BP'| XO| [BP]  0 L, NEXT A>                \ replacement
 {   bufv 3 + C@   C>S bufc 3 + L! }                  \ patching code
class optimisation >R >R
    M: opt-debug BODY> >NFA @ $@ ETYPE ^J DSP@ 1 ETYPE DROP M;
\ Fill in the offsets that are found during analysis of the <! !> stuff.
    M: first!  CELL+ ! M;
M: starts M;   HERE 11 CELLS ALLOT   ins-offsets SWAP COPY-BAG
    M: matches?  OVER first! 2@ ( org  mask ) matched M;
M: mask$ @ M; ,
M: original @ M; ,   \ Just a pointer
M: replacement$ @ find-next M;  R> ,     \ Code as a string.
   M: replace  opt-debug replacement$ bufc$ $! @ EXECUTE M;
M: correction @ M; R> ,
endclass
: DUMPO  mask$ $@ DUP >R DUMP
         original R> .code  CR
         replacement$ .code CR CR
         starts .BAG
         correction  crack-hl ;
REGRESS <! !Q! PUSH|X, !!T  !> <A Q: PUSH|X, AX| A> { } optimisation tmp S:
REGRESS tmp DUMPO starts 3 CELLS + @ S: 2

\ Isolate first instruction of matchee as a  string  .
: ins1$ starts 1 []  starts 3 [] ;

: ins1   starts 1 []   starts 2 [] + ;
: find-reg1  ins1 find-reg-RGL ;
: ins2   starts 1 []   starts 3 [] + ;
: find-reg2  ins2 find-reg-RGL ;
: ins3   starts 1 []   starts 4 [] + ;
: find-reg3  ins3 find-reg-RGL ;
: ins4   starts 1 []   starts 5 [] + ;
REGRESS <! !Q PUSH|X, !!T  !Q POP|X, !!T  !> <A A> { } optimisation tmp S:
REGRESS HERE Q: PUSH|X, BX| QN: POP|X, DI| matches? find-reg1 find-reg2 S: TRUE 3 NONE 15 NONE

\ ----------------------------------------------------------------
\ The following data is pertinent:
\ The offset in cells, # read accesses, # write accesses,
\ permanent register (0..16 where >8 are the ghost R@ registers)
\ A packed thing could have 4 16 bits fields.
20 BAG permanent-properties  \ of packed data
: (unpack) >R R@ $FFFF AND R> 16 RSHIFT ;
: unpack  (unpack)  (unpack)  (unpack)  ;
: (pack) 16 LSHIFT OR ;
: pack (pack) (pack) (pack) ;
REGRESS 1 2 3 4 pack unpack S: 1 2 3 4

\ For  cell-count   return   addr  of packed item.
: permanent-WHERE?    NULL permanent-properties DO-BAG
        OVER I @ unpack DROP DROP DROP = IF DROP I LEAVE THEN
    LOOP-BAG NIP ;
REGRESS permanent-properties DUP !BAG 7 1 2 3 pack SWAP BAG+! S:
REGRESS 7 permanent-WHERE? @ unpack S: 7 1 2 3
REGRESS 12 permanent-WHERE? S: NULL

\ For   celloffset   add 1 to the read count, maybe allocate.
    : add-read  DUP permanent-WHERE? NULL = IF
       get-permanent 0 1 pack  permanent-properties BAG+! ELSE
     permanent-WHERE? DUP @ unpack 1+ pack SWAP ! THEN ;
\ For   celloffset   add 1 to the write count ;
    : add-write  DUP permanent-WHERE? NULL = IF
       get-permanent 1 0 pack  permanent-properties BAG+! ELSE
     permanent-WHERE? DUP @ unpack SWAP 1+ SWAP pack SWAP ! THEN ;
REGRESS 0 permanent-WHERE? S: NULL
REGRESS 7 add-read 7 permanent-WHERE? @ unpack S: 7 1 2 4
REGRESS 7 add-write 7 permanent-WHERE? @ unpack S: 7 1 3 4

\ The number of times BP was adjusted.
VARIABLE RPO-LEA's

DATA use-BP   Q: MOV, X| F| BX'| XO| [BP] 0 L,  NOPS, NOPS,
DATA use-AXg  Q: MOV, X| F| BX'| R| AX| NOPS, NOPS, NOPS, NOPS, NOPS,
?64 use-BP @ use-AXg  @ XOR  CONSTANT BP-AXg

\ MAX-SIZE BAG permanent-registers
\ \ Remember a  register  object, that it is permanent.
\ : make-permanent   r># permanent-registers BAG+! ;
\ : make-ghost       r># 8 + permanent-registers BAG+! ;

: init-RSP   !permanent   permanent-properties !BAG 0 RPO-LEA's ! ;
