( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optimiser.frt,v 5.67 2021/05/06 14:15:48 albert Exp $)

\ This code optimises ciforth Forth code.
\ This file must be included by a Forth that has an analyser built in,
\ say lina-ana such that the stack effect bytes and the optimisation properties
\ have been filled in in the flag fields.
\ A bblock is a basic block in the sense of the dragon book, a piece of code
\ containing no jumps inside and entered from its start.

\ ERROR conditions identified, not documented with general ciforth
\ See `ERROR-LIST
\ --------------- tooltje ----------------------------
ASSEMBLER

\ NOTE: `LATEST-INSTRUCTION points to the latest instruction, i.e. after
\ a possible prefix.
( Dissassemble one instruction from ADDRESS using the whole instruction set )
( and starting with a clean slate. Prefixes are part of the instruction.)
( Leave an ADDRESS pointing after that instruction.                     )
\ : (DISASSEMBLE)   !DISS !TALLY pifustart ((DISASSEMBLE)) ;
\ : (DISASSEMBLE)'  !DISS     'TYPE RESTORED
\     !TALLY pifustart  ((DISASSEMBLE))
\ && EMIT .DISS && EMIT LATEST-INSTRUCTION @ >PRF @ H. CR ;

\ ' (DISASSEMBLE)' >DFA @ ' (DISASSEMBLE) >DFA !

DATA ^next  NEXT    HERE ^next - CONSTANT /NEXT
\ Test for those in optimisebblock.frt
: next$  ^next /NEXT ;
\ For  dea  return its code without the next as  adr  len .
: find-next  DUP BEGIN DUP next$ CORA WHILE 1+ REPEAT OVER - ;
: code$  >CFA @  find-next ;

: .code  OVER + DISASSEMBLE-RANGE ;
: ASS-CRACKED    code$ .code ;
: (crack)   DUP 'LIT = IF DROP $@ . ELSE ID. THEN  ;
: crack-hl   CELL+ @ BEGIN $@ DUP '(;) <> WHILE (crack) REPEAT 2DROP ;

DEFINITIONS
: NOP, XCHG|AX, AX| ;
HERE NOP, NOP, NOP, NOP,   DUP L@ CONSTANT NOPS4   C@ CONSTANT NOP@
: NOPS, 1 CELLS 0 DO NOP, LOOP ;

\ ' Q:MOVI|X,    HIDDEN
\ ' QN:MOVI|X,   HIDDEN
\ ' E:MOVI|X,    HIDDEN
\ ' EN:MOVI|X,   HIDDEN

PREVIOUS DEFINITIONS

\ ----------------------------------------------------
"RESTORED" WANTED    'OK RESTORED    1 WARNING !

\ Very general needed all over the place.
WANT $-PREFIX ALIAS @+ { ARGC GET-CURRENT
\ ALLOCATE must be loaded before class that uses SWAP-DP !
WANT ALLOCATE REGRESS
WANT :I class TRUE 0<>
REGRESS INIT-ALLOC S:

'0= ALIAS NOT?
'COUNT ALIAS C@+
:I CELL- 1 CELLS - ;
\ \ As long as the Forth is 32 bits the following works:
\ '@ ALIAS L@
\ '! ALIAS L!
\ Convert  signed-byte  to  signed  .
: C>S  DUP $80 AND 0= NOT? 8 LSHIFT OR ;
REGRESS  100 C>S  -100 $FF AND C>S   S: 100 -100
\ Convert  signed-long  to  signed  .
: L>S  DUP $8000,0000 AND 0= NOT? 32 LSHIFT OR ;
\ For  signed-long  :"It  is  negative"
: L0<  31 RSHIFT NEGATE ;
: fits-in-byte DUP 8 RSHIFT 0= SWAP $FF OR INVERT 0=   OR ;
REGRESS -8 fits-in-byte 8 fits-in-byte 1000 fits-in-byte S: TRUE TRUE FALSE
( ------------ DEBUGGING ------------------------------------- )
\ Use this decorator for `|^| if you want desperado debugging.
: decor-|^| R@ @ ID. &: EMIT &< EMIT .S DUP CRACK-COLON &> EMIT &; EMIT CR ;
: |^| ;

( ------------ CONFIGURATION --------------------------------- )

500 CONSTANT MAX-BAG       \ Default bag size (sets too)

\ Throw codes used :

( ------------- APPLICATION INDEPENDANT UTILITIES ----------------------------)
\ For a BAG print it backwards. Primarily intended as how to loop backwards example.
: BAG-PRINT-BACKWARDS
@+ SWAP BEGIN 2DUP > WHILE   >R CELL- >R
    R@ ?
R> R> REPEAT 2DROP ;

\ Fill from ADDRESS to END a number of cells with CONTENT.
: WFILL   ROT ROT SWAP ?DO DUP I !   0 CELL+ +LOOP DROP ;

\ Like +! but ors.
: OR!  DUP @ ROT OR SWAP ! ;

\ Like +! but ands.
: AND!  DUP @ ROT AND SWAP ! ;

\ Swap a SINGLE and a DOUBLE. Leave the DOUBLE and the SINGLE.
:I SDSWAP   ROT ;
\ Swap a DOUBLE and a SINGLE. Leave the SINGLE and the DOUBLE.
:I DSSWAP   ROT ROT ;

\ From ARRAY fetch element Index. Return IT.
:I [] CELLS + @ ;

\ From ARRAY fetch element ``I''. Return IT.
\ To be used within a loop
:I [I] I [] ;

\ Generate  n  constants from 1 up.
: ENUM 1+ 1 DO I CONSTANT LOOP ;

\ Generate  n  masks from 1 up.
: ENUM-MASK 0 DO 1 I LSHIFT CONSTANT LOOP ;

\ tooltje

( ------------ DATA STRUCTURES --------------------------------- )

\ The bag of dynamically allocated bblocks.
MAX-BAG BAG bblocks      bblocks !BAG
VARIABLE entry-bblock       \ Entry point of assembly code
VARIABLE semis-bblock       \ End block where semis code is to be added.

\ Initialise `bblocks, or reinitialize.
: !bblocks       bblocks DO-BAG I @ FREE THROW LOOP-BAG
    bblocks !BAG   NULL entry-bblock !   NULL semis-bblock ! ;

-1 CONSTANT UNKNOWN  \ Invalid address, different from NULL

\ Create a a basic block.
\ All properties are to be filled in later.
\ For `btype see optimcodgen.frt.
class BBLOCK
     M: this M;
         M: btype! ! M;
     M: btype @ M; _ ,     \ A tag that defines the properties of the bblock.
\ The proper technique is to collect all blocks from the entry point
\ on, then remember all blocks that still have to be collected
\ because they are referenced, but not yet gathered.
         M: ~gathered! FALSE SWAP ! M;
         M: gathered! TRUE SWAP ! M;
     M: gathered? @ M; FALSE ,

        M: bstart!  ! M;
     M: bstart  @ M;   _ ,   \ start of high level code.

        M: branch-target @ DUP  CELL- @ + M;
        M: ^branch-offset @ CELL- M;
        M: adjust-bend +! M;
        M: bend! ! M;
     M: bend  @ M;   _ ,

        M: ll@  2@ M;   \ lowlevel fetch (-- start end )
        M: ll!  2! M;   \ lowlevel store ( start end -- )
        M: bend-low!  ! M;
     M: bend-low  @ M;   NULL ,   \ end of low level code.
        M: bstart-low!  ! M;
     M: bstart-low  @ M;   NULL ,   \ start of low level code.

     M: fan-in M;    0 ,            \ # bblocks jumping to this one
     M: fan-out@   2@ 0= SWAP 0= + 2 + M;    \ # bblocks jumped out to
     M: next-bblock M;  UNKNOWN ,         \ Must not be zero initially!
     M: next-bblock-cond M;  UNKNOWN ,    \ Must not be zero initially! !
endclass

: (DUMP-BBLOCK)   CR
     "btype " TYPE btype . CR
     "this " TYPE this H. CR
     "bstart " TYPE bstart H. CR
     "bend " TYPE bend H. CR
     "bstart-low " TYPE bstart-low H. CR
     "bend-low " TYPE bend-low H. CR
     "fan-in " TYPE fan-in ? CR
     "next-bblock " TYPE next-bblock @  H. CR
     "next-bblock-cond " TYPE next-bblock-cond @ H. CR
     "branch-target " TYPE branch-target H. CR
     "^branch-offset " TYPE ^branch-offset @ H. CR
;

: DUMP-BBLOCK   ^BBLOCK !  (DUMP-BBLOCK) ;

\ Used in a `DO-BAG loop, make a bblock current.
:I >BBLOCK  I @ ^BBLOCK ! ;

\ Dynamically create a bblock and register it.
\ leave it as current in order to fill in other properties.
: new-bblock   BUILD-BBLOCK >ALLOC   DUP ^BBLOCK !   bblocks BAG+!   ;

\ Clone   bblock   . Leave the  clone  (also current.)
: clone-bblock   new-bblock this   DUP SIZE   MOVE   this ;

\ Do inlining and folding.
INCLUDE optimexpand.frt
INCLUDE optimhigh.frt
\ Inlines machine code.
INCLUDE optimcodgen.frt

\ : compiled compiled OK KEY &q = IF QUIT THEN ;
\ Try and optimise the DEA
: OPTIMISED  >R   R@ inline&fold
   R@ compiled DUP IF R@ >CFA ! ELSE DROP THEN
   RDROP ;

: OPTIMISED  DEPTH >R OPTIMISED DEPTH 1+ R> <> 4004 ?ERROR ;

\ Make expansion a noop.
: -w   SHIFT-ARGS   'TASK >DFA @ '(compress-bblock) >DFA ! ;

\ Print usage.
: USAGE
" Usage:
    start interactive system: optimiser
    help:        optimiser -h
    optimise:    optimiser [-w] source
    -w means without final compression step
" TYPE   ;

: ERROR-LIST
"
   ERROR NUMBERS
   The range 3000-4000 are internal errors in `optimiser'.
3017 : This optimisation should not have been called, because
    an earlier instruction already added a prefix.
3018 : matched length incorrect in optimisation
3019 : unexpected register found by optimisation
3020 : it is supposedly an unnecessary optimisation that is never called
   because the cases are covered by previous optimisations.
3025 : commutation with non-existing code attempted.
4002 : in optimhigh.frt : annihilation chain breaks down
4003 : in optimcodgen.frt FIXME:: ADD
4004 : OPTIMISED has not the proper stack effect (dea -- )
4005 : Unresolved source of a jump.
4007 : stack depth error
4008 : can't handle >32 bit constant (yet).
4009 : Attempt to run non-working code
" ;

\ Print usage, then go bye with EXITCODE 0.
: -h   USAGE     CR ERROR-LIST TYPE   0 EXIT-CODE !   BYE ;

\ Handle arguments, start interactive system if no arguments.
: HANDLE-ARG    ARGC 1 > IF 1 ARG[] OVER C@ &- = IF
        EVALUATE
    THEN THEN ;

'QUIT !OPTIMISED
'ERROR !OPTIMISED
ASSEMBLER
: doit 'OK RESTORED 'ERROR RESTORED 'ABORT RESTORED
  HANDLE-ARG
1 CELLS 4 = IF BITS-32 ELSE BITS-64 THEN
INIT-ALLOC ARGC 1 > IF 1 ARG[] INCLUDED ELSE QUIT THEN ;
