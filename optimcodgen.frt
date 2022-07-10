( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optimcodgen.frt,v 5.65 2021/05/06 14:15:48 albert Exp $)

\ This module tries to expand a sequence of code words xt's into
\ a code sequence with the contents of those words. In other words
\ it identifies basic blocks in high level code and transforms them
\ into a network of machine code basic blocks.
\ It also does some manipulations on that network, but optimisations
\ within a basic block are relegated to other modules.

\ The model is that a hl definition is a sequence of execution
\ tokens. You can get the dea from an execution token.
\ Also ' is assumed to give an execution token.

\ This file assumes in Forth all stack effects have been filled in,
\ and some special properties and optimisation bits, as per the
\ analyser.

\ Note that these don't need the class `bblock !
INCLUDE optimcategory.frt
INCLUDE optimisebblock.frt

WANT REGRESS TRUE ,,

'TASK @ CONSTANT docol

\ btypes, how a block ends. This remains valid for low levels code.
4 ENUM  #BRANCH #0BRANCH #FALL-THROUGH #SEMIS
\ Size of those block ends: size-end #XX []
\ These are the maximum sizes. After conversion to low level code
\ only #0BRANCH bblocks have code inside the block, that pertains to it.
\ It can be optimised but always will be a J|X, instruction.
\ All unconditional jump codes are inserted while gathering.
\ Such a jump may be added after a #0BRANCH bblock.
DATA size-end _ , 2 CELLS , 2 CELLS , 0 , 1 CELLS ,

\ ------------- Analysing blocks --------------------
\ Special THROW codes:
\ 1024 : some part of the code cannot be converted to machinecode.
\
\ For  dea  it  is  a normal code word.
: normalcode?  DUP >CFA @ SWAP >PHA = ;
REGRESS  'DROP normalcode?  S: TRUE
REGRESS  'NOOP normalcode?  S: TRUE
REGRESS  'TASK normalcode?  S: FALSE
REGRESS  'ASSEMBLER normalcode? S: FALSE

\ For  dea  it  is  a high level word.
: highlevel?  >CFA @ docol = ;
REGRESS  'DROP highlevel?  S: FALSE
REGRESS  'NOOP highlevel?  S: FALSE
REGRESS  'TASK highlevel?  S: TRUE
REGRESS  'ASSEMBLER highlevel? S: FALSE

DATA inlines-thingies   HERE 0 ,
 'LIT ,     \    'BRANCH ,       '0BRANCH ,
HERE SWAP !

\ For  dea  it is a  inlinable  code word with one word following in line.
: special?  inlines-thingies  IN-BAG? ;

\ For  dea  :it  is  an inlinable  code word.
\ Exceptions are possible and must be handled before.
: inlinable?   DUP normalcode? SWAP >FFA @ FMASK-IL AND NOT? AND ;
REGRESS  'DROP inlinable?  S: TRUE
REGRESS  'NOOP inlinable?  S: TRUE
REGRESS  'BRANCH inlinable?  S: FALSE
REGRESS  'LIT  inlinable?  S: FALSE

\ For  dea  : all constituents  are  inlineable shit thingies.
: candidate?   DUP highlevel? NOT? IF DROP FALSE EXIT THEN
    >DFA @  BEGIN  @+  DUP '(;) <> WHILE
    DUP inlinable? NOT?   OVER special? NOT? AND  IF 2DROP FALSE EXIT THEN
    special? IF CELL+ THEN
     REPEAT 2DROP TRUE
;
REGRESS : test3 18 ; 'test3 candidate? S: TRUE
REGRESS : TROT ROT ROT ; S:
REGRESS 'TROT candidate? S: TRUE
REGRESS : ifthen IF ROT ROT THEN ; 'ifthen candidate? S: FALSE
REGRESS : aap DROP R> TASK >R ; 'aap candidate? S: FALSE

\ For  pointer  to hl code, assemble equivalent code.
\ Return an advanced  pointer  .
ASSEMBLER
: handle-hl-item $@
    DUP special? IF DROP $@
    DUP 32 RSHIFT DUP 0<> SWAP INVERT 0<> AND 4008 ?ERROR
    Q: PUSHI|X, IL, EXIT THEN
    DUP inlinable? NOT? 1024 AND THROW
    code$ ,, ;
PREVIOUS
REGRESS  : test3 18 ; 'test3 >DFA @ handle-hl-item  DROP HERE 5 - C@ S: 104

\ For  start  end  which is high level code, expand to `HERE in low level.
: (hightolow)  SWAP   BEGIN  2DUP <> WHILE handle-hl-item REPEAT 2DROP ;

\ For current bblock compile bstart..bend then replace bstart bend.
ASSEMBLER
: hightolow   bstart   bend   size-end btype [] -
    HERE bstart-low! (hightolow)
        btype #0BRANCH = IF
            POP|X, AX|
            Q: AND, X| T| AX'| R| AX|
            J|X, Z|  Y| 0 (RL,)
        THEN
HERE bend-low!   ;
PREVIOUS
\ -------------------------------------------

\ A scratch bag for to items to be handled later.
MAX-BAG BAG unresolved

ASSEMBLER
\ For   bblock  append a conditional jump to it.
: conditional-jump
        ( bblock) unresolved BAG+! HERE unresolved BAG+! ;
REGRESS _ conditional-jump S:

\ For   bblock  append an unconditional jump to it.
: unconditional-jump
        JMP, 0 (RL,)
        ( bblock) unresolved BAG+! HERE unresolved BAG+! ;
REGRESS _ unconditional-jump S:
HERE _ unconditional-jump HERE - CONSTANT backup-jump
\ For   range  fill with nops.
: NOP-out   NOP@ FILL ;
PREVIOUS

\ Add bblock ending at  addr  to `bblocks .
\ Make sure it is current in order to fill in other properties.
    VARIABLE end-bblock  \ Where the previously created bblock ended.
: add-bblock   new-bblock    end-bblock @  bstart! DUP end-bblock !
 bend! ;

\ Resolve jumps, `unresolved contains pairs as stored by ..jump.
\ Note that relative jump distances are 32 bit : L not Q.
: resolve-jumps   unresolved DO-BAG >BBLOCK
    I CELL+ @   bstart-low  OVER -  SWAP 4 - L!
    2 CELLS +LOOP ;

\ For  end  btype  of bblock, register it and fill in its assembler code.
: add&lowlevel   >R  size-end R@ [] +   add-bblock  R> btype!   hightolow ;

\ For  address   if a block ends there add it to `bblocks.
: fill-one-bblock
    CONDS
    DUP @ 'BRANCH = IF #BRANCH add&lowlevel ELSE
    DUP @ '0BRANCH = IF #0BRANCH add&lowlevel ELSE
    DUP @ '(;) = IF #SEMIS add&lowlevel ELSE
        DROP
    THENS ;

\ Look up  targetaddr  return  bblock  (may be UNKNOWN).
: lookup ^BBLOCK @ >R
    UNKNOWN bblocks DO-BAG >BBLOCK
        OVER bstart = IF DROP this LEAVE THEN
    LOOP-BAG
    SWAP OVER UNKNOWN = IF unresolved SET+ ELSE DROP THEN
R> ^BBLOCK ! ;

\ Look up  targetaddr  .
\ Return  bblock  where it sits in, and make that current.
: which-bblock
    UNKNOWN bblocks DO-BAG >BBLOCK
        OVER bstart bend WITHIN IF DROP this LEAVE THEN
    LOOP-BAG  NIP  DUP UNKNOWN = 4003 ?ERROR
;

\ For the current bblock fill in the next bblock fields.
: complete-one-bblock
    next-bblock-cond @ UNKNOWN = IF
        btype #0BRANCH = IF   branch-target lookup ELSE NULL THEN
        next-bblock-cond !
    THEN
    next-bblock @ UNKNOWN = IF
        btype #SEMIS = IF   NULL THEN
        btype #FALL-THROUGH = btype #0BRANCH = OR IF   bend lookup THEN
        btype #BRANCH = IF branch-target lookup THEN
        next-bblock !
    THEN ;

: complete-bblocks   bblocks DO-BAG >BBLOCK   complete-one-bblock LOOP-BAG ;

\ For the current bblock fill in the outgoing fan numbers.
: fill-fan-one
    next-bblock-cond @   next-bblock @
    DUP IF ^BBLOCK !   1 fan-in +! ELSE DROP THEN
    DUP IF ^BBLOCK !   1 fan-in +! ELSE DROP THEN
;

: fill-fan   bblocks DO-BAG >BBLOCK   0 fan-in ! LOOP-BAG
             bblocks DO-BAG >BBLOCK   fill-fan-one LOOP-BAG
;

: dump-bblocks   CR "============" TYPE CR
    bblocks DO-BAG I @ DUMP-BBLOCK LOOP-BAG ;

\ For  targetaddress  that is inside the high level code of a bblock,
\ split that bblock at that point.
     VARIABLE bblock1                     VARIABLE bblock2
: split-bblock-high   >R
    R@ which-bblock  DUP bblock1 ! clone-bblock bblock2 !
    R@ bstart!   hightolow   1 fan-in !
    bblock1 @ ^BBLOCK !   R> bend!  #FALL-THROUGH btype!   hightolow
    bblock2 @ next-bblock !   NULL next-bblock-cond  ! ;

: resolve-bblocks   unresolved DO-BAG  I @ split-bblock-high LOOP-BAG ;

\ For a SEQUENCE fill the `bblocks set. The last block ends after the `(;).
\ The executable sequence is exactly the union of the nonoverlapping bblocks.
    : just-parse   NEXT-PARSE 2DROP ;
: fill-bblocks   DUP end-bblock !   !bblocks    unresolved !BAG
    BEGIN DUP fill-one-bblock DUP @ '(;) <> WHILE  just-parse REPEAT  DROP
    this semis-bblock !     bblocks CELL+ @ entry-bblock !
    complete-bblocks  resolve-bblocks  complete-bblocks fill-fan
    \ dump-bblocks
;
REGRESS : t 4 5 ; 't >DFA @ fill-bblocks btype next-bblock @ S: #SEMIS NULL
REGRESS : TROT ROT ROT ; 'TROT >DFA @ fill-bblocks S:
REGRESS bblocks 1 [] ^BBLOCK !  hightolow NEXT bstart-low 0<> S:   TRUE
REGRESS bstart-low 'TROT >CFA ! 1 2 3 TROT S: 3 1 2

\ -------------------------------------------

ASSEMBLER
: hightolow'
   "-------- htl start -----------" TYPE CR
   ll@ DISASSEMBLE-RANGE
   hightolow
   ll@ DISASSEMBLE-RANGE CR
   "-------- htl end -----------" TYPE CR
;
PREVIOUS

\ For  targetaddress  that is inside the low level code of a bblock,
\ split that bblock at that point.
\ `bstart and `bend become even less relevant.
\ unused and untested.
     VARIABLE bblock1                     VARIABLE bblock2
: split-bblock-low   >R
    R@ which-bblock  DUP bblock1 !
    DUP clone-bblock bblock2 ! R@ bstart-low!   1 fan-in !
    bblock1 @ ^BBLOCK !   R> bend-low!  #FALL-THROUGH btype!
    bblock2 @ next-bblock !   NULL next-bblock-cond  ! ;

\ Compile the current block.
: gather-one-bblock   gathered!
    HERE  ll@ OVER - ,, HERE ll!
    btype #0BRANCH = IF next-bblock-cond @ conditional-jump THEN
    btype #SEMIS = IF NEXT THEN ;
REGRESS : t 4 5 ; 't >DFA @ fill-bblocks S:
REGRESS bblocks 1 [] ^BBLOCK ! gather-one-bblock S:
REGRESS btype next-bblock @ S: #SEMIS NULL
REGRESS bstart-low 't >CFA ! t S: 4 5

\ Starting with current bblock lay down linear machine code, remember jumps,
\ until the semis block or one that has already been gathered.
: gather-chain
    BEGIN
    btype #SEMIS = IF gathered! this semis-bblock !   THEN
    gathered? NOT?  WHILE
        gather-one-bblock
        next-bblock @ ^BBLOCK !
    REPEAT
    this unconditional-jump ;
REGRESS : t2 8 9 ; 't2 >DFA @ fill-bblocks btype next-bblock @ S: #SEMIS NULL
REGRESS HERE 't2 >CFA ! gather-chain S:
REGRESS bblocks 1 [] semis-bblock @ = S: TRUE

\ Compile the last bblock,
\ Maybe add a test for not NULL.
: gather-last-bblock   semis-bblock @ ^BBLOCK ! gather-one-bblock ;
REGRESS : t3 13 14 ; 't3 >DFA @ fill-bblocks btype next-bblock @ S: #SEMIS NULL
REGRESS HERE 't3 >CFA ! gather-last-bblock t3 S:  13 14

\ Mark all blocks as not gathered.
: not-gathered bblocks  DO-BAG >BBLOCK ~gathered! LOOP-BAG ;

\ Compile the gathering in `bblocks , leave   startaddress to jump to.
\ FIXME : each chain ends with an unconditional jump, so there is one in front of semis.
: gather-bblocks   unresolved !BAG
    entry-bblock @ ^BBLOCK ! gather-chain
    bblocks  DO-BAG >BBLOCK
         gathered? NOT? fan-in @ 0<> AND IF gather-chain THEN
    LOOP-BAG
    gather-last-bblock
    resolve-jumps
    bblocks 1 [] ^BBLOCK ! bstart-low ;
REGRESS : t4 73 74 ; 't4 >DFA @ fill-bblocks btype next-bblock @ S: #SEMIS NULL
REGRESS gather-bblocks 't4 >CFA ! t4 S: 73 74

\ Make all bblocks following  start  into one bblock, replacing start.
: stitch-bblocks >R
    R@ ^BBLOCK !  HERE
    BEGIN ll@ OVER - ,,
        next-bblock @ ^BBLOCK ! btype #0BRANCH = UNTIL
    \ We are at the zero branch block, get its code and all its properties
    ll@ OVER - ,,
    btype next-bblock @ next-bblock-cond @
    \ Switch targets and invert condition, such that it loops on itself.
    SWAP   HERE 5 - 1 TOGGLE
    R> ^BBLOCK !   next-bblock-cond !   next-bblock !  btype!
    ( HERE ) HERE ll!  ;

\
\ Return : "The current bblock  is  a candidate for stitching."
: reordable?
\    btype #SEMIS = IF FALSE EXIT THEN
    this BEGIN next-bblock @ ^BBLOCK ! btype #0BRANCH = btype #SEMIS = OR UNTIL
    this = ;

\ :F optimise-one-bblock ;  \ By exception it is needed prematurely.

\ If the current bblock is the end of a primitive loop, make the loop into
\ one block.
: reorder-one-bblock  this reordable? IF
        ^BBLOCK ! next-bblock @ DUP stitch-bblocks  ^BBLOCK !
\         optimise-one-bblock
    ELSE DROP THEN ;

: reorder-bblocks
    bblocks  DO-BAG >BBLOCK
        btype #0BRANCH = IF reorder-one-bblock THEN
    LOOP-BAG ;

\ : optimise-RSP-one 2DROP ;  \ For now.

\ Combine   bblock   that has one output with the next one if
\ that one has this as only input, into a single block.
\ The next block becomes an orphan, no biggy.
: absorbe-next  >R    R@ ^BBLOCK !
    next-bblock @ ^BBLOCK !  fan-in @ 1 = IF
\          "The following block will be combined" TYPE CR
\          this DUMP-BBLOCK   KEY DROP ll@ CR D-R CR KEY DROP
         ll@   next-bblock @   next-bblock-cond @  0 fan-in !
         R@ ^BBLOCK !  next-bblock-cond !  next-bblock !
         HERE >R ll@ OVER - ,, OVER - ,,      R> HERE ll!
\      "The following block results " TYPE CR
\      this DUMP-BBLOCK   KEY DROP ll@ CR D-R CR KEY DROP
    THEN RDROP ;

\ Combine all block pairs
: combine-bblocks
    bblocks  DO-BAG >BBLOCK
\         this entry-bblock @ = IF
\        "Hurrah, entry block found" TYPE CR
\        this DUMP-BBLOCK   KEY DROP ll@ CR D-R CR KEY DROP
\         THEN
        fan-out@ 1 =   next-bblock @ this <>   AND IF
\             "LL@ " TYPE ll@ . . CR
            this absorbe-next
        THEN
    LOOP-BAG ;

\ Kill the current bblock.
\ LEADS TO CRASH
: kill-bblock
   next-bblock @   next-bblock-cond @
\    "The following bblock is about to be killed" TYPE CR
\    this DUMP-BBLOCK   KEY DROP ll@ CR D-R CR KEY DROP
    this DUP SIZE ERASE
    this FREE THROW
   DUP IF ^BBLOCK !   -1 fan-in +! ELSE DROP THEN
   DUP IF ^BBLOCK !   -1 fan-in +! ELSE DROP THEN
;

\ Eliminate all bblocks that are unreachable.
\ LEADS TO CRASH
: eliminate-bblocks  4009 THROW
    bblocks  DO-BAG >BBLOCK
    fan-in @ 0=   this entry-bblock @ <>   this semis-bblock @ <>  AND AND IF
        kill-bblock
        NULL I !  \ Can't remove just yet, mark for removal.
    THEN
    LOOP-BAG

     bblocks |BAG|
     BEGIN NULL bblocks BAG-WHERE DUP WHILE bblocks BAG-REMOVE REPEAT DROP
     bblocks |BAG| <> IF RECURSE THEN
;

\ The following code was useful, but modern is to use a decorator.
\    ll@ .S DISASSEMBLE-RANGE ." before" KEY DROP

\ The crux: optimise the current bblock.
\ :R optimise-one-bblock   \ for now
: optimise-one-bblock   \ for now
    10 BEGIN 0 PROGRESS !  0 COMMUTED !
        ll@ (optimise-bblock) ll!
        PROGRESS @ IF DROP 10 THEN   COMMUTED @ +
    DUP 0 > COMMUTED @ AND   PROGRESS @   OR WHILE
    REPEAT DROP ;

\ Optimise all blocks.
: optimise-bblocks bblocks  DO-BAG >BBLOCK optimise-one-bblock LOOP-BAG ;
: expand-bblocks bblocks  DO-BAG >BBLOCK ll@ (expand-bblock) ll!  LOOP-BAG ;
: compress-bblocks bblocks  DO-BAG >BBLOCK ll@ (compress-bblock) ll! LOOP-BAG ;

6 CONSTANT jumpsize
chgrsp-pattern mask$ @ CONSTANT LEARSPsize

\ Return a  string   that contains the alloc part.
: alloc$      bstart-low LEARSPsize ;

\ Clone the alloc part of a bblock (that has one) into the dictionary,
\ leave  range  .
: clone-alloc-range HERE alloc$  ,, HERE ;

\ Before  bblock1  insert a return stack allocation bblock
\ if  nextpos  points to it.
: ?insert-alloc-bblock?   2DUP @ = IF >R
        DUP ^BBLOCK !  clone-alloc-range   clone-bblock   R> !
        ll!   next-bblock !  NULL next-bblock-cond ! 1 fan-in !
    ELSE 2DROP THEN ;

\ Duplicate the alloc part of   block   for all jumps to it.
: split-off-RSP-allocate
    bblocks  DO-BAG
        >BBLOCK DUP next-bblock ?insert-alloc-bblock?
        >BBLOCK DUP next-bblock-cond ?insert-alloc-bblock?
    LOOP-BAG
    \ All jumps now pass through a separate alloc block.
   ^BBLOCK ! alloc$ NOP-out ;

\ Return a  string   that contains the release part for #0BRANCH .
: release$      bend-low jumpsize -  LEARSPsize -   LEARSPsize ;

\ Clone the release part of a bblock (that has one) into the dictionary,
\ leave  range  .
: clone-release-range HERE release$  ,, HERE ;

\ : .SD  .S (DUMP-BBLOCK) ;
:I .SD ;
\ Split off the return stack release of the current block to two different
\ blocks, and conditionally jump to those.
: split-off-RSP-release this  >R                      .SD
    \ Create next conditional and next straight-trhrough intermediate blocks.
    R@ clone-bblock    next-bblock-cond @ next-bblock !  NULL next-bblock-cond !
    #BRANCH btype!  1 fan-in !     .SD
    R@ clone-bblock    NULL next-bblock-cond !  #BRANCH btype!  1 fan-in !  .SD
    R@ ^BBLOCK !   OVER next-bblock-cond ! DUP next-bblock !                .SD

    \ Replace content with release RSP code.
    R@ ^BBLOCK !   clone-release-range   ROT ^BBLOCK ! ll!      .SD
    R@ ^BBLOCK !   clone-release-range   ROT ^BBLOCK ! ll!      .SD

    \ The conditional code J|X, stays, the release code is removed.
    R@ ^BBLOCK !   release$ NOP-out    ll@ D-R
   RDROP ;

\ Split all the RSP allocation and release stuff into separate bblocks.
: testje
          CR 80 0 DO &* EMIT LOOP CR
          ll@ .S D-R
          fan-in ?
          CR 80 0 DO &+ EMIT LOOP CR
;

\ Move the allocation and release of return stack space to separate blokcs,
\ such that they can be rearranged. This has been tested, but the result
\ can not be gathered properly.
: split-off-RSP
    DSP@ stopper !  \ Make sure matches work.
    bblocks  DO-BAG >BBLOCK
\        testje
       >BBLOCK
       chgrsp-pattern bstart-low matches? fan-in @ 1 > AND IF
\             split-off-RSP-allocate
        THEN
       >BBLOCK
        next-bblock-cond @  ( fan-out is 2 ) IF
            release$ DROP DUP chgrsp-pattern matches?
            SWAP bstart-low >   AND IF
\                  split-off-RSP-release
           THEN
        THEN
    LOOP-BAG ;

: optimise-RSP
    init-RSP
    bblocks  DO-BAG >BBLOCK ll@ analyse-return-one LOOP-BAG
    report-RSP
    \ For now we are at a loss of what to do if this fails.
    RPO-LEA's @ 2 <> IF EXIT THEN
    bblocks  DO-BAG >BBLOCK ll@ optimise-RSP-one LOOP-BAG
    \ FIXME. For some reason this must be followed by expanding the blocs ????
    \ In particular test2aa fails, without this.
    expand-bblocks
;

\ :I kk   DEPTH DUP . R@ . &$ EMIT R@ <> 4007 ?ERROR ;
:I kk ;
\ INCLUDE dumpdump.frt
\ For  dea  high level return a  pointer  to a code sequence or null.
: compiled   >DFA @ 'fill-bblocks  CATCH
    DUP 1024 = IF DROP 0 ELSE THROW THEN
    DEPTH >R                         kk
    expand-bblocks                   kk
    optimise-bblocks                 kk
    split-off-RSP                    kk
\     dumpdump
    optimise-RSP                     kk
    reorder-bblocks                  kk
    combine-bblocks                  kk
\     eliminate-bblocks                kk
    optimise-bblocks                 kk
    compress-bblocks                 kk
    not-gathered                     kk
\    dumpdump
    gather-bblocks
    RDROP
;
