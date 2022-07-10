( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optimisebblock.frt,v 5.121 2020/07/14 22:18:45 albert Exp $)

\ This is part of the optimiser for ciforth.
\ It works on bblocks "basic blocks" , i.e. it is not concerned with jumps.
\ It does however take into account with what jump the basic block
\ ends in behalf of optimisation context.
\ It doesnot rely on the class `bblock that retains many properties
\ apart from its code range.
\ All code snippets are passed as ranges (low high) and may be addressed
\ as "bblock" in this source module.
\ Here high is always the method `bend-high of the
\ bblock concerned, and it is stored in `stopper. Because all matching is
\ done by the same word, the impact of this global variable is limited.
\ It is also useful in antibugging.
\ In the future there may be some internal jumps, i.e. jumps
\ that do not pass control outside of the basic block.
\ All registers except RSP DSP and HIP are dead on entry and on exit of
\ such a bblock.
\ This module replaces the code of a bblock with equivalent faster or
\ shorter code. It works directly on machine code.
\ All registers except RSP DSP HIP or dead on start and exit of a bblock,
\ until further notice.
\
\ Design note: steps are
\    1.
\         A replace byte with xell in data and indices
\         B replace short branches with long
\         C replace one byte instruction reg A with regular instruction
\         D replace 32 bit with 64 bits
\         E replace T| R| by F| R|
\    2. do peephole
\    3. move instruction to ends or beginning and remove if possible
\    4. reverse one

\ WARNING: This code only works for little-endian (Intel)!
\ FIXME: using ALLOCATE it can be changed to a single pointer. (Using SIZE).

\ 'REGRESS HIDDEN
WANT REGRESS
WANT DUMP BOUNDS ,, class
WANT CASE
VARIABLE COMMUTED
INCLUDE optbb_tools.frt
\ This sequence represents steps
\ 1. make instructions uniform
INCLUDE optbb_expand.frt
\ 2a. non-controversial transformation
INCLUDE optbb_nobrain.frt
\ 2b. more subtle transformation
INCLUDE optbb_gen.frt
\ 2c. propagation transformation
INCLUDE optbb_propagate.frt
\ 2d. return stack optimisation
INCLUDE optbb_RSP.frt
\ 3. shorten instructions if there are equivalents
INCLUDE optbb_compress.frt

\ ----------------------------------------------------------------
\ ----------------------------------------------------------------
\ GENERAL maybe up front
\ From  adr1  :leave  adr2  gap  .
\ The `gap is a piece of code that ends at `adr2 and
\ is to be copied verbatim to the output.
: default-gap DUP >R  ~DISASSEMBLE R> OVER ;

\ We are advanced to  adr2  and there is code in    gap  .
\ Find next instruction and swap it verbatim with the one in gap.
\ Leave next instruction  in  gap  preceeded by the end of the gap.
\
\ ----------------------------------------------------------------
( adr1 adr2 n -- adr1 )
\ Swap `n instructions at `adr2 with  the range [ `adr1 `adr2)
\ which is mostly one instruction, or a pupo.
\ The output is similar but the instructions are swapped *at the input* .
\ so reside from `adr1
: kks CR &< EMIT   ." <commute " 2DUP D-R KEY DROP ;
: kke 2DUP D-R CR ." commute> " KEY DROP ;
:I kke ; :I kks ;
: commute2  OVER >R  \ 1 OVER <> 4026 ?ERROR
   BEGIN DUP WHILE 1- SWAP ~DISASSEMBLE SWAP REPEAT DROP
   R> SWAP  ( -- adr1 adr2 adr3 )
   DUP stopper @ > IF 2DROP EXIT THEN
   2DUP OVER -  bufc$ $!    DROP  ( -- adr1 adr2 )
   2DUP OVER -   bufc$ $+!  DROP  ( -- adr1 )
   bufc OVER bufc$ @  MOVE
   TRUE COMMUTED !
;

\ REGRESS   HERE Q: POP|X, AX|  HERE Q: PUSH|X, BX|  2DUP commute2 DROP ROT = ROT ROT = S: TRUE TRUE
\ REGRESS   HERE Q: POP|X, AX|  HERE Q: LEA, SI'| XO| [BP]  12 L, 2DUP commute2 DROP ROT 5 + = ROT ROT = S:   TRUE TRUE

\ ----------------------------------------------------------------
\ \ Return "the current disassembly  is  two operands."
\ : TWO-OPERANDS   'T| PDISS BAG-WHERE 'F| PDISS BAG-WHERE OR
\    '[BP] PDISS BAG-WHERE OR 0<> ;
\
\ ----------------------------------------------------------------
\ Investigate change of BP with xell offset.

<! Q: !!T LEA, BP'| XO| [BP]  !!T   0 {L,} ~!!T !>
<A Q: LEA, BP'| XO| [BP]  0 L, A>
{   bufv 3 + L@   bufc 3 + L! }
optimisation chgrsp-pattern

REGRESS chgrsp-pattern DUMPO S:
REGRESS HERE Q: LEA, BP'| XO| [BP] 8 L, matches? bufv$ @ bufv 3 + L@ S: TRUE 7 8
REGRESS HERE Q: LEA, BP'| XO| [BP] -8 L, matches? bufv 3 + L@ S: TRUE $FFFFFFF8

\ ----------------------------------------------------------------

\ After an unsuccessful match we have   adr2  adr1  adr2 , where there is
\ one instruction in `adr1. We know that there is still code behind
\ `adr2. If that code is to commute with the one instruction, swap them
\ into a buffer and return   adr3  the end of the second instruction and
\   gap  the buffer with the two instructions in the order wanted .

\ For instr at  addr  :"It  is  a movi immediate to register."
: IS-MOVI?  C@+ prop@ QE = >R C@ prop@ IMM1 = R> AND ;
REGRESS  HERE Q: MOVI|X, AX|  1234 IL, IS-MOVI? S: TRUE
REGRESS  HERE QN: MOV, X| T| DX'| REL| 1234 RL, IS-MOVI? S: FALSE

\ For instr at  addr  :"Its type  allows  commuting with movi"
: IS-MOVI-COMPAT?  REGULAR?  ;
REGRESS  HERE Q: MOVI|X, AX|  1234 IL, IS-MOVI-COMPAT? S: TRUE
REGRESS  HERE Q: POP|X, AX|  IS-MOVI-COMPAT? S: TRUE
REGRESS  HERE QN: MOV, X| T| DX'| REL| 1234 RL, IS-MOVI-COMPAT? S: TRUE
REGRESS HERE Q: ADD, X| F| AX'| R| BX| IS-MOVI-COMPAT? S: TRUE
REGRESS HERE Q: ADD, X| F| AX'| R| CX| IS-MOVI-COMPAT? S: TRUE
REGRESS HERE Q: POP|X, CX| IS-MOVI-COMPAT? S: TRUE
REGRESS HERE Q: PUSH|X, CX| IS-MOVI-COMPAT? S: TRUE
REGRESS HERE Q: PUSH|X, BX| IS-MOVI-COMPAT? S: TRUE

\ ----------------------------------------------------------------
\ For instr at  addr  :"It  is  a pupo."
: IS-PUPO?  pupo-pattern matches? ;
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| IS-PUPO? S: TRUE

\ For instr at  addr  :"Its type  allows  commuting with pupo"
\ FIXME: a pupo doesn't commute with a pushi, but it should.
\ : IS-PUPO-COMPAT?   DUP REGULAR?  SWAP 1+ C@ prop@ #STK AND 0= AND ;
: IS-PUPO-COMPAT?   REGULAR?  ;

: ?.S?  .S KEY DROP ;
: ?.S?  ;

\ ----------------------------------------------------------------
\ For instr at  addr  :"It  is  a push immediate."
: IS-PUSHI?  C@+ prop@ QE = >R C@ prop@ #PSHI = R> AND ;
REGRESS  HERE Q: PUSHI|X, 1234 IL, IS-PUSHI? S: TRUE
REGRESS  HERE QN: MOV, X| T| DX'| REL| 1234 RL, IS-PUSHI? S: FALSE

\ For instr at  addr  :"Its type  allows  commuting with pushi"
: IS-PUSHI-COMPAT?  DUP REGULAR?  SWAP 1+ C@ prop@ #STK AND 0= AND ;
\ : IS-PUSHI-COMPAT?   DUP REGULAR? OVER 1+ C@ prop@ #STK AND  IF
\     DROP FALSE ELSE DROP TRUE THEN ;

REGRESS  HERE Q: MOVI|X, AX|  1234 IL, IS-PUSHI-COMPAT? S: TRUE
REGRESS  HERE Q: POP|X, AX|  IS-PUSHI-COMPAT? S: FALSE
REGRESS  HERE QN: MOV, X| T| DX'| REL| 1234 RL, IS-PUSHI-COMPAT? S: TRUE
REGRESS HERE Q: ADD, X| F| AX'| R| BX| IS-PUSHI-COMPAT? S: TRUE
REGRESS HERE Q: ADD, X| F| AX'| R| CX| IS-PUSHI-COMPAT? S: TRUE
REGRESS HERE Q: POP|X, CX| IS-PUSHI-COMPAT? S: FALSE
REGRESS HERE Q: PUSH|X, CX| IS-PUSHI-COMPAT? S: FALSE
REGRESS HERE Q: PUSH|X, BX| IS-PUSHI-COMPAT? S: FALSE

\ ( end-adr gap -- end-adr' gap' )
\ : ?commute-pushi?  OVER IS-PUSHI? IF DUP IS-PUSHI-COMPAT? OVER IS-MOVI? NOT? AND IF
\ \        DUP IF TRUE COMMUTED ! THEN
\        commute  TRUE COMMUTED !
\ THEN THEN ;
\
\ REGRESS  HERE Q: PUSHI|X, 12 IL, HERE Q: ADD, X| T| BX'| R| CX| DUP ROT ROT ?commute-pushi? -  NIP  S: -3
\ REGRESS  HERE Q: PUSHI|X, 12 IL, HERE Q: POP|X, BX| DUP ROT ROT ?commute-pushi? -  NIP  S: -6
\ REGRESS  HERE Q: PUSHI|X, 1 IL, HERE Q: PUSHI|X, 2 IL, DUP ROT ROT ?commute-pushi? -  NIP  S: -6

\ ----------------------------------------------------------------
\ Not used?
\ \ GENERAL maybe up front
\ \ For regular reg  fixup: "It is implied in the latest disassembly"
\ : reg-used  >R  R@ PDISS BAG-WHERE  R@ reg->reg' PDISS BAG-WHERE  OR
\     R@ reg->[reg] PDISS BAG-WHERE  OR 0<> RDROP ;
\ REGRESS HERE POP|X, AX|  ~DISASSEMBLE DROP 'AX| reg-used S: TRUE
\ REGRESS HERE LEA, BP'| BO| [BP]  $80 B, ~DISASSEMBLE DROP 'BP| reg-used S: TRUE
\ REGRESS HERE LEA, BP'| BO| [BP]  $80 B, ~DISASSEMBLE DROP 'AX| reg-used S: FALSE


\ For  ins1  ins2  : flag  .
\ `flag means that the first instruction can be annihilated.
\ That means that the first instruction has as a destination
\ the same desination as the second instruction and it is none of the
\ sources of the second instruction. Moreover the first instruction
\ must not disturb the stack.
\ Using masks: rotate one
: src<->dst  DUP 32 RSHIFT OR SWAP 32 RSHIFT AND ;
\ can interchange two instructions, you can interchange them back!
\ : annil?-masks   tbd ;
\ : annil? RGL-modmask SWAP RGL-modmask .SH annil?-masks ;


\ For  ins1  ins2  : the instructions  can  be interchanged.
\ That means that the first instruction doesnot has as a destination any
\ of the sources or destinations of the second instruction.
\ It also means that the second instruction doesnot have as a destination
\ any of the source or destinations of the first instruction.
\ Bloody hell, it is symmetrical! Who would have though that if you
\ can interchange two instructions, you can interchange them back!
\ Using masks: rotate one
: no-overlap-masks   2DUP src<->dst >R SWAP src<->dst R> OR 0= ;
: no-overlap RGL-modmask SWAP RGL-modmask .SH no-overlap-masks ;

\ For   addr  return:  gap  cnt  .
\ The  `gap  starts at `addr and at its end there are `cnt instruction
\ that may be commuted with the content of the gap.
\ `cnt is mostly zero, no commute possible.
\ Note:
\ The " 1 AND " phrase is shorthand for "IF 1 ELSE 0 THEN "
\ It works because forth flags are all bits set.
: #commute
    DUP ~DISASSEMBLE >R
CONDS
    R@ stopper @ >= IF FALSE
    ELSE chgrsp-pattern R@ matches? IF
        \ Following instruction changes rsp.
        '[BP] PDISS BAG-WHERE NOT?   R@ 3 + L@ L>S 0 < AND
        1 AND
    ELSE chgrsp-pattern DUP matches? IF
        \ This instruction changes rsp.
        R@ ~DISASSEMBLE DROP  \ Just fill `PDIS .
        '[BP] PDISS BAG-WHERE NOT?
        'J|X,   PDISS BAG-WHERE NOT?  AND
        OVER 3 + L@ L>S 0 > AND
        1 AND
    ELSE DUP IS-PUSHI? IF
        R@ IS-PUPO? IF 2 ELSE
            R@ REGULAR?  R@ 1+ C@ prop@ #STK AND 0= AND
\             R@ IS-MOVI? NOT? AND   1 AND
            1 AND
        THEN
    ELSE DUP  IS-MOVI? IF
        R@ IS-MOVI-COMPAT? OVER R@ no-overlap AND
        1 AND
     ELSE DUP pupo-pattern matches? IF \ .S
         RDROP DUP bufv$ @ + >R  \ Not one instruction like with ~DISASSEMBLE
         R@ IS-PUPO-COMPAT?                                        \ .S
         pupo-get-mask   R@ RGL-modmask   no-overlap-masks   AND   \ .S
        1 AND
    ELSE
        0
THENS
    R> SWAP
;
\ ???            ( ?commute-pupo?)

\ In the following 'HERE without correction means commuting took place.
\ REGRESS HERE >R POP|X,  AX| LEA, BP'| BO| [BP]  $80 B, R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE 3 - -1
\ REGRESS HERE >R LEA, BP'| BO| [BP]  4 B, POP|X, AX| R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE -4
\ REGRESS HERE >R MOVI|X, BX| 4 IL, POP|X, AX| R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE -6
\ REGRESS HERE >R MOVI|X, AX| 4 IL, POP|X, AX| R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE 1- -5
\ REGRESS HERE >R PUSHI|X, 4 IL, POP|X, AX| R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE 1- -5
\ REGRESS HERE >R PUSHI|X, 4 IL, PUSH|X, AX| R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE 1- -5
\ REGRESS HERE >R PUSHI|X, 4 IL, MOVI|X, AX| 5 IL, R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE 5 - -5
\ REGRESS HERE >R PUSHI|X, 4 IL,  LEA, BP'| BO| [BP]  $80 B, R@ ~DISASSEMBLE R> OVER ?commute2? - S: HERE -8
\ REGRESS bufc$ $@ OVER + DISASSEMBLE-RANGE S:

\ For code at  adr1  :
\  leave  addr1  and "it  matches  some optimisation pattern."
\ For a match relevant buffers are set. `bufv$ contains the variant part of
\ the match. `bufc$ contains a proposed replacement. See `next-gap.
\ FIXME: an operation with a result into a register at the end of
\ a non-conditional block can also be removed,
\ as long as the register is not permanent.
: peep-hole
    \ ignore a failing modification of a pupo, because of pupomovi,pupopo, puponihil

    pupo-pattern DUP matches? IF
        ?pupo-nihil-replace? IF TRUE EXIT THEN
        ?puponope-replace? IF TRUE EXIT THEN
        ?pupo-replace? IF TRUE EXIT THEN
        ?pupo-propagate? FALSE EXIT
    THEN
    CONDS
    \ The remaining patterns are excluding each other, hence the ELSE's.
    xchg-aa-pattern DUP matches? IF replace TRUE ELSE
    condition-pattern DUP matches? IF replace TRUE ELSE
\     noq-pushi-pattern DUP matches? IF replace TRUE ELSE  removed
    pushi-pattern DUP matches? IF replace TRUE ELSE
\     movfromr-pattern DUP matches? IF replace TRUE ELSE
    movipop-pattern DUP matches? IF ?movipop-replace? ELSE
\     DUP is-movilea IF go-movilea TRUE ELSE
    movimovxo-pattern DUP matches? IF ?movimovxo-replace? ELSE
\     movilea-pattern DUP matches? IF ?movilea-replace? ELSE
    movipush-pattern DUP matches? IF ?movipush-replace? ELSE
\     movicmpr-pattern DUP matches? IF ?movicmpr-replace? ELSE
\ Complication, because same pattern for two replacements
\ More over, for now rgopr makes things slower.
    movigopr-pattern DUP matches? IF
        ?movigopr-replace?
\         IF TRUE ELSE movirgopr-pattern DUP matches? IF ?movirgopr-replace? THEN THEN
    ELSE
\ Results in XCHG left out with no ill effect !!
\     pupopo-pattern DUP matches? IF ?pupopo-replace? ELSE
    movimovr-pattern DUP matches? IF ?movimovr-replace? ELSE
    movzx-pattern DUP matches? IF ?movzx-replace? ELSE
    movijx-pattern DUP matches? IF replace TRUE ELSE
    xorijx-pattern DUP matches? IF replace TRUE ELSE
    subjx-pattern DUP matches? IF replace TRUE ELSE
    andjx-pattern DUP matches? IF replace TRUE ELSE
\     DUP is-movimovix IF go-movimovix TRUE ELSE
\     DUP is-movimovb IF go-movimovb TRUE ELSE
\     DUP is-movimovbob IF go-movimovbob TRUE ELSE
\ Results in XCHG left out with no ill effect !!
\    pupomovi-pattern DUP matches? IF ?pupomovi-replace? ELSE
\ This has no effect! Comment in when it has!
    lealea-pattern DUP matches? IF ?lealea-replace? ELSE
    lea0-pattern DUP matches? IF ?lea0-replace? ELSE
    movrspxo-pattern DUP matches? IF ?movrspxo-replace? ELSE
    movxorsp-pattern DUP matches? IF ?movxorsp-replace? ELSE
    xchg-pattern DUP matches? IF ?xchg-replace? ELSE
    oprnotest-pattern DUP matches? IF ?oprnotest-replace? ELSE
    noclc-pattern DUP matches? IF ?noclc-replace? ELSE
    movinope-pattern DUP matches? IF ?movinope-replace? ELSE
\     DUP is-movdecrsp IF go-movdecrsp TRUE ELSE
\    DUP is-decincrsp IF go-decincrsp TRUE ELSE
\    DUP is-xorjmp IF go-xorjmp TRUE ELSE
        FALSE
     THENS
;

\ Do a `peep-hole but give it a second change after commuting.
\ The effect is the same than `peep-hole. (addr1 -- addr1 flag)
: peep-hole2
    peep-hole IF TRUE ELSE
        #commute DUP IF commute2 peep-hole ELSE 2DROP FALSE THEN
    THEN ;

\ From  adr1  stopper  :advance to  adr2  and return equivalent code for
\ (adr1, adr2) into  gap  .
\ This may be an optimised equivalent or the original code.
\ (adr1,adr2) need not be a single instruction.
\ `adr2 must not extend beyond `stopper
: next-gap  stopper !
    peep-hole2 IF
        \ Advance by what is matched and return the gap composed
        bufv$ @ +   bufc$ $@ OVER +   TRUE PROGRESS !
    ELSE
        default-gap
    THEN
\     &: EMIT   2DUP D-R &* EMIT CR \ KEY DROP
;
REGRESS HERE PUSH|X, AX| POP|X, AX| CONSTANT pupo S:
REGRESS pupo HERE next-gap  S: pupo 1 + pupo pupo 1+
REGRESS  DATA puno PUSH|X, BX|  POP|X, CX|  S:
REGRESS puno HERE next-gap  S: puno 1+   puno puno 1+
\ REGRESS condition HERE next-gap S: condition 19 + bufc DUP 6 +

\ Put  gap at `HERE.
: l>here    OVER - ,, ;
\ : next-gap 2DUP DISASSEMBLE-RANGE next-gap &_ EMIT CR KEY DROP ;
\ : next-gap next-gap CR this H. &: EMIT 2DUP DISASSEMBLE-RANGE &_ EMIT CR KEY DROP ;
\ For  range  which is low level code, copy to `HERE while optimising.
\ Return a new  range  .
: (optimise-bblock) SWAP   HERE >R
    BEGIN 2DUP > WHILE
        OVER next-gap l>here
    REPEAT 2DROP
    R> HERE \ start end.
\     ." next " 2DUP D-R CR KEY DROP
;

: kk  2DUP D-R &^ EMIT KEY DROP ;
:I kk ;

\ Stack diagram is like `(optimise-bblock)  (range -- range')
: (expand-bblock)
    HERE >R       kk
    SWAP BEGIN 2DUP > WHILE
        DUP Q-needed? IF Q: THEN   default-gap
    l>here REPEAT 2DROP
    R> HERE       kk
    HERE >R
    SWAP BEGIN 2DUP > WHILE
        OVER stopper ! (next-gap-expand)
        IF bufv$ @ + bufc$ $@ OVER + ELSE default-gap THEN
    l>here REPEAT 2DROP
    R> HERE        kk
;
: (compress-bblock)
    HERE >R
    SWAP BEGIN 2DUP > WHILE
        OVER stopper ! (next-gap-compress)
        IF bufv$ @ + bufc$ $@ OVER + ELSE default-gap THEN
    l>here REPEAT 2DROP
    R> HERE          kk
    HERE >R          kk
    SWAP BEGIN 2DUP > WHILE
        DUP Q-unneeded? IF 1+ THEN   default-gap
    l>here REPEAT 2DROP
    R> HERE            kk
;

PREVIOUS

\ Post optimisers go here for now.

\ ----------------------------------------------------------------

\ Make a move to a register short.
\ FIXME: to be done at the end, not after a long time

\ \ Use the bald `{L,} instead of `L, whenever we don't want to disturb
\ \ the state machine.
\ DATA movimovix  <!
\    MOVI, X| R| !!T  0 {L,} ~!!T
\    !>  movimovix-mask$
\ REGRESS movimovix-mask$ 40 DUMP S:
\ REGRESS movimovix-mask$ $@  NIP S: 6
\
\ CODE movimovix-replacement
\    MOVI|X, 0 IL, !TALLY
\    NEXT
\ END-CODE
\
\ \ Compare  pointer  to code with `movimovix
\ : is-movimovix   movimovix movimovix-mask$ matched ;
\ REGRESS movimovix 1+ is-movimovix S: FALSE
\ REGRESS HERE MOVI|X, BX| 1234 IL, is-movimovix S: FALSE
\ REGRESS HERE MOVI, X| R| BX| 1234 IL, is-movimovix S: TRUE
\ REGRESS HERE MOVI, X| R| DI| 2 IL,  is-movimovix S: TRUE
\
\ \ Fill in the replacement for a matched movimovix
\ : go-movimovix
\     'movimovix-replacement code$   bufc$ $!
\     bufv 2 + L@  bufc 1+ L!
\     bufv 1+ C@  bufc OR!U
\ ;
\ REGRESS  go-movimovix  bufc$ @ S: 5
\ REGRESS bufc$ $@ OVER + DISASSEMBLE-RANGE S:
