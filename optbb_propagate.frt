( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_propagate.frt,v 5.7 2020/08/16 18:37:45 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.

\ This file contains peephole optimisations that replace a source register
\ whose content has just been filled in with that content.
\ In particular it works on push/pop pairs followed by an
\ instruction that uses the register that was popped.

\ For  addr  return the  mod-mask  of the instruction there.
\ NONE means that it can't be handled.
: RGL-modmask   >R
CONDS
   R@ C@ prop@ QE <> IF NONE ELSE
   R@ Qp@ OP0 AND IF 0 ELSE    \ PUSHI
   \ MOVI|X, PUSH|X, POP|X,
   R@ Qp@   SZ1 OP1 OR  AND  SZ1 OP1 OR  = IF
        R@ get1-msk-QN
        R@ Qp@ #SN AND IF 0 ELSE 32 THEN
        LSHIFT
   ELSE
   R@ Qp@   SZ2 OP2 OR  AND  SZ2 OP2 OR  = IF
        R@ get2-msk-QN  R@ get2-msk-Q'
        R@ Qp@ #SN AND 0= IF SWAP THEN  32 LSHIFT OR
   ELSE
   R@ Qp@   SZ2 OP1 OR  AND  SZ2 OP1 OR  = IF   \ immediate
        R@ get2-msk-QN
        \ Also a target, unless comparing
        R@ 1+ @ 'CMPI, >DATA @ XOR 11 RSHIFT 7 AND IF DUP 32 LSHIFT OR THEN
   ELSE
   \ I can't deal behind more for now
   NONE
THENS RDROP
;
REGRESS  HERE CLD, RGL-modmask   S: NONE
REGRESS  HERE Q: PUSHI|X, 12 IL, RGL-modmask   S: 0000,0000,0000,0000
REGRESS HEX HERE Q: PUSH|X, AX| RGL-modmask   S: 0000,0000,0000,0001
REGRESS HERE Q: POP|X, AX| RGL-modmask   S: 0000,0001,0000,0000
REGRESS  HERE E: MOVI|X, BX| 12 IL, RGL-modmask   S: 0000,0008,0000,0000
REGRESS  HERE EN: MOVI|X, BX| 12 IL, RGL-modmask      S: 0000,0800,0000,0000
REGRESS  HERE QN: POP|X, BX| RGL-modmask   S: 0000,0800,0000,0000
REGRESS  HERE Q: ADD, X| F| AX'| R| BX|  RGL-modmask   S: 0000,0008,0000,0001
REGRESS  HERE Q: ADD, X| T| AX'| R| BX|  RGL-modmask   S: 0000,0001,0000,0008
REGRESS  HERE Q: ADDI, X| R| BX| 4321 IL,  RGL-modmask   S: 0000,0008,0000,0008
REGRESS  HERE Q: CMPI, X| R| BX| 4321 IL,  RGL-modmask   S: 0000,0000,0000,0008
REGRESS DECIMAL S:

\ \ For  addr  return the  mod-mask  of the instruction there.
\ \ NONE means that it can't be handled.
\ : RGL-modmask-sz1 >R
\    \ MOVI|X, PUSH|X, POP|X,
\    R@ Qp@ OP1 AND IF
\         R@ get1-msk-QN
\         R> Qp@ #SN AND IF 0 ELSE 32 THEN
\         LSHIFT
\    ELSE
\        RDROP NONE   \ I can't deal beind more for now
\    THEN ;


\ : RGL-modmask-sz2 >R
\    R@ Qp@ OP2 AND IF
\         R@ get2-msk-QN  R@ get2-msk-Q'
\         R> Qp@ #SN AND 0= IF SWAP THEN  32 LSHIFT OR
\    ELSE
\        RDROP NONE   \ I can't deal beind more for now
\    THEN ;

: cnt-bits  0 BEGIN SWAP 2/ DUP WHILE SWAP 1+ REPEAT DROP ;
REGRESS 1 cnt-bits  8 cnt-bits S: 0 3

\ MACRO'S : R@ points to instruction address!
\ For   mask  set N-bit according to `mask, leave only mod/rm  mask  .
:I setbit-QN DUP 8 RSHIFT IF 8 RSHIFT 1 ELSE 0 THEN
    R@ C@  1 INVERT AND OR  R@ C!  ;

\ For   mask  set N-bit according to `mask, leave only mod/rm  mask  .
:I setbit-Q' DUP 8 RSHIFT IF 8 RSHIFT 1 ELSE 0 THEN
    R@ C@  2 INVERT AND OR  R@ C!  ;

\ For   mask  set N-reg according to `bitmask for size 1 instruction.
:I set1-msk-N    cnt-bits  7 AND R@ 1+ C@ 7 INVERT AND  OR R@ 1+ C! ;

\ For   mask  set N-reg according to `bitmask for size 2 instruction.
:I set2-msk-N    cnt-bits  7 AND R@ 2 + C@ 7 INVERT AND  OR R@ 2 + C! ;

\ For   mask  set '-reg according to `bitmask for size 2 instruction.
:I set2-msk-'    cnt-bits  7 AND 3 LSHIFT R@ 2 + C@ $38 INVERT AND  OR
    R@ 2 + C! ;

: \D POSTPONE \ ; IMMEDIATE
\ : \D ; IMMEDIATE
\ Change to source in  modmask  the destination for instruction at  addr  .
\ For now we assume that we have a one 86-byte onesrc instruction, i.e. PUSH|X,
: change-src1   >R  $0FFFF,FFFF AND
\D   ." BEFORE ___________________"
\D   R@ bufv$ @ - DUP 20 + D-R
   setbit-QN
\D .S
  set1-msk-N
\D   ." AFTER ___________________"
\D   R@ bufv$ @ - DUP 20 + D-R
\D   KEY DROP
  RDROP
;

: change-src2   >R  $0FFFF,FFFF AND
\D   ." BEFORE ___________________"
\D   R@ bufv$ @ - DUP 20 + D-R
   setbit-QN
\D  .S
  set2-msk-N
\D   ." AFTER ___________________"
\D   R@ bufv$ @ - DUP 20 + D-R
\D   KEY DROP
  RDROP
;

: change-src2'   >R  $0FFFF,FFFF AND
\D  ." BEFORE ___________________"
\D  R@ bufv$ @ - DUP 20 + D-R
   setbit-Q'
\D  .S
  set2-msk-'
\D   ." AFTER ___________________"
\D   R@ bufv$ @ - DUP 20 + D-R
\D   KEY DROP
  RDROP
;

\ \ Can handle only size 1 and size 2 instructions!
\ \  (modmask addr)
\ : change-src
\     DUP Qp@ SZ1 AND IF change-src1 ELSE
\         DUP Qp@ #SN AND IF change-src2 ELSE change-src2' THEN
\     THEN ;

\ Can handle only size 1 and size 2 instructions!
\  (modmask addr)
: change-src2-N-and-'
    DUP Qp@ #SN AND IF change-src2 ELSE change-src2' THEN ;

: .SH HEX: .S ;
: .SH ;
\ Given   pupoaddr mod-mask2  mod-mask1  .
\ If the dest of mod-mask1 is the same than the src of mod-mask2
\ replace the src of mod-mask2 by the source of mod-mask1.
\ Leave pupoaddr
\ FIXME : there are several things wrong here.
\  1. there may be two registers to be replaced
\  2. Any [XX] register is a source not a destination
\  3. First we try to find out whether the N-position or the '-position
\    are source or destination. The we try to find out where a replacemet
\   for the source has to go.
\ : dest-to-src1   .SH
\    OVER NONE = IF 2DROP EXIT THEN
\    2DUP 32 RSHIFT  AND IF    \ proposed: destinaton matches one of the src
\     \D ." the dest of mod-mask1 is the same than the src of mod-mask2 "
\      NIP   OVER bufv$ @ +   change-src1
\   ELSE 2DROP THEN
\ \D DUP DUP 20 + D-R
\ ;
\ : dest-to-src2   .SH
\    OVER NONE = IF 2DROP EXIT THEN
\    2DUP 32 RSHIFT  AND IF    \ proposed: destinaton matches one of the src
\     \D ." the dest of mod-mask1 is the same than the src of mod-mask2 "
\      NIP   OVER bufv$ @ +   change-src2-N-and-'
\   ELSE 2DROP THEN
\ \D DUP DUP 20 + D-R
\

$07,01 CONSTANT #mask1-QN
$07,00,01 CONSTANT #mask2-QN
$38,00,04 CONSTANT #mask2-Q'
: get-#mask1-QN    @ #mask1-QN AND ;
: set-#mask1-QN    >R R@ @ #mask1-QN INVERT AND OR R> ! ;
: get-#mask2-QN    @ #mask2-QN AND ;
: set-#mask2-QN    >R R@ @ #mask2-QN INVERT AND OR R> ! ;
: get-#mask2-Q'    @ #mask2-Q' AND ;
: set-#mask2-Q'    >R R@ @ #mask2-Q' INVERT AND OR R> ! ;

\ For   ^pupo  ^instr  patch a instruction L-size 1 and
\ leave  ^pupo  . Note that pupo is current with valid `start `bufv `bufc .
\ ( ^pupo ^ins2  -- ^pupo )
: handle-sz1 DROP
   \ MOVI|X, PUSH|X, POP|X,
   ins3 Qp@ OP1 AND IF
        ins2 get-#mask1-QN ins3 get-#mask1-QN = IF
            ins3 Qp@ #SN AND IF   \ Source, i.e. push
                    ins1 get-#mask1-QN ins3 set-#mask1-QN
             ELSE
                 0 bufc$ !
            THEN
        THEN
   THEN
;
\ FIXME : add tests here if matches fail.
: longer $100 /MOD $10000 * + ;
REGRESS  #mask1-QN longer S: #mask2-QN
: lefter $100 /MOD 3 LSHIFT SWAP 2 LSHIFT SWAP $100 * + ;
REGRESS  #mask2-QN lefter S: #mask2-Q'

\ ( ^pupo ^ins  -- ^pupo )
: handle-sz2  DROP
   ins3 Qp@ OP2 AND IF
        ins2 get-#mask1-QN longer ins3 get-#mask2-QN = IF  \ Propagation
            ins3 Qp@ #SN AND IF   \ Source,
                  ins1 get-#mask1-QN longer ins3 set-#mask2-QN
            THEN
        THEN
        ins2 get-#mask1-QN longer lefter ins3 get-#mask2-Q' = IF  \ Propagation
            ins3 Qp@ #S' AND IF   \ primed reg is source
                  ins1 get-#mask1-QN longer lefter ins3 set-#mask2-Q'
            THEN
        THEN
    THEN
   ins3 Qp@ IMM2 AND IF
        ins2 get-#mask1-QN longer ins3 get-#mask2-QN = IF  \ Propagation
            ins3 1+ ~DISASSEMBLE DROP
            LATEST-INSTRUCTION @ DUP 'CMPI, = SWAP 'TESTI, =   OR IF
                  ins1 get-#mask1-QN longer ins3 set-#mask2-QN
            THEN
        THEN
    THEN
;
REGRESS pupo-pattern S:
REGRESS HERE Q: PUSH|X, BX| Q: POP|X, DI| Q: ADD, X| F| DI'| R| CX| matches? S:  TRUE
REGRESS ins1 ins3 handle-sz2 ins3 ~DISASSEMBLE D-R S:
REGRESS HERE QN: PUSH|X, BX| Q: POP|X, DI| Q: ADD, X| F| DI'| R| CX| matches? S:  TRUE
REGRESS ins1 ins3 handle-sz2 ins3 ~DISASSEMBLE D-R S:
REGRESS ins1 ins3 ~DISASSEMBLE D-R S:
REGRESS HERE Q: PUSH|X, AX| Q: POP|X, BX| Q: CMPI, X| R| BX| 8190 IL, matches? S: TRUE
REGRESS ins1 ins3 handle-sz2 ins3 ~DISASSEMBLE D-R S:
REGRESS ins1 ins3 ~DISASSEMBLE D-R S:

\ : RGL-modmask  DROP NONE ;
\ ( ^pupo -- ^pupo )
: ?pupo-propagate?  \ all we need to know about the pupo
    DUP bufv$ @ +
    CONDS
        DUP C@ prop@ QE <> IF DROP ELSE          \ Unknown
        DUP Qp@ OP0 AND IF DROP ELSE     \ PUSHI
        DUP Qp@ SZ1 AND IF handle-sz1  ELSE
        DUP Qp@ SZ2 AND IF handle-sz2  ELSE
        DROP
    THENS
;
