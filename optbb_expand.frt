( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_expand.frt,v 5.21 2021/05/07 13:19:03 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.
\
\ This file contains uniformisation of the code, so as to minimize the set
\ of instructions the peephole optimiser has to deal with.
\         A replace byte with xell in data and indices
\         B replace short branches with long
\         C replace one byte instruction reg A with regular instruction
\         D replace 32 bit with 64 bits
\         E replace T| R| by F| R|  where possible
\         F get Q: before all pushes and pops.
\         G replace SUB or XOR that zeroes a register by MOVI.
\         H replace register register moves by pushes and pops.

\ ----------------------------------------------------------------

\ Replace a byte operand with a xell operand.
\ This replacement is special because it works for all opcodes.
<! !Q! XXX, BO| !!T   $00 C, ~!!T !>
<A Q: ADD, XO| 0 L, !TALLY A>
{   bufv 1+ L@  $FFFF AND   bufc 1+ OR!U
    bufv 3 + C@   C>S bufc 3 + L! }
optimisation oprboxo-pattern
REGRESS oprboxo-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS HERE Q: ADD, X| F| BX'| BO| [BP] 0 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 0
REGRESS HERE Q: CMP, X| F| BX'| BO| [BP] -8 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 -8 $FF AND
REGRESS HERE Q: CMP, X| T| SI'| BO| [BP] -8 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 -8 $FF AND
REGRESS HERE Q: CMP, B| T| DL'| BO| [DX] 8 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 8
REGRESS  replace bufc$ @ S: 7
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Replace byte offset for BP with xell offset.

<! !Q! LEA, BP'| BO| [BP]  !!T   $00 C, ~!!T !>
<A Q: LEA, BP'| XO| [BP]  0 L, A>
 {   bufv 3 + C@   C>S bufc 3 + L! }
optimisation leaboxo-pattern

REGRESS leaboxo-pattern DUMPO S:
REGRESS HERE Q: LEA, BP'| BO| [BP] 8 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 8
REGRESS HERE Q: LEA, BP'| BO| [BP] -8 B, matches? bufv$ @ bufv 3 + C@ S: TRUE 4 -8 $FF AND
REGRESS  replace bufc$ @ S: 7
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Replace byte offset for MOV, with xell offset.

<! !Q! MOV, BO| !!T 0 C, ~!!T !>
<A  Q: MOV, B| F| AL'| XO| [AX] 0 L, A>
{   bufv @ $FF,FFFF AND   bufc OR!U
    bufv 3 + C@   C>S bufc 3 + L! }
optimisation movboxo-pattern

REGRESS movboxo-pattern S:
REGRESS HERE Q: MOV, B| F| AL'| BO| [AX] 7 B, matches? bufv$ @ S: TRUE 4
REGRESS HERE Q: MOV, X| T| AX'| BO| [AX] 7 B, matches? bufv$ @ S: TRUE 4
REGRESS HERE Q: MOV, X| T| DX'| BO| [SI] 7 B, matches? bufv$ @ S: TRUE 4
REGRESS replace bufv$ @ S: 4
REGRESS bufc$ @ bufc$ $@ .code S: 7

\ ----------------------------------------------------------------
\ Replace zero offset for MOV, with xell offset.

<! !Q! MOV, ZO| !!T !>
<A  Q: MOV, B| F| AL'| XO| [AX] 0 L, A>
{   bufv 2 + $80 TOGGLE ( mode: BO -> XO )
    bufv @ $FF,FFFF AND   bufc OR!U
    0 bufc 3 + L! }
optimisation movzoxo-pattern

REGRESS movzoxo-pattern S:
REGRESS HERE Q: MOV, B| F| AL'| ZO| [AX] matches? bufv$ @ S: TRUE 3
REGRESS HERE Q: MOV, X| T| AX'| ZO| [AX] matches? bufv$ @ S: TRUE 3
REGRESS HERE Q: MOV, X| T| DX'| ZO| [SI] matches? bufv$ @ S: TRUE 3
REGRESS replace bufv$ @ S: 3
REGRESS bufc$ @ bufc$ $@ .code S: 7

\ --------------------------------------------------------------------
<! !Q MOV, X| R| F| !!T !>
<A QX: !TALLY PUSH|X, !TALLY  QX: !TALLY POP|X, !TALLY A>
{ starts 3 [] 3 <>   3018 ?ERROR
   find-reg1 DUP NONE = 3019 ?ERROR
   \ Normal register dest, to be popped
   OVER 3 RSHIFT bufc 2 + OR!U    OVER 7 AND    bufc 3 + OR!U
   \ Primed register src, to be pushed
   DUP  3 RSHIFT bufc OR!U    DUP  7 AND bufc 1 + OR!U
   2DROP }
optimisation movrfpupo-pattern

REGRESS movrfpupo-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS  original 1+ matches? S: FALSE
REGRESS  HERE QN: MOV, X| R| T| SI'|  DI| matches? S: FALSE
REGRESS  HERE QN: MOV, X| R| F| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS  HERE Q': MOV, X| R| F| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS  HERE QN': MOV, X| R| F| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS replace bufc$ @ S: 4
REGRESS bufc$ $@ .code S:
REGRESS  HERE Q': MOV, X| R| F| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS replace bufc$ @ S: 4
REGRESS bufc$ $@ .code S:
\ ----------------------------------------------------------------
\ --------------------------------------------------------------------
<! !Q MOV, X| R| T| !!T !>
<A QX: !TALLY PUSH|X, !TALLY  QX: !TALLY POP|X, !TALLY A>
{ starts 3 [] 3 <>   3018 ?ERROR
   find-reg1 DUP NONE = 3019 ?ERROR
   \ Primed register dest, to be popped
   DUP 3 RSHIFT bufc 2 + OR!U    DUP 7 AND    bufc 3 + OR!U
   \ Normal register src, to be pushed
   OVER  3 RSHIFT bufc OR!U    OVER  7 AND bufc 1 + OR!U
   2DROP }
optimisation movrtpupo-pattern

REGRESS movrtpupo-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS  original 1+ matches? S: FALSE
REGRESS  HERE QN: MOV, X| R| F| SI'|  DI| matches? S: FALSE
REGRESS  HERE QN: MOV, X| R| T| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS  HERE Q': MOV, X| R| T| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS  HERE QN': MOV, X| R| T| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS replace bufc$ @ S: 4
REGRESS bufc$ $@ .code S:
REGRESS  HERE Q': MOV, X| R| T| SI'|  DI| matches? bufv$ @ S: TRUE 3
REGRESS replace bufc$ @ S: 4
REGRESS bufc$ $@ .code S:
\ ----------------------------------------------------------------
\ ----------------------------------------------------------------
\ Optimise XOR R R to  MOVI R 0
\ FIXME: there must be a check whether setting the flags can be ignored,
\ e.g. if some operation follows before the end jump or carry-dependant
\ instruction.

<!  !Q XOR, X| R|  !!T !>
<A  Q: MOVI|X, 0 IL,  !TALLY A>
{ bufv get2-reg-QN  bufc put1-reg-QN }
optimisation xormovi-pattern
REGRESS xormovi-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: XOR, X| F| AX'| R| DI| Q: NOP, matches? S: TRUE

\  :" xor  intends to zero the register"
: xormovi-okay   bufv DUP get2-reg-QN SWAP get2-reg-Q' = ;

REGRESS HERE Q: XOR, X| F| AX'| R| DI| Q: NOP, matches? xormovi-okay   S: TRUE FALSE
REGRESS HERE Q: XOR, X| T| DI'| R| DI| Q: NOP, matches? xormovi-okay   S: TRUE TRUE
REGRESS HERE QN': XOR, X| T| DI'| R| DI| Q: NOP, matches? xormovi-okay   S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?xormovi-replace? xormovi-okay DUP IF replace THEN ;
REGRESS HERE QN': XOR, X| T| DI'| R| DI| matches? xormovi-okay   S: TRUE TRUE
REGRESS  ?xormovi-replace? bufv$ @ S: TRUE 3
REGRESS bufc$ $@ .code bufc$ @ S: 6
\ ----------------------------------------------------------------
\ The code at  addr  :"  needs  a Q-prefix"
: Q-needed?  DUP C@ prop@ QE AND IF DROP FALSE ELSE
    C@ prop@ REGULAR AND 0<> THEN ;
REGRESS HERE POP|X, BX| Q-needed? S: TRUE
REGRESS HERE Q: POP|X, BX| Q-needed? S: FALSE
REGRESS HERE Q: LEA, BP'| XO| [BP]  0 L, Q-needed? S: FALSE
REGRESS HERE LEA, BP'| XO| [BP]  0 L, Q-needed? S: TRUE

: (next-gap-expand)
  CONDS
     movzoxo-pattern DUP matches? IF replace TRUE ELSE
     movboxo-pattern DUP matches? IF replace TRUE ELSE
     leaboxo-pattern DUP matches? IF replace TRUE ELSE
     oprboxo-pattern DUP matches? IF replace TRUE ELSE
     movrfpupo-pattern DUP matches? IF replace TRUE ELSE
     movrtpupo-pattern DUP matches? IF replace TRUE ELSE
     xormovi-pattern DUP matches? IF ?xormovi-replace? ELSE
        FALSE
  THENS
;
