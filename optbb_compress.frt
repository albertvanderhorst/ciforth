( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_compress.frt,v 5.23 2021/05/07 13:19:03 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.
\
\ This file contains futile optimisations, that replace instructions with
\ shorter equivalents. This is the very last step before combining basic
\ blocks at there definitive place.
\ Note that this is a single step! There can be no reliance upon
\ building up resulting smaller code.
\ Examples are
\         replace long branches with short ones
\         replace a long offset by a zero or byte offset
\         remove superfluous prefixes.
\         remove superfluous operations, add zero, lea with offset zero.
\         replace regular instruction with a reg A specific one

\ ---------------------------------------------------------------------------
\ A movi reg, is up till now during optimisation has a 32 bit value.
\ That works as long as there is no ghost register, because the Q:
\ prefix is removed. For those cases where the immediate value must be 64 bit,
\ this expansion is needed in the compression phase.

<! !Q  MOVI|X, !!T  0 {L,} ~!!T !>
<A  Q: MOVI|X, 0 , !TALLY A>
{ bufv 2 + L@  L>S bufc 2 + !
  bufv C@  bufc OR!U
  bufv 1+ C@  bufc 1+ OR!U
}
optimisation movimoviq-pattern
REGRESS movimoviq-pattern  DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, matches? S: TRUE

\  :" move  is  needs 64bits data"
: movimoviq-okay   bufv get1-reg-QN 7 > ;
REGRESS HERE Q: MOVI|X, DX| 1 IL, matches? movimoviq-okay   S: TRUE FALSE
REGRESS HERE Q: MOVI|X, AX| 0 IL, matches? movimoviq-okay   S: TRUE FALSE
REGRESS HERE QN: MOVI|X, AX| 0 IL, 0 {L,} matches? movimoviq-okay   S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?movimoviq-replace? movimoviq-okay DUP IF replace THEN ;
REGRESS HERE QN: MOVI|X, DI| 1234 IL, matches? S: TRUE
REGRESS  ?movimoviq-replace? bufv$ @ S: TRUE 6
REGRESS bufc$ $@ .code bufc$ @ S: 10

  \ ----------------------------------------------------------------
\ Replace xell offset for MOV, with byte offset.
\ This is storing a fixed value at a fixed address: e.g. DECIMAL

<! !Q MOVI, X| XO| !!T 0 {L,} 0 {L,} ~!!T !>
<A  QX: !TALLY MOVI, X| BO| 0 B, 0 IL, !TALLY A>
{   \ bufv 2 + $40 TOGGLE ( mode: BO )
    bufv @ $FF,FFFF AND   bufc OR!U
    bufv 3 + L@   bufc 3 + C!
    bufv 7 + L@   bufc 4 + L! }
optimisation movixobo-pattern

REGRESS  movixobo-pattern original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE

\ At  addr  :" a movxo  is  to be found that can be simplified"
: movixobo-simpler bufv 3 + L@  L>S fits-in-byte ;

\ Optional replace, leave " was  replaced".
:  ?movixobo-replace? movixobo-simpler DUP IF replace THEN ;

REGRESS HERE Q: MOV, B| F| AL'| XO| [AX] 1234 L, matches? S: FALSE
REGRESS HERE Q: MOVI, X| XO| [AX] 7 L, 1234 IL,  matches?  movixobo-simpler S: TRUE  TRUE
REGRESS HERE QN: MOVI, X| XO| [DX] -7 L, 18 IL, matches?  movixobo-simpler S: TRUE  TRUE
REGRESS HERE QN: MOVI, X| XO| [DX] 7 L, 18 IL, matches?  movixobo-simpler S: TRUE  TRUE
REGRESS ?movixobo-replace? bufv$ @ S: TRUE 11
REGRESS bufc$ @ bufc$ $@ .code S: 8

\ ----------------------------------------------------------------
\ Replace a xell offset with a byte offset.
\ Due to XXX, a don't care operations all opcodes match.
\ XXX, is like an operation such as ADD, but doesn't set the $38 bits as filled.
<! !Q XXX, XO| !!T  $00 {L,} ~!!T !>
<A QX: !TALLY XXX, BO| 0 C, !TALLY A>
{   bufv L@  $FFFFFF AND   bufc OR!U
    bufv 3 + L@   bufc 3 + C! }
optimisation oprxobo-pattern

\  :" return strack  is  decremented."
: oprxobo-small-enough bufv 3 + L@  L>S fits-in-byte ;
\ Optional replace, leave " was  replaced".
: ?oprxobo-replace? oprxobo-small-enough DUP IF replace THEN ;

REGRESS oprxobo-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS HERE Q: ADD, X| F| BX'| XO| [BP] 0 L, matches? bufv$ @ bufv 3 + L@ S: TRUE 7 0
REGRESS HERE Q: CMP, X| F| BX'| XO| [BP] -8 L, matches? bufv$ @ bufv 3 + L@ S: TRUE 7 -8 $FFFF,FFFF AND
REGRESS HERE Q: CMP, B| T| DL'| XO| [DX] 8 L, matches? bufv$ @ bufv 3 + L@ S: TRUE 7 8
REGRESS  ?oprxobo-replace? S: TRUE
REGRESS HERE QN': CMP, X| T| SI'| XO| [BP] -8 L, matches? bufv$ @ bufv 3 + L@ S: TRUE 7 -8 $FFFF,FFFF AND
REGRESS  ?oprxobo-replace? bufv$ @ S: TRUE 7
REGRESS bufc$ @ bufc$ $@ .code S: 4

\ --------------------------------------------------------------------
<! !Q PUSH|X, !!T  !Q POP|X, !!T  !>
<A QX: !TALLY MOV, X| R| F| !TALLY A>
{  starts 4 [] 4 <> 3018 ?ERROR
   find-reg1 NONE <> 3019 ?ERROR
   find-reg2 NONE <> 3019 ?ERROR
   bufc put2-reg-QN' }
optimisation pupomovr-pattern

REGRESS pupomovr-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS  original 1+ matches? S: FALSE
REGRESS  HERE Q: PUSH|X, SI|  Q: POP|X, DI| matches? bufv$ @ S: TRUE 4
REGRESS replace bufc$ @ S: 3
REGRESS bufc$ $@ .code S:
REGRESS  HERE QN: PUSH|X, SI|  QN: POP|X, DI| matches? bufv$ @ S: TRUE 4
REGRESS replace bufc$ @ S: 3
REGRESS bufc$ $@ .code S:
REGRESS  HERE QN: PUSH|X, SI|  Q: POP|X, DI| matches? bufv$ @ S: TRUE 4
REGRESS replace bufc$ @ S: 3
REGRESS bufc$ $@ .code S:

\ The code at  addr  :"  doesnot  need a Q-prefix"
\ So it has one, and it is a one byte regular instruction or a lea.
: Q-unneeded?  DUP C@ $48 ( Q:) <> IF DROP FALSE ELSE
    1+ DUP C@ $8D ( LEA) = IF DROP TRUE ELSE
      C@ prop@   DUP REGULAR AND 0<>    SWAP SZ1 AND 0<>   AND
    THEN THEN ;

REGRESS HERE Q: POP|X, BX| Q-unneeded? S: TRUE
REGRESS HERE POP|X, BX| Q-unneeded? S: FALSE
REGRESS HERE Q: PUSHI|X, 1234 IL, Q-unneeded? S: TRUE
REGRESS HERE PUSHI|X, 1234 IL, Q-unneeded? S: FALSE
REGRESS HERE Q: LEA, BP'| XO| [BP]  0 L, Q-unneeded? S: TRUE
REGRESS HERE LEA, BP'| XO| [BP]  0 L, Q-unneeded? S: FALSE
REGRESS HERE Q: CMPI, X| R| BX| 8190 IL, Q-unneeded? S: FALSE


: (next-gap-compress)
  CONDS
     pupomovr-pattern DUP matches? IF replace TRUE ELSE
     movixobo-pattern DUP matches? IF ?movixobo-replace? ELSE
     oprxobo-pattern DUP matches? IF ?oprxobo-replace? ELSE
     movimoviq-pattern DUP matches? IF ?movimoviq-replace? ELSE
     FALSE
  THENS
;
