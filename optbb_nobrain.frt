( Copyright{2020}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: optbb_nobrain.frt,v 5.18 2021/05/07 13:19:03 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.
\
\ This file contains the optimisations that need no consideration regards
\ circumstances because they are always better.
\
\ Examples are :
\          no operation
\          push pop of the same register
\          lea with zero offset
\ ----------------------------------------------------------------

\ Get rid of adjusting an effective address with zero.

<! !Q LEA, XO| 0 L, !!T !>
<A A>
{  }
optimisation lea0-pattern

\ Replace if normal and primed register are the same. Leave " was  replaced".
: ?lea0-replace? bufv find-reg-RGL = DUP IF replace THEN ;

REGRESS lea0-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS original 1+ matches? S:  FALSE
REGRESS  HERE Q: LEA, XO| [BP] BP'| 1 L, matches? S: FALSE
REGRESS  HERE Q: LEA, XO| [BX] DX'| 0 L, matches? ?lea0-replace? S: TRUE FALSE
REGRESS  HERE QN: LEA, XO| [BX] BX'| 0 L, matches? ?lea0-replace? S: TRUE FALSE
REGRESS  HERE QN: LEA, XO| [BP] BP'| 0 L, matches? ?lea0-replace? S: TRUE FALSE
REGRESS  HERE Q: LEA, XO| [BP] BP'| 0 L, matches? ?lea0-replace? S: TRUE TRUE
REGRESS bufc$ @ S: 0

\ ----------------------------------------------------------------

<! !Q PUSH|X, !!T  !Q !!T POP|X, !!T !>
<A A>
{  }
optimisation pupo-pattern

: pupo-get-mod  ins1 get-mod-mask   ins2 get-mod-mask 32 LSHIFT OR ;
\ Return  reg-from  reg-to  (after match).
: pupo-get-reg   ins1 get1-reg-QN    ins2 get1-reg-QN
\    "HERE pupo " TYPE pupo-get-mod H. CR ;
;

\ Return  modmask   reg-to  (after match).
: pupo-get-mask  ins1 get1-msk-QN  ins2 get1-msk-QN 32 LSHIFT OR ;
\    "HERE pupo " TYPE pupo-get-mod H. CR ;

\ At  addr  :" a push/pop pair  is  to be found with same registers."
: pupo-same-reg   pupo-get-reg   = ;
\ Optional replace, leave " was  replaced".
: ?pupo-replace? pupo-same-reg DUP IF replace THEN ;

REGRESS pupo-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS  HERE -1 {L,} matches? S: FALSE
REGRESS  HEX HERE Q: PUSH|X, BX|  Q: POP|X, CX| matches? pupo-get-mask S: TRUE 0000,0002,0000,0008
REGRESS  DECIMAL HERE Q: PUSH|X, SI|  Q: POP|X, SI| matches? pupo-same-reg  S: TRUE TRUE
REGRESS  HERE Q: PUSH|X, SI|  Q: POP|X, SI| matches? pupo-same-reg  S: TRUE TRUE
REGRESS  HERE QN: PUSH|X, SI|  Q: POP|X, SI| matches? pupo-same-reg  S: TRUE FALSE
REGRESS  HERE QN: PUSH|X, SI|  QN: POP|X, SI| matches? pupo-same-reg  S: TRUE TRUE
REGRESS  HEX pupo-get-mod  S: 4000,0000,4000
REGRESS DECIMAL ?pupo-replace? bufc$ @ S: TRUE 0

\ ---------------------------------------------------
\ Remove nop.

<! XCHG|AX, AX| !!T !>
<A A>
{  }
optimisation xchg-aa-pattern

REGRESS xchg-aa-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS replace bufc$ @ S: 0

\ ---------------------------------------------------
\ Remove exchange reg with itself, another nop.

<! Q: !!T XCHG, X| R| !!T !>
<A A>
{  }
optimisation xchg-pattern

REGRESS xchg-pattern DUMPO S:
REGRESS original matches? S: TRUE

\ At  addr  :" an exchange is  to be found with itself."
: xchg-same-reg bufv 2 + C@ DUP 3 RSHIFT XOR 7 AND 0= ;
REGRESS HERE Q: XCHG, X| R| AX'| BX| matches? xchg-same-reg S: TRUE FALSE
REGRESS HERE Q: XCHG, X| R| BX'| BX| matches? xchg-same-reg S: TRUE TRUE
\ Optional replace, leave " was  replaced".
:  ?xchg-replace? xchg-same-reg DUP IF replace THEN ;
REGRESS ?xchg-replace? bufc$ @ S: TRUE 0

\ ----------------------------------------------------------------
\ Combining push and pop's.
<!
   !Q PUSHI|X, !!T  0 {L,}  ~!!T    \ 5  FF 00 00 00 00
   !Q POP|X,              !!T    \ 1  F0
   !>
<A QX: !TALLY   MOVI|X, 0 IL,  !TALLY   NEXT A>
{  starts 4 [] 8 <> 3018 ?ERROR
   starts 1 []   starts 3 [] + find-reg-RGL   NONE <> 3019 ?ERROR
   DUP 3 RSHIFT  bufc OR!U       7 AND        bufc 1 + OR!U
   bufv 2 + L@  bufc 2 + L! }
optimisation pushi-pattern
REGRESS DUMPO S:
REGRESS original 1+ matches? S: FALSE
REGRESS original matches? S: TRUE
REGRESS HERE Q: PUSHI|X, 1234 IL, Q: POP|X, DX| matches? bufv$ @ S: TRUE 8
REGRESS replace bufc$ @ S: 6
REGRESS bufc$ $@ .code S:
REGRESS HERE Q: PUSHI|X, 1234 IL, QN: POP|X, DX| matches? bufv$ @ S: TRUE 8
REGRESS replace bufc$ @ S: 6
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Remove superfluous movi to register.
\ Note that this must be done before trying to commute!

<! !Q! MOVI|X, !!T  0 {L,}  ~!!T   !Q! POP|X, !!T  !>
<A  Q: POP|X, !TALLY NEXT A>
{  bufv 7 + C@ 7 AND  bufc 1 + OR!U }
optimisation movipop-pattern
REGRESS DUMPO S:
REGRESS original 1+ matches? S:  FALSE
REGRESS original matches? S:  TRUE

\  :" it  is  all the same register."
: movipop-same bufv 1+ C@ bufv 7 + C@ XOR 7 AND 0= ;

\ Optional replace, leave " was  replaced".
: ?movipop-replace? movipop-same DUP IF replace THEN ;
REGRESS HERE Q: MOVI|X, BX| 0 IL, Q: POP|X, AX| matches? movipop-same S: TRUE FALSE
REGRESS HERE Q: MOVI|X, BX| 0 IL, Q: POP|X, BX| matches? movipop-same S: TRUE TRUE
REGRESS replace bufc$ @ S: 2
REGRESS HERE Q: MOVI|X, SI| 0 IL, Q: POP|X, SI| matches? movipop-same S: TRUE TRUE
REGRESS ?movipop-replace? bufv$ @ S: TRUE 8
REGRESS bufc$ $@ .code bufc$ @ S: 2
