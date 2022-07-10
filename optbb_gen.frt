( Copyright{2020}: Albert van der Horst, FIG Holland by GNU Public License 2)
( $Id: optbb_gen.frt,v 5.46 2021/05/07 13:19:03 albert Exp $)

\ This is part of the optimiser for ciforth in particular optimising basic
\ blocks. See the comment in optimisebblock.frt.
\
\ This file contains all the regular peephole optimisations.
\ It can work on a restricted set,
\        only XO| , no BO| or ZO| ,
\        Q: for 64 bit operands where not needed
\ thanks to optbb_expand.frt and then optbb_compress.frt.
\ This contains only changes that count, for
\ trivial changes see optbb_nobrain.frt

\ ----------------------------------------------------------------
\ Move combine LEA for same register.

<! !Q LEA, XO| !!T 0 {L,} ~!!T
    !Q LEA, XO| !!T 0 {L,} ~!!T !>
<A  QX: !TALLY LEA, XO| 0 L,  !TALLY A>
{    bufv L@  $FFFFFF AND bufc OR!U
     bufv 3 + L@ bufv 10 + L@ + bufc 3 + L! }
optimisation lealea-pattern

REGRESS lealea-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS HERE Q: LEA, BP'| XO| [BP] -16 L, Q: LEA, BP'| XO| [BP] +24 L, matches? S: TRUE
REGRESS HERE Q: LEA, BP'| XO| [BP] -16 L, Q: POP|X, BP| matches? S: FALSE

\  :" it  is  all the same register."
: lealea-same
    ins1 DUP get2-reg-QN SWAP get2-reg-Q' <> IF FALSE EXIT THEN
    ins1 @ ins2 @ XOR $FFFFFF AND 0= ;
REGRESS HERE QN: LEA, BP'| XO| [BP] -16 L, QN: LEA, BP'| XO| [BP] +24 L, matches? lealea-same    S: TRUE FALSE
REGRESS HERE Q: LEA, BP'| XO| [DI] -16 L, Q: LEA, BP'| XO| [BP] +24 L, matches? lealea-same    S: TRUE FALSE
REGRESS HERE QN': LEA, BP'| XO| [BP] -16 L, QN': LEA, BP'| XO| [BP] +24 L, matches? lealea-same    S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?lealea-replace? lealea-same DUP IF replace THEN ;

REGRESS lealea-same S: TRUE
REGRESS  ?lealea-replace? bufc$ @ bufc 3 + L@ S: TRUE 7 8
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Move decrementing BP to the front, through non-commuting usage.
\ Note that |T |F doesn't matter.
\ This combines with ?commute-rsp? that just commutes BP to the front.
\ Ghost registers will match, awaiting usage of frame register.
<! !Q MOV, XO| [BP] !!T 0 {L,} ~!!T    \ Matches BP's ghost too!
    !Q LEA, BP'| XO| [BP] !!T 0 {L,} ~!!T !>
<A  QX: !TALLY LEA, BP'| XO| [BP] 0 L,
    QX: !TALLY MOV, XO| [BP] 0 L, !TALLY A>
{   bufv L@ $FF,FFFF AND  bufc 7 + OR!U  \ Copies B| X| F| T| R| xX'|
    bufv 7 + L@ $FFFFFF AND  bufc OR!U  \ Copies B| X| F| T| R| xX'|
    bufv 3 + L@ bufv 10 + L@ - bufc 10 + L!
    bufv 10 + L@ bufc 3 + L! }
optimisation movrspxo-pattern

REGRESS movrspxo-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS HERE Q: MOV, X| F| BX'| XO| [BP] 0 L, Q: LEA, BP'| XO| [BP] -8 L, matches? S: TRUE
REGRESS HERE QN: MOV, X| F| BX'| XO| [BP] 0 L, QN: LEA, BP'| XO| [BP] -8 L, matches? S: TRUE

\  :" return stack  is  decremented." Both ghost or both not ghost.
: movrspxo-negative bufv 10 + L@ L>S 0<  ins1 C@ ins2 C@ = AND ;
REGRESS movrspxo-negative S: TRUE

\ Optional replace, leave " was  replaced".
: ?movrspxo-replace? movrspxo-negative DUP IF replace THEN ;

REGRESS  ?movrspxo-replace? bufc$ @ S: TRUE 14
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Move incrementing BP to the end, through non-commuting usage.
\ Note that |T |F doesn't matter.
\ ??? is combines with ?commute-rsp? that just commutes BP to the front.
\ Ghost registers will match, awaiting usage of frame register.
<! !Q LEA, BP'| XO| [BP] !!T 0 {L,} ~!!T
    !Q MOV, XO| [BP] !!T 0 {L,} ~!!T !>
<A QX: !TALLY MOV, XO| [BP] 0 L, !TALLY
   QX: !TALLY LEA, BP'| XO| [BP] 0 L, A>
{   bufv L@ $FF,FFFF AND  bufc 7 + OR!U  \ Copies B| X| F| T| R| xX'|
    bufv 7 + L@ $FFFFFF AND  bufc OR!U  \ Copies B| X| F| T| R| xX'|
    bufv 3 + L@ bufv 10 + L@ + bufc 3 + L!
    bufv 3 + L@ bufc 10 + L! }
optimisation movxorsp-pattern

REGRESS movxorsp-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS HERE Q: MOV, X| F| BX'| XO| [BP] 0 L, Q: LEA, BP'| XO| [BP] -8 L, matches? S: FALSE
REGRESS HERE Q: LEA, BP'| XO| [BP] 8 L, Q: MOV, X| F| BX'| XO| [BP] 0 L, matches? S: TRUE
REGRESS HERE QN: LEA, BP'| XO| [BP] 16 L, QN: MOV, X| F| BX'| XO| [BP] 24 L, matches? S: TRUE

\  :" return stack is decremented  and  both ghost or both not ghost."
: movxorsp-positive bufv 3 + L@ L>S 0 > ins1 C@ ins2 C@ = AND ;
REGRESS movxorsp-positive S: TRUE

\ Optional replace, leave "  was  replaced".
: ?movxorsp-replace? movxorsp-positive DUP IF replace THEN ;

REGRESS  ?movxorsp-replace? bufc$ @ S: TRUE 14
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Introduce lea instead of movi
\ FIXME! This doesn't play well with carry flag and is disabled.
<!  !Q! MOVI|X, !!T  0 {L,} ~!!T  !Q!  ADD, X| F| R| !!T !>
<A Q: LEA, XO| 0 L,       !TALLY NEXT A>
{    bufv 2 + L@  bufc 3 + L!
    bufv 1 + C@  3 LSHIFT bufc 2 + OR!U   \ Fill in target register
    bufv 8  + C@ 3 RSHIFT bufc 2 + OR!U   3020 THROW }
optimisation movilea-pattern
REGRESS movilea-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE MOVI|X, BX| 1234 IL, MOVI|X, BX| 1234 IL, matches? S: FALSE

\ :"the movi fills a reg that  is  the same as the add dest"
: movilea-same   bufv 1 + C@ bufv 8 + C@ XOR 7 AND 0= ;
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: ADD, X| F| AX'| R| BX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: ADD, X| F| BX'| R| SI| matches? S: TRUE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: ADD, X| F| AX'| R| CX|    matches? movilea-same   S: TRUE FALSE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: ADD, X| F| CX'| R| DX|  matches? movilea-same   S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?movilea-replace? movilea-same DUP IF replace THEN ;
\ REGRESS  ?movilea-replace? bufv$ @ S: TRUE 9
\ REGRESS bufc$ @ bufc$ $@ .code S: 7

\ ----------------------------------------------------------------
\ ----------------------------------------------------------------
\ This pattern removes a pupo if the destination register is filled next
\ and not otherwise needed.

\ There is no pupo-nihil-pattern , pupo-pattern from optbb_nobrain suffices.

\ For   inst  :"A possible source  register  is the same as the
\ destination register"
: pupo-nihil-same?  DUP get2-reg-QN SWAP get2-reg-Q'  =  ;

\ For  instruction  return the  reg  that is destroyed there, or NONE .
: destroyed  >R
    CONDS
    \ Don't worry, if this is the last instruction puponope will get it.
    R@ stopper @ >= IF NONE ELSE
    R@ 1+ ~DISASSEMBLE DROP LATEST-INSTRUCTION @
    DUP 'POP|X, = IF DROP  R@ get1-reg-QN ELSE
    DUP 'MOVI|X, = IF DROP  R@ get1-reg-QN ELSE
    \ If a LEA is based on the register itself, it is not a destroyer.
    DUP 'LEA, = R@ pupo-nihil-same? NOT? AND IF DROP R@ get2-reg-Q' ELSE
    DUP 'MOV, = R@ pupo-nihil-same? NOT? AND IF DROP
        R@ Qp@ #DD AND
        IF R@ get2-reg-Q'  ELSE R@ get2-reg-QN  THEN
    ELSE
    DROP NONE
    THENS RDROP ;
REGRESS  HERE Q: POP|X, AX|  destroyed  S: 0
REGRESS  HERE Q: ADDI, X| R| BX|  12 IL, destroyed  S: NONE
REGRESS  HERE  QN: POP|X, SI|  destroyed  S: 14
REGRESS  HERE Q: MOVI|X, AX| 12 IL,  destroyed  S: 0
REGRESS  HERE  QN: MOVI|X, SI| 12 IL,  destroyed  S: 14
REGRESS  HERE Q: LEA, AX'| REL| 12 (RL,)   destroyed  S: 0
REGRESS  HERE  Q': LEA, SI'| REL| 12 (RL,)  destroyed  S: 14
REGRESS  HERE  Q': LEA, SI'| XO| [AX] 12 L, DUP pupo-nihil-same? SWAP destroyed  S: FALSE 14
REGRESS  HERE  Q: LEA, SI'| XO| [SI] 12 L, DUP pupo-nihil-same? SWAP destroyed  S: TRUE NONE
REGRESS  HERE Q: MOV, X| T| AX'| R| AX|  DUP pupo-nihil-same? SWAP destroyed  S: TRUE NONE
REGRESS  HERE Q: MOV, X| T| AX'| R| BX|  DUP pupo-nihil-same? SWAP destroyed  S: FALSE 0
REGRESS  HERE  Q': MOV, X| T| SI'| R| BX|  destroyed  S: 14
REGRESS  HERE Q: MOV, X| F| AX'| R| BX|  destroyed  S: 3
REGRESS  HERE  Q': MOV, X| F| SI'| R| BX|  destroyed  S: 3

\ Leave: "pupo  is  to be annihiliated"
\ : pupo-nihil?  ins2 get1-reg-QN  DUP ins3 destroyed  =  SWAP
\     ins1 get1-reg-QN  = OR ;
: pupo-nihil?  ins2 get1-reg-QN  ins3 destroyed  =  ;

\ Optional replace, leave " was  replaced".
\ A check for permanent registers is superfluous: destroyed is destroyed.
\ : ?pupo-nihil-replace?
\     ins2 get1-reg-QN permanent-registers BAG-WHERE IF FALSE ELSE
\         pupo-nihil? DUP IF replace THEN
\     THEN ;
: ?pupo-nihil-replace? pupo-nihil? DUP IF replace THEN ;

REGRESS pupo-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS  HERE -1 {L,} matches? S: FALSE
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| Q: POP|X, AX| matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| QN: POP|X, SI| matches? pupo-nihil?  S: TRUE TRUE
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| Q: MOVI|X, AX| 12 IL, matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| QN: MOVI|X, SI| 12 IL, matches? pupo-nihil?  S: TRUE TRUE
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| Q: LEA, AX'| REL| 12 (RL,)  matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| Q': LEA, SI'| REL| 12 (RL,) matches? pupo-nihil?  S: TRUE TRUE
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| Q: MOV, X| T| AX'| R| BX| matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| Q': MOV, X| T| SI'| R| BX| matches? pupo-nihil?  S: TRUE TRUE
REGRESS  HERE Q: PUSH|X, BX|  Q: POP|X, CX| Q: MOV, X| F| AX'| R| BX| matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| Q': MOV, X| F| SI'| R| BX| matches? pupo-nihil?  S: TRUE FALSE
REGRESS  HERE Q: PUSH|X, SI|  QN: POP|X, SI| Q': MOV, X| T| SI'| R| BX| matches? pupo-nihil?  S: TRUE TRUE
REGRESS ?pupo-nihil-replace? bufc$ @ S: TRUE 0

\ ----------------------------------------------------------------
\ Detects  push r1;pop r2;r1:= ; sequence. To r1<->r2;r2=
\ DEACTIVATED!

<! !Q PUSH|X, !!T  !Q POP|X, !!T   !Q MOVI|X, !!T 0 {L,} ~!!T !>
<A   Q: XCHG, X| R| AX| AX'|   Q: MOVI|X, AX| 0 IL,  A>
{    bufv 6 + L@ bufc 5 + L!
    find-reg1 DROP   find-reg2 DROP bufc put-regs
    find-reg1 DROP DUP 7 AND  bufc 4 + OR!U   3 RSHIFT bufc 3 + OR!U }
optimisation pupomovi-pattern

REGRESS  pupomovi-pattern original matches? S: TRUE

\ : " a push/pop-pair-then-move  has  matching registers."
: pupomovi-aba find-reg1 DROP DUP   find-reg2 DROP <> SWAP
     find-reg3 DROP = AND ;
\ Optional replace, leave "It  was  replaced".
: ?pupomovi-replace? pupomovi-aba DUP IF replace THEN ;
REGRESS  HERE Q: PUSH|X, BX| Q: POP|X, DX| Q: MOVI|X, DI| 0 IL, matches? S: TRUE
REGRESS  ?pupomovi-replace? S: FALSE
REGRESS  HERE Q: PUSH|X, BX| Q: POP|X, DX| Q: MOVI|X, BX| 1234 IL, matches? S: TRUE
REGRESS  ?pupomovi-replace? S: TRUE
REGRESS bufc$ $@ .code S:
REGRESS  HERE QN: PUSH|X, BX| QN: POP|X, DX| QN: MOVI|X, BX| 1234 IL, matches? S: TRUE
REGRESS  ?pupomovi-replace? S: TRUE
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Detects  push r1;pop r2;pop r1 ; sequence. Eliminates pupo, for an exchange.
\ DEACTIVATED!

<! !Q! PUSH|X, !!T  !Q! POP|X, !!T   !Q! POP|X, !!T !>
<A Q: XCHG, X| R| !TALLY   Q: POP|X, !TALLY   NEXT A>
{   bufv 5 + C@ bufc 4 +  OR!U
    bufv 1+ C@ bufv 3 + C@ 3 LSHIFT OR  bufc 2 + OR!U }
optimisation pupopo-pattern
REGRESS pupopo-pattern DUMPO S:
REGRESS  original matches? S: TRUE
REGRESS HERE Q: PUSH|X, SI| Q: POP|X, AX| Q: POP|X, BX| matches? S: TRUE

\ :"The register  match  for this replacement."
: match-aba bufv 1 + C@ bufv 5 + C@ = ;
REGRESS match-aba S: FALSE
REGRESS HERE Q: PUSH|X, SI| Q: POP|X, BX| Q: POP|X, SI| matches? S: TRUE
REGRESS match-aba S: TRUE

\ Optional replace, leave " was  replaced".
: ?pupopo-replace? match-aba DUP IF replace THEN ;
REGRESS ?pupopo-replace? bufc$ @ S: TRUE 5
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Replaces the way conditions are normally used in Forth by an
\ efficient code sequence.

<!
   SET,         R| AL|   !!T   \ 3  FF F0 FF
   !Q! MOVZX|B, AL'| R| AL|  !!T   \ 3  FF FF FF
   !Q! NEG, X| R| AX|        !!T   \ 2  FF FF
   !Q! TEST, X| AX'| R| AX| !!T  \ 2  FF FF
   J|X, Z| Y|            !!T   \ 2  FF FF
   0 {L,}                   ~!!T   \ 4  00 00 00 00
   !>
<A J|X, 0 RL,      !TALLY NEXT A>
{ bufv 1+ C@   $01 XOR ( invert condition)   bufc 1+ OR!U   }
optimisation condition-pattern
REGRESS condition-pattern DUMPO S:
REGRESS original matches? S:  TRUE
REGRESS original 1+ matches? S:  FALSE
REGRESS replace bufc$ @ S: 6
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Replace a push of a constant register by a push immediate.

<!  !Q MOVI|X, !!T  0 {L,}  ~!!T    \ 5  F0 00 00 00 00
     !Q PUSH|X,            !!T  !>   \ 1  F0
<A   QX: !TALLY PUSHI|X, 0 IL,     !TALLY A>
{   bufv 2 + L@    bufc 2 + L!
    ins1$ bufc$ $+! }
optimisation movipush-pattern

REGRESS  movipush-pattern original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE

\ At  addr  :" a movi/push pair  is  to be found with same registers."
: movipush-same-reg find-reg1 DROP find-reg2 DROP = ;

\ Optional replace, leave " was  replaced".
:  ?movipush-replace? movipush-same-reg DUP IF replace THEN ;

REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: PUSH|X, AX| matches? ?movipush-replace? S: TRUE FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: PUSH|X, BX| matches? ?movipush-replace? S: TRUE  TRUE
REGRESS bufc$ @ bufc$ $@ .code S:  12
REGRESS HERE QN: MOVI|X, BX| 1234 IL, Q: PUSH|X, BX| matches? ?movipush-replace? S: TRUE FALSE
REGRESS HERE QN: MOVI|X, BX| 1234 IL, QN: PUSH|X, BX| matches? ?movipush-replace? S: TRUE  TRUE
REGRESS bufc$ @ bufc$ $@ .code S:  12

\ ----------------------------------------------------------------
\ Replace a "operation to register", by a "operate immediate.

<! !Q MOVI|X, !!T  0 {L,} ~!!T   !Q XXX, X| F| R| !!T !>
<A QX: !TALLY XXXI, X| R| 0 IL, !TALLY NEXT A>
{ bufv 2 + L@  bufc 3 + L!
   ins2 find-reg-RGL    DROP bufc 0 + put2-reg
   bufv 7 + C@ ( $38 AND ) bufc 2 + OR!U  \ Transfer opcode
   ins1$ bufc$ $+!
}
optimisation movigopr-pattern
REGRESS movigopr-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, PUSH|X, BX| matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: CMP, X| F| AX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: XOR, X| F| DX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| DX'| R| CX| matches? S: TRUE

\  :" it  is  all the same register."
: movigopr-same bufv 1 + C@ bufv 8 + C@ 3 RSHIFT XOR 7 AND 0=    ;
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: XOR, X| F| DX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| CX'| R| DX| matches? movigopr-same S: TRUE FALSE

\ Optional replace, leave " was  replaced".
: ?movigopr-replace? movigopr-same DUP IF replace THEN ;
REGRESS HERE Q: MOVI|X, DX| 123400 IL, QN: CMP, X| F| DX'| R| CX| matches? movigopr-same S: TRUE TRUE
REGRESS  ?movigopr-replace? bufv$ @ S: TRUE 9
REGRESS bufc$ @ bufc$ $@ .code S: 13
REGRESS HERE QN: MOVI|X, DX| 123400 IL, Q': CMP, X| F| DX'| R| CX| matches? movigopr-same S: TRUE TRUE
REGRESS  ?movigopr-replace? bufv$ @ S: TRUE 9
REGRESS bufc$ @ bufc$ $@ .code S: 13
\ ----------------------------------------------------------------
\ Replace a "operation to register reversed", by a "operate immediate.
\ This only works for symmetric operations: OR AND XOR ADD ADDC
<! !Q MOVI|X, !!T  0 {L,} ~!!T   !Q XXX, X| F| R| !!T !>
<A QX: !TALLY  PUSH|X,  !TALLY QX: !TALLY POP|X, !TALLY
QX: !TALLY XXXI, X| R| 0 IL, !TALLY NEXT A>
{   bufv 2 + L@  bufc 7 + L!
    ins1  get1-reg-QN  DUP bufc 4 + put2-reg-QN  bufc 2 + put1-reg-QN
    ins2  get2-reg-Q'  bufc put1-reg-QN
   bufv 7 + C@ ( $38 AND ) bufc 6 + OR!U  \ Transfer opcode
}
optimisation movirgopr-pattern
REGRESS movirgopr-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, PUSH|X, BX| matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: CMP, X| F| AX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: XOR, X| F| DX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| DX'| R| CX| matches? S: TRUE
\  :" the move and the opr  have  the same target register."
\ Also the operator is viable
: movirgopr-viable ins1 get1-reg-QN ins2 get2-reg-QN =
        bufv 7 + C@  $38 AND  $28 <   AND ;
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: XOR, X| F| DX'| R| CX| matches? movirgopr-viable S: TRUE FALSE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| DX'| R| CX| matches? movirgopr-viable S: TRUE FALSE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| CX'| R| DX| matches? movirgopr-viable S: TRUE FALSE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: ADD, X| F| CX'| R| DX| matches? movirgopr-viable S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?movirgopr-replace? movirgopr-viable DUP IF replace THEN ;
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: CMP, X| F| CX'| R| DX| matches? movirgopr-viable S: TRUE FALSE
REGRESS  ?movirgopr-replace? bufv$ @ S: FALSE 9
REGRESS HERE Q: MOVI|X, AX| 123400 IL, Q: ADD, X| F| CX'| R| AX| matches? movirgopr-viable S: TRUE TRUE
REGRESS  ?movirgopr-replace? bufv$ @ S: TRUE 9
REGRESS bufc$ @ bufc$ $@ .code S: 11
REGRESS HERE Q:  MOVI|X, AX| 1 IL, Q: ADD, X| F| BX'| R| AX|  matches? movirgopr-viable S: TRUE TRUE
REGRESS  ?movirgopr-replace? bufv$ @ S: TRUE 9
REGRESS bufc$ @ bufc$ $@ .code S: 11

\ ---------------------------------------------------------------------------
\ A movi reg, followed by an operation using that reg as input.
\ Replace by an operation using immediate data.
\ A MOVI|X, can be followed by all r/mem addressing modes.
<! !Q  MOVI|X, !!T  0 {L,} ~!!T
    !Q MOV, X| F| XO| !!T 0 {L,} ~!!T !>
<A  Q: MOVI, X| XO| 0 L,  0 IL, !TALLY A>
{ bufv 9 + L@ bufc 3 + L!   \ Offset
  bufv 2 + L@ bufc 7 + L!   \ Data
  ins2 get2-reg-QN  bufc put2-reg-QN
  ins1$ bufc$ $+!
}
optimisation movimovxo-pattern
REGRESS movimovxo-pattern  DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: MOV, X| F| AX'| XO| [CX] 12 L, matches? S: TRUE

\  :" the targetof the first move  is  the source for the second move"
: movimovxo-okay   bufv 1+ C@ bufv 8 + C@ 3 RSHIFT XOR 7 AND 0=
   bufv 8 + C@ 7 AND 5 <> AND ;
REGRESS HERE Q: MOVI|X, DX| 1 IL, Q: MOV, X| F| CX'| XO| [BX] 7 L, matches? movimovxo-okay   S: TRUE FALSE
REGRESS HERE Q: MOVI|X, DX| 1 IL, Q: MOV, X| F| DX'| XO| [BP] 7 L, matches? movimovxo-okay   S: TRUE FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: PUSH|X, BX| matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: MOV, X| F| AX'| BO| [CX] 0 B, matches? S: FALSE
REGRESS HERE Q: MOVI|X, AX| 0 IL, Q: MOV, X| F| AX'| XO| [BP] 7 L, matches? movimovxo-okay   S: TRUE FALSE
REGRESS HERE Q: MOVI|X, AX| 0 IL, Q: MOV, X| F| DX'| XO| [BX] 7 L, matches? movimovxo-okay   S: TRUE FALSE

\ Optional replace, leave " was  replaced".
: ?movimovxo-replace? movimovxo-okay DUP IF replace THEN ;
REGRESS HERE Q: MOVI|X, BX| 13 IL, Q: MOV, X| F| BX'| XO| [BP] 16 L, matches? S: TRUE
REGRESS HERE Q': MOVI|X, DI| 1234 IL, Q': MOV, X| F| DI'| XO| [CX] 4321 L, matches? S: TRUE
REGRESS  ?movimovxo-replace? bufv$ @ S: TRUE 13
REGRESS bufc$ $@ .code bufc$ @ S: 17

\ ----------------------------------------------------------------
\ Optimise MOVI MOVR to MOVI MOVI .
<! !Q MOVI|X, !!T  0 {L,} ~!!T   !Q MOV, X| F| R| !!T !>
<A QX: !TALLY MOVI|X, 0 IL, !TALLY  A>
{ bufv 2 + L@  bufc 2 + L!
  ins2 get1-reg-QN bufc put1-reg-QN
  ins1$ bufc$ $+!
}
optimisation movimovr-pattern
REGRESS movimovr-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: PUSH|X, BX| matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: MOV, X| F| AX'| R| CX| matches? S: TRUE
REGRESS HERE Q: MOVI|X, DX| 123400 IL, Q: MOV, X| F| DX'| R| CX| matches? S: TRUE

\  :" it  is  all the same register."
: movimovr-same bufv 1+ C@ bufv 8 + C@ 3 RSHIFT XOR 7 AND 0=    ;
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: PUSH|X, BX| matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, Q: MOV, X| F| AX'| R| CX| matches? movimovr-same S: TRUE FALSE
REGRESS HERE QN: MOVI|X, DI| 123400 IL, QN': MOV, X| F| DI'| R| CX| matches? movimovr-same S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?movimovr-replace? movimovr-same DUP IF replace THEN ;
REGRESS  ?movimovr-replace? bufv$ @ S: TRUE 9
REGRESS bufc$ $@ .code bufc$ @ S: 12

\ ----------------------------------------------------------------
\ Optimise MOVI JX to MOVI .
\ ----------------------------------------------------------------
<!  !Q MOVI|X, !!T 0 {L,} ~!!T J|X, !!T 0 {L,} ~!!T !>
<A  J|X, !TALLY 0 {L,} !TALLY A>
{ bufv 7 + C@ bufc 1 + OR!U }
optimisation movijx-pattern
REGRESS movijx-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, J|X, Y| C| 1234 RL, matches? S: TRUE
REGRESS HERE QN: MOVI|X, BX| 1234 IL, J|X, Y| C| 1234 RL, matches? S: TRUE

REGRESS  replace bufv$ @ S: 12
REGRESS bufc$ $@ .code bufc$ @ S: 6

\ ----------------------------------------------------------------
\ Optimise XORI JX S| to TEST JX S|  Y| / N|
\ ----------------------------------------------------------------
<!  !Q XORI, X| R|  !!T 0 {L,} ~!!T J|X, S| !!T 0 {L,} ~!!T !>
<A  Q: TESTI, X| R| -1 IL,       !TALLY
    J|X, S|    0 (RL,) !TALLY A>
{ bufv 2 + C@  7 AND    bufc 2 + OR!U  \ reg
     bufv 3 + L@ L>S 0< 1 AND  bufc 8 + SWAP TOGGLE }
optimisation xorijx-pattern
REGRESS xorijx-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: XORI, X| R| DI| -1 IL, J|X, S| Y| 123 (RL,) matches? S: TRUE

REGRESS  replace bufv$ @ S: 13
REGRESS bufc$ $@ .code bufc$ @ S: 13
\ ----------------------------------------------------------------
\ Optimise SUB JX to CMP JX
<!  !Q SUB, X| R|  !!T J|X, !!T 0 {L,} ~!!T !>
<A  QX: !TALLY  CMP, X| R|  !TALLY  J|X,     0 (RL,) !TALLY A>
{ bufv L@  $FFFFFF AND bufc OR!U  \ All variable of first
     bufv 3 + L@ bufc 3 + OR!U }  \ All remaining variable
optimisation subjx-pattern
REGRESS subjx-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: SUB,  X| R| DI| T| AX'|  J|X, S| Y| 123 (RL,) matches? S: TRUE

REGRESS  replace bufv$ @ S: 9
REGRESS bufc$ $@ .code bufc$ @ S: 9

\ ----------------------------------------------------------------
\ Optimise AND JX to TEST JX
<!  !Q AND, X| R|  !!T J|X, !!T 0 {L,} ~!!T !>
<A  QX: !TALLY  TEST, X| R|  !TALLY  J|X,     0 (RL,) !TALLY A>
{ bufv L@  $FFFDFF AND bufc OR!U  \ All variable of first, except T|
     bufv 3 + L@ bufc 3 + OR!U }  \ All remaining variable
optimisation andjx-pattern
REGRESS andjx-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS HERE Q: AND,  X| R| DI| T| AX'|  J|X, S| Y| 123 (RL,) matches? S: TRUE

REGRESS  replace bufv$ @ S: 9
REGRESS bufc$ $@ .code bufc$ @ S: 9

\ ----------------------------------------------------------------------
\ Optimise  OPR <reg> TESTI <reg>
<! !Q XXX, X| R|  !!T !Q TESTI, X| R|  !!T -1 {L,} !!T !> \  J|X, !!T 0 {L,} ~!!T
<A Q: XXX, X| R|  !TALLY CLC, !TALLY A>
\ Maybe context  ( J|X, 0 (RL,) !TALLY )
{ bufv L@  bufc OR!U }
optimisation  oprnotest-pattern

REGRESS oprnotest-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
\  :" it  is  all the same register."
: oprnotest-same
    ins1  DUP Qp@ #DD AND IF get2-reg-Q' ELSE  get2-reg-QN  THEN
    ins2  get2-reg-QN = ;
REGRESS HERE Q: XOR, X| T| R| DI'| BX| Q: TESTI, X| R| BX| -1 IL, matches? oprnotest-same S: TRUE FALSE
REGRESS HERE Q: XOR, X| F| R| DI'| BX| Q: TESTI, X| R| BX| -1 IL, matches? oprnotest-same S: TRUE TRUE
\ Optional replace, leave " was  replaced".
: ?oprnotest-replace? oprnotest-same DUP IF replace THEN ;
REGRESS ?oprnotest-replace? bufv$ @ S: TRUE 10
REGRESS bufc$ $@ .code bufc$ @ S: 4
\ ----------------------------------------------------------------------
\ Optimise  away CLC, before a jump.
<! CLC, !!T J|X, !!T 0 {L,} ~!!T              !>
<A J|X, !TALLY 0 {L,} !TALLY A>
{ bufv 2 + C@ bufc 1 + OR!U }
optimisation  noclc-pattern

REGRESS noclc-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
\  :" it  does  not involve the carry flag."
: noclc-normal-jump
    bufv 2 + C@ $0E AND  DUP 2 <> SWAP 6 <> AND ;
REGRESS HERE CLC, J|X, CZ| Y| 12 (RL,) matches? noclc-normal-jump S: TRUE FALSE
REGRESS HERE CLC, J|X, C| N| 12 (RL,) matches? noclc-normal-jump S: TRUE FALSE
REGRESS HERE CLC, J|X, Z| Y| 12 (RL,) matches? noclc-normal-jump S: TRUE TRUE
\ Optional replace, leave " was  replaced".
: ?noclc-replace? noclc-normal-jump DUP IF replace THEN ;
REGRESS ?noclc-replace? bufv$ @ S: TRUE 7
REGRESS bufc$ $@ .code bufc$ @ S: 6

\ ----------------------------------------------------------------
\ Optimise AX := 0  AL:=[MEM] by a zero extend move.
\ ----------------------------------------------------------------
<!  !Q MOVI|X, !!T 0 {L,}
     !Q  MOV, B| T| XO| !!T 0 {L,} ~!!T !>
<A  Q: MOVZX|B, XO| !TALLY 0 {L,} !TALLY A>
{  find-reg2  7 AND 3 LSHIFT  >R
DUP 3 RSHIFT 2 RSHIFT bufc OR!U  7 AND R> OR bufc 3 + OR!U
   ins2 3 + L@ bufc 4 + L! }
optimisation movzx-pattern
REGRESS movzx-pattern DUMPO S:
REGRESS original matches? S: TRUE
REGRESS original 1+ matches? S: FALSE
REGRESS  HERE Q: MOVI|X, AX| 0 IL, Q:    MOV, B| T| AL'| XO| [BX] 12345 L, matches? S: TRUE

\  :" the same register  is  zero-extended."
:  movzx-same find-reg1 DROP  find-reg2 NIP = ;
REGRESS  HERE Q: MOVI|X, AX| 0 IL, Q:    MOV, B| T| BL'| XO| [BX] 12345 L, matches? movzx-same S: TRUE FALSE
REGRESS  HERE Q: MOVI|X, AX| 0 IL, Q:    MOV, B| T| AH'| XO| [BX] 12345 L, matches? movzx-same S: TRUE FALSE
REGRESS  HERE Q: MOVI|X, AX| 0 IL, Q:    MOV, B| T| AL'| XO| [BX] 12345 L, matches? movzx-same S: TRUE TRUE

\ Optional replace, leave " was  replaced".
: ?movzx-replace? movzx-same DUP IF replace THEN ;
REGRESS  HERE Q: MOVI|X, CX| 0 IL, Q:    MOV, B| T| CL'| XO| [BX] 12345 L, matches? S: TRUE
REGRESS  ?movzx-replace? bufv$ @ S: TRUE 13
REGRESS bufc$ $@ .code bufc$ @ S: 8

\ ----------------------------------------------------------------
\ Remove MOVI|X . IRREGULAR!
\ This can only be used for a non-permanent register at the end of a bblock.
\ It must be tried last, after all matches to MOVI, because it has no
\ context and would prevent other matches to ever trigger.

<! !Q MOVI|X, !!T  0 {L,}  ~!!T !>
<A A>
{ }
optimisation movinope-pattern
REGRESS DUMPO S:
REGRESS original 1+ matches? S:  FALSE
REGRESS original matches? S:  TRUE

\ Compare  pointer  to code with `movinope
\ For now permanent registers are valid through a cluster of blocks.
\ Only at the last block this optimisation can be applied to a permanent
\ register. This is not know here.
: movinope-non-permanent   ins1 get1-reg-QN permanent? NOT? ;
\ : movinope-non-permanent   TRUE ;
REGRESS HERE Q: MOVI|X, BX| 1234 IL, matches? movinope-non-permanent S: TRUE TRUE

\ For  ins  : "it  is  the end of the basic block or its body. "
: at-end  DUP stopper @ =   SWAP  @ $F0,FF AND $80,0F = OR ;
REGRESS HERE J|X, Z| Y| 33 (RL,) at-end S: TRUE

\ Optional replace, leave " was  replaced".
: ?movinope-replace? movinope-non-permanent  ins2 at-end AND
    ( btype #SEMIS = OR ) DUP IF replace THEN ;
REGRESS HERE Q: MOVI|X, BX| 1234 IL, CLC, 0 stopper ! matches? S: TRUE
REGRESS ?movinope-replace? S: FALSE
REGRESS HERE Q: MOVI|X, BX| 1234 IL, HERE stopper ! matches? S: TRUE
REGRESS ?movinope-replace? bufc$ @ S: TRUE 0
REGRESS bufc$ $@ .code S:

\ ----------------------------------------------------------------
\ Remove PUPO . IRREGULAR!
\ This can only be used for a non-permanent register at the end of a bblock.
\ It must be tried last, after all matches to PUPO, because it has no
\ context and would prevent other matches to ever trigger.

\ There is no puponope-pattern , pupo-pattern from optbb_nobrain suffices.
REGRESS pupo-pattern DUMPO S:
REGRESS original 1+ matches? S:  FALSE
REGRESS original matches? S:  TRUE

\ Compare  pointer  to code with `puponope
\ For now permanent registers are valid through a cluster of blocks.
\ Only at the last block this optimisation can be applied to a permanent
\ register. Here we don't know about blocks or their types.
: puponope-non-permanent   ins2 get1-reg-QN permanent? NOT? ;

REGRESS HERE Q: PUSH|X, BX| Q: POP|X, SI| matches? puponope-non-permanent S: TRUE FALSE

\ Optional replace, leave " was  replaced".
: ?puponope-replace? puponope-non-permanent  ins3 at-end AND
    ( btype #SEMIS = OR ) DUP IF replace THEN ;
REGRESS HERE Q: PUSH|X, SI| Q: POP|X, BX| 0 stopper ! matches? S: TRUE

REGRESS ?puponope-replace? S: FALSE
REGRESS HERE Q: PUSH|X, BX| Q: POP|X, SI| HERE stopper ! matches? puponope-non-permanent S: TRUE FALSE
REGRESS HERE Q: PUSH|X, SI| Q: POP|X, BX| HERE stopper ! matches? puponope-non-permanent ins3 at-end S: TRUE TRUE TRUE
REGRESS HERE Q: PUSH|X, SI| Q: POP|X, BX| J|X, Z| N| 12 RL, DSP@ stopper ! matches? ins3 at-end S: TRUE TRUE
REGRESS ?puponope-replace? bufc$ @ S: TRUE 0
