( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: analyserconfig.frt,v 5.2 2019/05/02 13:34:00 albert Exp $)

\ WARNING: HEX THROUGHOUT THIS WHOLE FILE !

                                HEX

\ ------------------------------------------------
\ Set BITS of mask in ADDRESS.
: OR!U >R R@ @ OR R> ! ;

\ Reset BITS of mask in ADDRESS.
: AND!U >R INVERT R@ @ AND R> ! ;
\ ------------------------------------------------
10 CONSTANT FMASK-IL    \ Data is following in line.
20 CONSTANT FMASK-HO    \ This definition has been high level optimised.
40 CONSTANT FMASK-HOB   \ This definition cannot be high level optimised.
80 CONSTANT FMASK-DUP   \ This is a duplicator, i.e. stack items consumed
                        \ are put back unchanged.

0FF00 CONSTANT FMASK

\ If set the side effects are absent, allowing optimisations.
100 CONSTANT FMASK-N@    \ No input side effects. "No fetches."
200 CONSTANT FMASK-N!    \ No output side effects. "No stores."
\ FIXME : the following line is unclear.
400 CONSTANT FMASK-ST \ "No absolute stack reference or return stack."
\ Always set except if the code contains Intel looping instructions.
\ This definition has the `EDI'' register free. "No loop"
\ Ignore for now, this probably has to expanded ro information about all
\ registers.
1000 CONSTANT FMASK-FDI

FMASK-N@ FMASK-N! OR CONSTANT FMASK-JS \ No side effects "just stack"
FMASK-JS FMASK-ST OR CONSTANT FMASK-NS \ No side effects at all.

\ ------------------------------------------------

\ From FLAGS POPS PUSHES compose a flag field content and return IT.
: COMPOSE-FLAGS 1+ SWAP 1+ 4 LSHIFT OR 18 LSHIFT OR ;

\ Fill FLAGS POPS PUSHES in into DEA's flag field.
\ Leave hidden, immediate etc. alone.
: !FLAGS >R COMPOSE-FLAGS R> >FFA OR!U ;

\ Fill in the stack effect BYTE into the flag field of DEA.
: !SE >FFA 3 + C! ;

\ For DEA return its stack effect BYTE.
: SE@ >FFA 3 + C@ ;

\ Split a BYTE into a STACK EFFECT .
: SE:1>2 DUP 4 RSHIFT SWAP 0F AND ;

\ Combine the STACK EFFECT into one BYTE.
: SE:2>1 0F AND SWAP 4 LSHIFT OR ;

\ ------------------------------------------------

'TASK >CFA @ CONSTANT DOCOL
'FORTH >CFA @ CONSTANT DODOES
'BASE  >CFA @ CONSTANT DOUSER
VARIABLE DUMMY 'DUMMY >CFA @ CONSTANT DOVAR
'BL >CFA @ CONSTANT DOCON

\ ------------------------------------------------
DECIMAL
