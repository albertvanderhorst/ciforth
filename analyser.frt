( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id: analyser.frt,v 5.9 2019/08/20 22:36:09 albert Exp $)

\ Adds to the flag fields of Forth words with the stack effect and
\ optimisation data. See DATA DESIGN.

\ Assumptions made by the analyser:
\   1. In low level code pushes and pops are never by-passed with branches.
\   2. In low level code POP|ALL, and PUSH|ALL, are not used at all.
\   3. Variable stack effect is filled manually.
\   4. Once the stack effect has been filled in, it isn't touched.
\      This way a prefilled stack effect overrules the automated search.
\   5. In the same way optimisation bits can be overruled,
\        but only for code words.
\
\ Convention: ?xxx? works only have effect if needed.

\ Caveat : adapt the CODE-TYPES bag, if you use threading, as indicated.

1 LOAD          \ To get `` WANT ''.

WANT ALIAS BAG IN-BAG? INCLUDE

INCLUDE analyserconfig.frt
INCLUDE asgen.frt
INCLUDE asi386.frt
INCLUDE analyseras.frt
INCLUDE analysermain.frt
                                HEX

\ Set of duplicators.
CREATE DUPS  HERE 0 ,
' DUP       ,           ' 2DUP ,
' OVER      ,           ' 2OVER ,
HERE SWAP !

\ Mark all duplicators as such.
: MARK-DUP  DUPS DO-BAG   FMASK-DUP I @ >FFA OR!U   LOOP-BAG ;

\ Irritating exceptions filled in by hand. Filling in the stack effect
\ (although it could be found automatically), prevents changes by ``FILL-ALL''.
0FF '?DUP !SE
0FF 'EXECUTE !SE
0FF 'DSP!    !SE
0FF 'THROW   !SE
\ DEFECT: this should follow from THROW, but only one branch of an IF is inspected.
0FF '?ERROR  !SE
0                     1  0 'FOR-VOCS  !FLAGS  \ Despite an execute this is known
0                     2  0 'FOR-WORDS !FLAGS
FMASK-NS FMASK-IL OR  0  1 'LIT       !FLAGS
FMASK-NS FMASK-IL OR  0  0 'SKIP      !FLAGS
FMASK-NS FMASK-IL OR  0  0 'BRANCH    !FLAGS
FMASK-NS FMASK-IL OR  1  0 '0BRANCH   !FLAGS
\ FMASK-JS is weaker, it doesn't preclude affecting the return stack
\ FMASK-JS FMASK-IL OR  0  0 '(LOOP)    !FLAGS
FMASK-JS              1  1 '(+LOOP)   !FLAGS
FMASK-JS FMASK-IL OR  2  0 '(DO)      !FLAGS
FMASK-JS FMASK-IL OR  2  0 '(?DO)     !FLAGS
\ FMASK-JS              0  0 'LEAVE     !FLAGS
FMASK-JS              0  0 'EXIT      !FLAGS
FMASK-JS              0  1 'DSP@      !FLAGS
\ FMASK-JS              0  1 'RSP@      !FLAGS   why not?
FMASK-JS              0  0 '.S        !FLAGS
FMASK-JS              1  0 '>R        !FLAGS
FMASK-JS              0  1 'R@        !FLAGS
FMASK-JS              0  1 'I         !FLAGS
FMASK-JS              0  1 'R>        !FLAGS
FMASK-JS              0  0 'RDROP     !FLAGS
FMASK-JS              0  0 'UNLOOP    !FLAGS
\ This one has conditional branches around pushes:
FMASK-ST FMASK-N! OR  3 4  '$/    !FLAGS

MARK-DUP
FILL-ALL

DECIMAL

INCLUDE analyserdebug.frt
