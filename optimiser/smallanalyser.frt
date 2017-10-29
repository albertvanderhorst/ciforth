( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( $Id$)

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

CREATE marker-a

REQUIRE BAG
REQUIRE IN-BAG?

INCLUDE analyserconfig.frt
\ We can't do anything here, but drop the DEA. A code definitions will lead
\ to unknown, unless filled in by hand using `` !FLAGS ''.
: FILL-FLAG-CODE   DROP ;

INCLUDE analysermain.frt
DECIMAL
