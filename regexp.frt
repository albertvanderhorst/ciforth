( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)

( $Id$)

'$@ ALIAS @+
REQUIRE TRUE

\ Regular expressions in Forth.
\ This package handles only simple regular expressions and replacements.
\ See the words RE-MATCH and RE-REPLACE for usage.
\ The following aspects are handled:
\    1. Compiling ^ (begin only)  $  (end only) and special characters + ? * [ ] < >
\    2. Grouping using ( ) , only for replacement.
\    3. Above characters must be escaped if used as is.

\ Implementation notes:
\ * Usually regular expressions are compiled into a buffer consisting of
\   tokens followed by strings or characters in some format.
\   We follow the same here, except that tokens are execution tokens.
\ * No attempt is done at reentrant code.
\ * \d \s \w etc. can be handled by making the expression compiler more
\   powerful.

\ Data structures :
\   a char set is a bit set, with a bit up for the matching character.
\   a string is a regular string variable (so with a cell count).

\ Configuration
1000 CONSTANT MAX-RE     \ # cells in a regular expression.
128 8 / CONSTANT MAX-SET \ # chars in a charset. (So no char's > 0x80 !)

\ -----------------------------------------------------------------------
\                      Bit sets
\ -----------------------------------------------------------------------
REQUIRE NEW-DO
\ REQUIRE 2SET+!
: 2SET+! >R SWAP R@ SET+! R> SET+! ;
\ REQUIRE WHERE-IN-SET

( For VALUE and SET return the POSITION of value in set, or a null. )
: WHERE-IN-SET $@ SWAP ?DO
   DUP I @ = IF DROP I UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;


CREATE BIT-MASK-TABLE 1   8 0 DO DUP C, 1 LSHIFT LOOP DROP
: BIT-MASK   CHARS BIT-MASK-TABLE + C@ ;

\ Set BIT in BITSET
: SET-BIT SWAP 8 /MOD SWAP BIT-MASK >R DUP C@ R> OR SWAP C! ;

\ For BIT in BITSET , return "it IS set".
: BIT? SWAP 8 /MOD SWAP BIT-MASK >R DUP C@ R> AND 0= 0= ;

\ -----------------------------------------------------------------------
\                  char sets, generic part
\ -----------------------------------------------------------------------
: |  OVER SET-BIT ; \ Shorthand, about to be hidden.

\ This contains alternatingly a character, and a pointer to a charset.
\ The charset is denotated by this character preceeded by '\' e.g. \s.
100 SET CHAR-SET-SET     CHAR-SET-SET SET!

\ For an identifying CHAR add an as yet empty set to ``CHAR-SET-SET''.
\ Leave a POINTER to the set (to fill it in).
: CHAR-SET CREATE HERE CHAR-SET-SET 2SET+!   HERE MAX-SET 0 DO 0 C, LOOP  DOES> ;
\ For a CHAR-SET ; convert it into its complementary set.
: INVERT-SET MAX-SET 1+ 0 DO I OVER + DUP C@ INVERT SWAP C! LOOP   DROP ;

\ -----------------------------------------------------------------------
\                  char sets, actual part
\ -----------------------------------------------------------------------

REQUIRE ?BLANK      \ Indicating whether a CHAR is considered blank in this Forth.

&w CHAR-SET \w   256 0 DO I IS-BLANK? 0= IF I | THEN LOOP DROP

\ Example of another set definition
\ &d CHAR-SET \d   &9 1+ &0 DO I | LOOP   DROP
\ &D CHAR-SET \D   \d OVER MAX-SET CMOVE  INVERT-SET

'| HIDDEN

\ -----------------------------------------------------------------------
\                  escape table
\ -----------------------------------------------------------------------
: | ROT DUP >R 2SET! R> ;    \ Shorthand, about to be hidden.
\ This contains alternatingly an escaped character, and its ASCII meaning.
100 SET ESCAPE-TABLE     ESCAPE-TABLE SET!
&n ^J |   &r ^M |   &b ^H |   &t ^I |   &e ^Z 1+ |

\ For CHARACTER return the ``ASCII'' VALUE it represents, when escaped.
: GET-ESCAPE ESCAPE-TABLE WHERE-IN-SET DUP IF CELL+ @ _ THEN DROP ;
'| HIDDEN

\ -----------------------------------------------------------------------

\ The compiled pattern.
\ It contains xt's, strings and charsets in a sequential format, i.e.
\ you can only find out what it means by reading from the beginning.
\ It is NULL-ended.
\ BNF = <term>+ <endsentinel>
\ term = <quantifier>? <atom> CHAR-SET | 'ADVANCE-EXACT STRING-VARIABLE
\ atom = 'ADVANCE-CHAR
\ quantifier = 'ADVANCE? | 'ADVANCE+ | 'ADVANCE*
\ endsentinel = 0
\ For nested expressions one could add :
\ atom = 'ADVANCE( <term>+ <endsentinel>

CREATE RE-PATTERN MAX-RE CELLS ALLOT
: CELL- 0 CELL+ - ;
\ For CHARPOINTER and EXPRESSIONPOINTER :
\ bla bla + return "there IS a match"
: MATCH BEGIN @+ DUP WHILE EXECUTE 0= IF CELL- 0 EXIT THEN REPEAT CELL- 1 ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the character matches the charset at the expression,
\ advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
: ADVANCE-CHAR  OVER C@ OVER IN-SET? DUP >R IF SWAP CHAR+ SWAP MAX-SET CHARS + THEN R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ as long as the character agrees with the matcher at the expression,
\ advance it.
\ Return CHARPOINTER advanced and EXPRESSIONPOINTER advanced past the matcher.
: (ADVANCE*)
    @+ >R
    BEGIN
        2DUP R@ EXECUTE
    WHILE
        DROP >R >R DROP R> R> SWAP
    REPEAT
    >R DROP DROP R> RDROP ;
\ This would benefit from locals :
\ : (ADVANCE*) @+ LOCAL MATCHER   LOCAL EP   LOCAL CP
\         BEGIN CP EP MATCHER EXECUTE WHILE DROP TO CP REPEAT
\         TO EP DROP     CP EP ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the char sequence at charpointer matches the string variable at the
\ expressionpointer, advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
: ADVANCE-EXACT  2DUP $@ CORA 0= DUP >R IF $@ >R SWAP R@ + SWAP R> + ALIGNED THEN R> ;

\

\ For CHARPOINTER and EXPRESSIONPOINTER and BACKTRACKPOINTER :
\ if there is match between btp and cp with the ep,
\ return CHARPOINTER ann EXPRESSIONPOINTER incremented past the match,
\ else return BTP and EP. Plus "there IS a match".
: BACKTRACK
    >R BEGIN MATCH 0= WHILE
        OVER R@ = IF RDROP 0 EXIT THEN
        SWAP 1 - SWAP
    REPEAT
    RDROP 1 ;

\ if the following match xt (at ``EXPRESSIONPOINTER'' ) works out,
\ with the modifier ( * + ? ) ,
\ advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
: ADVANCE? OVER >R   \ Remember backtrack point
    @+ EXECUTE DROP
    R> BACKTRACK ;
: ADVANCE* OVER >R   (ADVANCE*) R> BRACKTRACK ;
: ADVANCE+ @+ EXECUTE >R R@ IF ADVANCE* DROP THEN R> ;
