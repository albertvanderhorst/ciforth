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
\                  char sets, generic part
\ -----------------------------------------------------------------------
: |  OVER BIT-SET ; \ Shorthand, about to be hidden.

\ This contains alternatingly a character, and a pointer to a charset.
100 SET CHAR-SET-SET     CHAR-SET-SET SET!

\ For an identifying CHAR add an as yet empty set to ``CHAR-SET-SET''.
\ Leave a POINTER to the set (to fill it in).
: CHAR-SET CREATE HERE CHAR-SET-SET 2SET+!   HERE MAX-SET 0 DO 0 C, LOOP  DOES> ;
\ For a CHAR-SET ; convert it into its complementary set.
: INVERT-SET MAX-SET 1+ 0 DO I OVER + DUP C@ INVERT SWAP C! LOOP   DROP ;

\ -----------------------------------------------------------------------
\                  char sets, actual part
\ -----------------------------------------------------------------------

REQUIRE NEW-DO
REQUIRE ?BLANK      \ Indicating whether a CHAR is considered blank in this Forth.

&w CHAR-SET \w   256 0 DO I IS-BLANK? 0= IF I | THEN LOOP DROP

\ Example of another set definition
\ &d CHAR-SET \d   &9 1+ &0 DO I | LOOP   DROP
\ &D CHAR-SET \D   \d OVER MAX-SET CMOVE  INVERT-SET

\ -----------------------------------------------------------------------

'| HIDDEN      \
\ The compiled pattern.
\ It contains xt's, strings and charsets in a sequential format, i.e.
\ you can only find out what it means by reading from the beginning.
\ It is NULL-ended.
CREATE RE-PATTERN MAX-RE CELLS ALLOT

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the character matches the charset at the expression,
\ advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
\ *****************************************************************************/
\ For the moment the set is replaced by a single character.
\ *****************************************************************************/

: ADVANCE-CHAR  OVER C@ OVER @ = DUP >R IF SWAP CHAR+ SWAP CELL+ THEN R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ return "there IS a match"
: MATCH @+ EXECUTE >R 2DROP R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the char sequence at charpointer matches the string variable at the
\ expressionpointer, advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
: ADVANCE-EXACT  2DUP $@ CORA 0= DUP >R IF $@ >R SWAP R@ + SWAP R> + ALIGNED THEN R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the following match xt (at ``EXPRESSIONPOINTER'' ) works out,
\ with the modifier ( * + ? ) ,
\ advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
: ADVANCE?
    OVER >R @+ EXECUTE >R 2DUP MATCH IF
        RDROP RDROP 2DROP TRUE
    ELSE R> IF
            DROP R> SWAP MATCH
        ELSE
            2DROP RDROP  FALSE
        THEN
    THEN ;
: ADVANCE*
    OVER >R
    @+ >R
    BEGIN
        2DUP R@ EXECUTE
    WHILE
        DROP >R >R DROP R> R> SWAP
    REPEAT
    >R DROP DROP R> RDROP

MATCH

;


: ADVANCE+ 2DUP @+ EXECUTE >R 2DROP R@ IF ADVANCE* DROP THEN R> ;
