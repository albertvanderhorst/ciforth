( Copyright{2002}: Albert van der Horst, HCC FIG Holland by GNU Public License)

( $Id$)

: \D ;

'$@ ALIAS @+
REQUIRE TRUE
INCLUDE set.frt
INCLUDE bits.frt

\ Regular expressions in Forth.
\ This package handles only simple regular expressions and replacements.
\ See the words RE-MATCH and RE-REPLACE for usage.
\ The following aspects are handled:
\    1. Compiling ^ (begin only)  $  (end only) and special characters + ? * [ ] < >
\    2. Grouping using ( ) , only for replacement.
\    3. Above characters must be escaped if used as is by \ , making \ a special char.
\    4. Some sets are escaped by \ (\w) , some non-printables are denote by an
\       escape sequence.

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
: |  OVER SET-BIT ; \ Shorthand, about to be hidden.

\ This contains alternatingly a character, and a pointer to a charset.
\ The charset is denotated by this character preceeded by '\' e.g. \s.
100 SET CHAR-SET-SET     CHAR-SET-SET !SET

\ Allocate a char set and leave a POINTER to it.
: ALLOT-CHAR-SET   HERE MAX-SET 0 DO 0 C, LOOP ;
\ Note that ``ASCII'' null must be excluded from every char-set!
\ Algorithms rely on it.
\ For an identifying CHAR create an as yet empty char set with "NAME"
\ and add it to ``CHAR-SET-SET''.
\ Leave a POINTER to the set (to fill it in).
: CHAR-SET CREATE HERE CHAR-SET-SET 2SET+!  ALLOT-CHAR-SET DOES> ;
\ For a CHAR-SET ; convert it into its complementary set.
: INVERT-SET MAX-SET 0 DO DUP I + DUP C@ INVERT SWAP C! LOOP  0 SWAP CLEAR-BIT ;

\ For CHAR and CHARSET return "it BELONGS to the charset".
: IN-CHAR-SET   BIT? ;

\ Add CHARSET1 to CHARSET2 .
: OR-SET! MAX-SET 0 DO OVER I + C@ OVER I + C@ OR OVER I + C! LOOP 2DROP ;

\ ------------------------------------------
\                  char sets, actual part
\ ------------------------------------------

REQUIRE ?BLANK      \ Indicating whether a CHAR is considered blank in this Forth.

\ Passing a 0 makes a char-set unfindable in ``CHAR-SET-SET''.

\ The set of characters to be escaped.
0 CHAR-SET \\     &\ | &^ | &$ | &+ | &? | &* | &[ | &] | &< | &> | &( | &) |  DROP

\ For CHAR : "it IS special".
: SPECIAL?   \\ BIT? ;

\ The set matched by .
0 CHAR-SET \.  ^J | INVERT-SET

&w CHAR-SET \w   256 1 DO I ?BLANK 0= IF I | THEN LOOP DROP

\ Example of another set definition
\ &d CHAR-SET \d   &9 1+ &0 DO I | LOOP   DROP
\ &D CHAR-SET \D   \d OVER MAX-SET CMOVE  INVERT-SET

'| HIDDEN

\ For CHAR: "it IS a quantifier".
: QUANTIFIER? >R R@ &+ = R@ &* = R@ &? = OR OR RDROP ;

\ -----------------------------------------------------------------------
\                  escape table
\ -----------------------------------------------------------------------
\ To SET add an escape CHAR and the escape CODE. Leave SET .
: | ROT DUP >R 2SET+! R> ;    \ Shorthand, about to be hidden.

\ This contains alternatingly an escaped character, and its ASCII meaning.
100 SET ESCAPE-TABLE     ESCAPE-TABLE !SET
ESCAPE-TABLE &n ^J |   &r ^M |   &b ^H |   &t ^I |   &e ^Z 1+ |   DROP

\ For CHARACTER return the ``ASCII'' VALUE it represents, when escaped.
: GET-ESCAPE DUP &a &z 1+ WITHIN 0= IF DROP 0 EXIT THEN
    ESCAPE-TABLE WHERE-IN-SET DUP IF CELL+ @ THEN ;
'| HIDDEN

\ -----------------------------------------------------------------------
\                  matched substrings
\ -----------------------------------------------------------------------
\ This table contains the ends and starts of matching substrings between ( )
\ 0 is what matches the whole expression, and is available without ( )
CREATE SUBSTRING-TABLE 20 CELLS ALLOT
\ To where has the table been used (during expression parsing).
VARIABLE ALLOCATOR
\ Initialise ALLOCATOR
: !ALLOCATOR 2 ALLOCATOR ! ;
\ Return a new ALLOCATOR index, and increment it.
: ALLOCATOR++
    ALLOCATOR @ DUP 11 = ABORT" Too many substrings with ( ), max 9, user error"
    1 ALLOCATOR +! ;

\ Return a new INDEX for a '('.
: ALLOCATE( ALLOCATOR++
DUP 1 AND ABORT" ( where ) expected, inproper nesting, user error" ;
\ Return a new INDEX for a ')'.
: ALLOCATE) ALLOCATOR++
DUP 1 AND 0= ABORT" ) where ( expected, inproper nesting, user error" ;

\ Remember CHARPOINTER as the substring with INDEX.
: REMEMBER()
\D DUP 0 10 WITHIN 0= ABORT" substring index out of range, system error"
CELLS SUBSTRING-TABLE + ! ;

\ For INDEX create a "word" that returns the matched string with that index.
: CREATE\ CREATE 2 * CELLS SUBSTRING-TABLE + , DOES> @ 2@ SWAP OVER - ;

\ &9 1+ &0 DO   &\ PAD C!   I PAD 1+ C!   PAD 2 POSTFIX CREATE\ LOOP
0 CREATE\ \0    1 CREATE\ \1    2 CREATE\ \2    3 CREATE\ \3   4 CREATE\ \4
5 CREATE\ \5    6 CREATE\ \6    7 CREATE\ \7    8 CREATE\ \8   9 CREATE\ \9

\ -----------------------------------------------------------------------
\ START OF TESTED FOR COMPILATION ONLY AREA

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
\ Backup from ADDRESS one cell. Leave decremented ADDRESS.
: CELL- 0 CELL+ - ;
\ For CHARPOINTER and EXPRESSIONPOINTER :
\ bla bla + return "there IS a match"
: (MATCH) BEGIN @+ DUP WHILE EXECUTE 0= IF CELL- FALSE EXIT THEN REPEAT CELL- TRUE ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ as long as the character agrees with the matcher at the expression,
\ advance it.
\ Return CHARPOINTER advanced and EXPRESSIONPOINTER advanced past the matcher.
    \ From ONE TWO THREE FOUR leave THREE and TWO
    : KEEP32 DROP >R >R DROP R> R> SWAP ;
    \ From ONE TWO THREE FOUR leave ONE and FOUR
    : KEEP14 >R DROP DROP R> ;
: (ADVANCE*)   @+ >R BEGIN 2DUP R@ EXECUTE WHILE KEEP32 REPEAT KEEP14 RDROP ;

\ This would benefit from locals :
\ : (ADVANCE*) @+ LOCAL MATCHER   LOCAL EP   LOCAL CP
\         BEGIN CP EP MATCHER EXECUTE WHILE DROP TO CP REPEAT
\         TO EP DROP     CP EP ;

\ For CHARPOINTER and EXPRESSIONPOINTER and BACKTRACKPOINTER :
\ if there is match between btp and cp with the ep,
\ return CHARPOINTER ann EXPRESSIONPOINTER incremented past the match,
\ else return BTP and EP. Plus "there IS a match".
: BACKTRACK
    >R BEGIN (MATCH) 0= WHILE
        OVER R@ = IF RDROP FALSE EXIT THEN
        \ WARNING: 1 - will go wrong if there is a larger gap between backtrackpoints
        \ i.e. when ADVANCE( is there that would use larger leaps than ADVANCE-CHAR.
        SWAP 1 - SWAP
    REPEAT
    RDROP TRUE ;

\ ----------------------------------------------------------------
\           xt's that may be present in a compiled expression
\ -----------------------------------------------------------------

\ All of those xt's accept a charpointer and an expressionpointer.
\ The char pointer points into the string to be matched that must be
\ zero ended. The expressionpointer points into the buffer with
\ Polish xt's, i.e. xt's to be executed with a pointer to the
\ data following the xt.
\ They either leave those as is, and return FALSE, or
\ If the \ match still stands after the operation intended,
\ both pointers are bumped passed the characters consumed, and
\ data, and possibly more xt's and more data consumed.
\ The incremented pointers are returned, plus a true flag.
\ Otherwise the pointers are returned unchanged, plus a false flag.
\ The xt's need not do a match, they can do an operation that
\ never fails, such as remembering a pointer.

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the character matches the charset at the expression,
\ advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
\ In a regular expression buffer this xt must be followed by a char-set.
: ADVANCE-CHAR  OVER C@ OVER BIT? DUP >R IF SWAP CHAR+ SWAP MAX-SET CHARS + THEN R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if the char sequence at charpointer matches the string variable at the
\ expressionpointer, advance both past the match, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
\ In a regular expression buffer this xt must be followed by a string.
: ADVANCE-EXACT  2DUP $@ CORA 0= DUP >R IF $@ >R SWAP R@ + SWAP R> + ALIGNED THEN R> ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ if there is match between cp and the end of string with the ep,
\ return CHARPOINTER and EXPRESSIONPOINTER incremented past the match,
\ else return CP and EP. Plus "there IS a match".
\ (Note: this is the syncronisation, to be done when the expression does
\ *not* start with `^'.
: FORTRACK
    BEGIN (MATCH) 0= WHILE
        SWAP 1 + SWAP
        OVER C@ 0= IF FALSE EXIT THEN
    REPEAT
    RDROP TRUE ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ return CHARPOINTER and EXPRESSIONPOINTER plus "the strings HAS been used up".
\ (Note: this is an end-check, to be done only when the expression ends with '$'.)
: CHECK$
\D DUP @ 0= ABORT" CHECK$ compiled not at end of expression, system error"
    OVER C@ 0= ;

\ Where the matched part of the string starts.
: STARTPOINTER    SUBSTRING-TABLE @ ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ return CHARPOINTER and EXPRESSIONPOINTER plus "we ARE at the start of a word"
: CHECK< OVER STARTPOINTER @ = DUP 0= IF DROP OVER 1- C@ \w IN-CHAR-SET 0= THEN >R
         OVER C@ DUP IF \w IN-CHAR-SET THEN   R> AND ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ return CHARPOINTER and EXPRESSIONPOINTER plus "we ARE at the end of a word"
: CHECK< OVER STARTPOINTER @ = 0= DUP IF DROP OVER 1- C@ \w IN-CHAR-SET THEN >R
         OVER C@ DUP IF \w IN-CHAR-SET THEN 0=  R> AND ;

\ For CHARPOINTER and EXPRESSIONPOINTER :
\ Remember this as the start or end of a substring.
\ Leave CHARPOINTER and leave the EXPRESSIONPOINTER after the substring number.
\ Plus "yeah, it IS okay"
\ In case you wonder, because the offset is known, during backtracking just
\ the last (and final) position is remembered.
: HANDLE() @+ >R   OVER R> REMEMBER() TRUE ;

\ If the following match xt (at ``EXPRESSIONPOINTER'' ) works out,
\ with one of the modifiers: * + ?
\ advance both past the remainder of the expression, else leave them as is.
\ Return CHARPOINTER and EXPRESSIONPOINTER and "there IS a match".
\ In a regular expression buffer each of those xt must be followed by the
\ xt of ADVANCE-CHAR.
: ADVANCE? OVER >R @+ EXECUTE DROP R> BACKTRACK ;
: ADVANCE* OVER >R   (ADVANCE*) R> BACKTRACK ;
: ADVANCE+ DUP >R @+ EXECUTE IF DROP R> ADVANCE* ELSE RDROP FALSE THEN ;


\ END OF TESTED FOR COMPILATION ONLY AREA
\ ---------------------------------------------------------------------------
\                    building the regexp
\ ---------------------------------------------------------------------------

\ The compiled expression.
CREATE RE-COMPILED 1000 ALLOT

\ Regular expressions are parsed using a simple recursive descent
\ parser.

\ Build up a string to be matched simply.
CREATE NORMAL-CHARS 1000 ALLOT
: NORMAL-CHARS!   0 NORMAL-CHARS ! ;

\ To where is the compiled expression filled.
VARIABLE RE-FILLED

\ Initialise ``RE-FILLED
: !RE-FILLED RE-COMPILED RE-FILLED ! ;

\ Add ITEM to the ``RE-EXPR''
: RE,   RE-FILLED @ ! 1 CELLS RE-FILLED +! ;

\ Add STRINGCONSTANT to the ``RE-EXPR''
: RE$,   DUP >R RE-FILLED @ $!   RE-FILLED @ CELL+ R@ + ALIGNED RE-FILLED ! ;

\ Add CHARSET to the ``RE-EXPR''.
: RE-SET,   RE-FILLED @ MAX-SET MOVE   MAX-SET RE-FILLED +! ;

\ Make a hole in the ``RE-EXPR'' for a quantifier, and leave IT.
: MAKE-HOLE   MAX-SET >R  RE-FILLED @ R@ -
    DUP DUP CELL+ R> MOVE
    1 CELLS RE-FILLED +! ;

\ Add the command to match the string in ``NORMAL-CHARS'' to the compiled
\ expression.
: HARVEST-NORMAL-CHARS NORMAL-CHARS @ IF
        'ADVANCE-EXACT RE,   NORMAL-CHARS $@ RE$,   NORMAL-CHARS!
    THEN
;
\    -    -    -   --    -    -   -    -    -   -    -    -   -

\ Build up a set to be matched.
CREATE SET-MATCHED ALLOT-CHAR-SET
: SET-MATCHED!   SET-MATCHED MAX-SET ERASE   1 SET-MATCHED C! ;


\ Add the command to match the string in ``NORMAL-CHARS'' to the compiled
\ expression.
: HARVEST-SET-MATCHED SET-MATCHED @ IF
    'ADVANCE-CHAR RE,   SET-MATCHED RE-SET,  SET-MATCHED!
THEN
;
\    -    -    -   --    -    -   -    -    -   -    -    -   -
\ For EP and CHAR : add the char to the simple match, or make it
\ a single character set, whatever is needed. Leave EP.
: ADD-TO-NORMAL OVER C@ QUANTIFIER? IF
    HARVEST-NORMAL-CHARS 'ADVANCE-CHAR RE, RE-FILLED \EMPTY RE-SET, SWAP SET-BIT
ELSE NORMAL-CHARS $+! THEN ;

\    -    -    -   --    -    -   -    -    -   -    -    -   -

: GET-CHAR-SET CHAR-SET-SET WHERE-IN-SET
             DUP 0= ABORT" Illegal escaped char set, user error"
             CELL+ @ ;

\ EP is pointing after an '\' between '[  and ']'. Add the escaped
\ char (or set) to ``SET-MATCHED''
: ESCAPE[] C@+
    DUP GET-ESCAPE DUP IF SET-MATCHED SET-BIT DROP ELSE
        DROP GET-CHAR-SET SET-MATCHED OR-SET! THEN ;

\ EP is pointing to the first char of a range, between '[' and ']'.
\ Add the range to the ``SET-MATCHED''
\ CAN'T HANDLE ESCAPES, BUT WE ARE GOING TO DO THIS BY ELIMINATING
\ THE ESCAPES IN THE FIRST ROUND BY COPYING THE DATA TO A TEMPORARY
\ AREA, AND ADDING THE ZERO.
: SET-RANGE DUP C@ DUP 2+ C@ 1+ SWAP DO I SET-MATCHED SET-BIT LOOP 2DROP 3 + ;


\ For EP and the first CHAR of an item (pointing between [ and ] ) add
\ the item to ``SET-MATCHED''. Leave EP pointing after the item.
: ADD[]-1  C@+
    DUP &\ = IF DROP ESCAPE[] ELSE
    OVER C@ &- = IF DROP SET-RANGE ELSE
    SET-MATCHED SET-BIT
    THEN THEN
;


\ Build up the set between [ and ] into ``SET-MATCHED''.
\ EP points after the intial [ , leave it pointing after the closing ].
: PARSE[]    SET-MATCHED! C@+
    DUP &^ = IF DROP RECURSE SET-MATCHED INVERT-SET ELSE
    BEGIN ADD[]-1 C@+ DUP &] = UNTIL
    THEN ;

\ EP points to a char-set. Add it to the compiled expression.
\ Leave EP incremented past the char-set.
: PARSE-CHAR-SET C@+
    DUP &. = IF DROP \. SET-MATCHED MAX-SET MOVE ELSE
    DUP &\ = IF DROP C@+ GET-CHAR-SET SET-MATCHED MAX-SET MOVE ELSE
    DUP &[ = IF DROP PARSE[]
    THEN THEN THEN
    HARVEST-SET-MATCHED
;
\    -    -    -   --    -    -   -    -    -   -    -    -   -
\ Transform the EXPRESSION string. Copy it to the ``(RE-EXPR)'' buffer,
\ (NOT YET) meanwhile normalising it, i.e. a character has a special meaning, iff
\ it is preceeded by '\' (NOT YET). Also make it zero ended.
\ Leave a pointer to the zero ended EXPRESSION to parse.
: FIRST-PASS >R (RE-EXPR) R@ MOVE   0 (RE-EXPR) R> + C!   (RE-EXPR) ;

\ Everything to be initialised for a build.
: INIT-BUILD   FIRST-PASS NORMAL-CHARS!   SET-MATCHED!   RE-EXPR RE-FILLED ! ;

\ Everything to be harvested after a build.
: EXIT-BUILD   HARVEST-NORMAL-CHARS   HARVEST-CHAR-SET ;

\    -    -    -   --    -    -   -    -    -   -    -    -   -

\ For EP and CHAR : EP plus "it IS one of ^ $ with its special meaning".
\ ``EP'' points after ``CHAR'' in the re, and is of course needed to
\ determine this.
: ^$? DUP &^ = IF DROP RE-PATTERN 1+ OVER = ELSE
    &$ = IF OVER C@ 0= 0= ELSE FALSE THEN THEN ;

\ If the character at EP is normal, return incremented EP plus IT,
\ else EP plus FALSE. EP may be incremented past 2 char escapes!
: NORMAL-CHAR? C@+   DUP SPECIAL? 0= IF EXIT THEN
                     DUP ^$? IF EXIT THEN
                     \ Escapes not representing a character, may still represent a set.
                     &\ = IF C@+ GET-ESCAPE DUP IF EXIT ELSE 1- THEN THEN
                     1- FALSE ;

\ - - - - - - - - - - - - - - - - - - - - - - - -
\ Commands that get executed upon a special character
\ - - - - - - - - - - - - - - - - - - - - - - - -

\ Patch up the previous single character match with a quantifier.
: ADD*   MAKE-HOLE 'ADVANCE* ! ;
: ADD+   MAKE-HOLE 'ADVANCE+ ! ;
: ADD?   MAKE-HOLE 'ADVANCE? ! ;

\ Add specialties, more like markers.
: ADD<   'CHECK< RE, ;
: ADD>   'CHECK> RE, ;
: ADD(   'HANDLE() RE, ALLOCATE( RE, ;
: ADD)   'HANDLE() RE, ALLOCATE) RE, ;

: | OVER 2 SET+! ;
SET COMMAND-SET     COMMAND-SET !SET
&[ 'PARSE-CHAR-SET |   &< 'ADD< |   &> 'ADD> |
&( 'ADD( |   &) 'ADD) | &* 'ADD* |   &+ 'ADD+ |   &? 'ADD? |
'| HIDDEN

\ Execute the command that belongs to the abnormal CHARACTER.
: DO-ABNORMAL COMMAND-SET WHERE-IN-SET
             DUP 0= ABORT" Illegal escaped char for command, system error"
             CELL+ @ EXECUTE ;

\ Parse one element of regular EXPRESSION .
\ Leave EXPRESSION incremented past parsed part.
: BUILD-RE-ONE    NORMAL-CHAR? IF ADD-TO-NORMAL ELSE DO-ABNORMAL THEN ;

\ Parse the EXPRESSION string, put the result in the buffer
\ ``RE-PATTERN''.
: BUILD-RE INIT-BUILD BEGIN BUILD-RE-ONE C@ 0= UNTIL DROP INIT-BUILD ;
