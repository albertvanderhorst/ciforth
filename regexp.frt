
REQUIRE TRUE

: /STRING >R R@ - SWAP R@ + SWAP RDROP ;

\ In the expression '$' is used to denoted the end of the record, e.g.
\ "\n\n\n$[^\n]" means a record ends in two empty lines, but the
\ following line must not be empty.
\ A consequence is that a real '$' must be escaped.
\ So escaped are '\\','\$','\t' and '\n'

\ ***************************************************************************/
\                                                                           */
\   Note: SeparatorE[n] may contain either a character, or one of the       */
\   explicit enumerations following, or a closure (CCHAR|CLOS), or a        */
\   count. This depends from the order.                                     */
\   Instead of using a union, we make it an integer area and rely as        */
\   much as possible on safe conversions.                                   */
\                                                                           */
\ ***************************************************************************/

\ Auxiliary string: compiled expression.
\ Contains a mix of ``charclass'' enums, counts and ``CHAR''.
\ All three types are subtypes of ``VARIABLE''.
1000 CONSTANT lenpatternE
CREATE patternE lenpatternE CHARS ALLOT

\ Points at where the next fields start in the buffer
\ Stored outside of recursively called routines
\ seppositionE[0] field that starts at '^' or before the first '$'
\ seppositionE[i] field that begin with i-th '$'
\ seppositionE[maxsep] start of next record,
\ lines up next time with seppositionE[0]
CREATE seppositionE 1000 CELLS ALLOT
\ Store X at POSITION in seppositionE .
: !SEPPOSITION CELLS seppositionE + ! ;

\ ``maxfield'' is the length of this array
\ it indicates where the one and only record starts and ends
VARIABLE maxfield

\ Return the CONTENT of ``maxfield'' and bump it by one.
: maxfield++ maxfield @ 1 maxfield +! ;

VARIABLE  fieldpointer

\ The end of the buffer where a match is to be found.
\ As yet we use a 0 to mark the end of the buffer.
VARIABLE         limitE

2    CONSTANT CCHAR      \ Specific character
4    CONSTANT CANY       \ Any character
6    CONSTANT CSET       \ A character set
8    CONSTANT CSETC      \ A character set, characterized by its complement
01   CONSTANT CLOS       \ And this in to get the closure of one of the above
10   CONSTANT CEOF       \ Marks the separation between fields
12   CONSTANT CSYNC      \ Match as few characters as possible
0    CONSTANT CEND       \ Marks the end of the regular expression

\ Layout of expression in ``patternE''.
\ NOTE: THIS IS A SEQUENTIAL LAYOUT!
\ It consists of the following elements (separated by ',' | is bitwise or )
\ CCHAR c , CLOS|CCHAR c , CANY , CLOS|CANY , CEOF #field ,
\ CSET count (count-1)*c , CSETC count (count-1)*c ,
\ CLOS|CSET count (count-1)*c , CLOS|CSETC count (count-1)*c ,
\ CSYNC , CEND
\ ``c'' means any character except null ('\000'). Null is not allowed,
\ because it must not be present in the expression to be matched, it is
\ needed as a terminator. ``count'' and ``#field'' are integers.
\ #field 0 matches the beginning of the record (say '^') and 1 ..
\ matches the field spearators ('$' of implied in '@'). The last field
\ separator is the CEOF 0 of the next record.
\ CEOF and CEND are restricted
\ Note: the position in ``patternE'' determines whether it is a
\ ``charclass'', a count or a character. If sequentially parsed this
\ does not lead to ambiguity despite overlapping ranges
\ (All unsigned char's are acceptable in the expression as c.)


\ ***************************************************************************/
\                                                                           */
\  Compile the string ``s'' into ``patternE' with the tricky format         */
\  described above. This is in order to be able to swiftly parse text       */
\  searching for regular expressions.                                       */
\                                                                           */
\ ***************************************************************************/

: !++ OVER C! 1+ ;

VARIABLE lastep

: compileE ( STRING s )
\    CHAR c;
    patternE
\    \ Pointers into source string
\
\    VARIABLE *lastep;   \ Points to a character class that could be closed
\    \ Counts the members of a class (between [ ])  == #members +1 (!!)
\    VARIABLE cclcnt;
\
\    \ Initialize the separator counter
     0 maxfield !
     0 lastep !
\
\    \ The '^' at the beginning has a special meaning (see manual)
     OVER C@ &^ = IF CSYNC !++ THEN

     BEGIN OVER C@ DUP WHILE

        OVER patternE lenpatternE + < 0= 1023 AND THROW
        DUP &. = IF
           DUP lastep !   CANY !++
        ELSE DUP &* = IF
            lastep @ 0= 1024 AND THROW
\           ssort specific :
\             lastep C@ CANY <> IF CLOS lastep TOGGLE ELSE CSYNC LASTEP ! THEN
            lastep CLOS TOGGLE   0 lastep !
        ELSE DUP &$ = IF DROP
\           ssort specific, normally this would be accepted at the end only
           0 lastep !   CEOF !++   maxfield++ !++
        ELSE DUP &^ = IF DROP
           OVER patternE 1+ <> 1025 AND THROW
           0 lastep !   CEOF !++   0 !++
        ELSE DUP &[ = IF DROP
            DUP lastep !
           \ Consume '^' if it is a negator, not a character
           OVER C@ &^ = IF SWAP 1+ SWAP CSETC ELSE CSET THEN
           !++
           0 !++
\          \ Put characters in set, at least one. So do{ }while.
           OVER C@ 1 >R
           BEGIN
              !++
              R> 1+ >R
\             assert( ep<patternE+lenpatternE );
\             SWAP NEXTCHAR DUP &[ <> WHILE
              SWAP COUNT DUP &] <> WHILE
              >R SWAP R>
           REPEAT
           R> lastep @ 1+ C!
\         ELSE DUP &@ = IF DROP  \ ssort specific
\       case '@':
\          c = nextchar(sp);
\          lastep = NULL;        \ No closure possible
\          *ep++ = CSETC|CLOS;   \ A complement set
\          *ep++ = 2;            \ With one character excluded:
\          *ep++ = c;            \   the field separator
\          *ep++ = CCHAR;        \ Followed by a single char:
\          *ep++ = c;            \   the field separator again
\          *ep++ = CEOF;         \ Mark the field separation point
\          *ep++ = ++maxfield;
\          break;
\
          ELSE DUP &\ = IF DROP
             OVER C@
             DUP 0= 1026 AND THROW
             >R
             DUP lastep !
             CCHAR !++
             R> !++
          ELSE >R
             DUP lastep !
             CCHAR !++
             R> !++
          THEN THEN THEN THEN THEN
      REPEAT DROP
     \ Add default field marker if none found as yet
      maxfield @ 0= IF
         CEOF !++ SWAP
         maxfield++ !++ SWAP
     THEN
\    assert( ep<patternE+lenpatternE );
     \ End of input string: close compiled expression

      CEND !++

      2DROP
;

\ ***************************************************************************/
\                                                                           */
\   ``PrintSingleCharE'' prints the single character ``c''. It need not be  */
\   a printable character, but it may not be '\000'                         */
\                                                                           */
\ ***************************************************************************/

\ void Separators::PrintSingleCharE(CHAR c )
\ {
\    switch( c )
\    {
\       case '*'       : cout<<"'\\ ' ";         break;
\       case '@'       : cout<<"'\\@' ";         break;
\       case '$'       : cout<<"'\\$' ";         break;
\       case '\b'       : cout<<"'\\b' ";         break;
\       case '\f'       : cout<<"'\\f' ";         break;
\       case '\n'       : cout<<"'\\n' ";         break;
\       case '\t'       : cout<<"'\\t' ";         break;
\       default         : if ( '\000' != c )
\                            cout<<"'"<<c<<"' ";
\                         else
\                            cout<<"No character where expected\n";
\                         break;
\    }
\ }
\

\ ***************************************************************************/
\                                                                           */
\  Decipher the string in ``patternE '' to reveal the tricky format partly  */
\  described above. This is a debugging function.                           */
\                                                                           */
\ ***************************************************************************/

\ void Separators::decipherE( void )
\ {
\    VARIABLE *sp = Separators:: patternE;
\    CHAR c;
\    VARIABLE i;
\    bool tobeclosed;
\
\    \ Print hex versions of characters
\    while ( *sp || CEOF==sp[-1]) cout<<setw(2)<<setfill('0')<<hex<<*sp++<<' ';
\
\    \ Print interpreted versions of chars
\    cout<<"   ";
\    sp = patternE;
\
\    while ( CEND != *sp )
\    {
\       tobeclosed = false;
\       switch( c= *sp++ )        \ Note all the fall throughs!
\       {
\       case CEOF       : cout<<(*sp++?" $ ":" ^ ");
\                         break;
\
\       case CCHAR|CLOS : tobeclosed = true;
\       case CCHAR      : PrintSingleCharE( c= *sp++);
\                         break;
\
\       case CANY|CLOS  : tobeclosed = true;
\       case CANY       : cout<<" . ";
\                         break;
\
\       case CSYNC      : cout<<" ... ";
\                         break;
\
\       case CSETC|CLOS : tobeclosed = true;
\       case CSETC      : cout<<"~";
\                         goto set;
\
\       case CSET|CLOS  : tobeclosed = true;
\       case CSET       : goto set;
\
\       set             : cout<<"[";
\                         i = *sp++ -1;
\                         if (i<=0) cout<<"illegal length in set\n";
\                         while ( i-- )  PrintSingleCharE( c= *sp++);
\                         cout<<"]";
\                         break;
\
\       default         : cout<<"wrong format\n";
\                         break;
\       }
\       if ( tobeclosed ) cout<<"* ";
\    }
\    cout<<"\n";
\ }


\ ***************************************************************************/
\                                                                           */
\  Returns "``ac'' is a member of the set ``set''".                         */
\  ``set'' is a part of the compiled expression representing a "class"      */
\  of characters, i.e. a set between '[' and ']'.                           */
\  Structure of ``set'' : count, (count-1) characters.                      */
\                                                                           */
\ ***************************************************************************/

: ismemberofsetE ( VARIABLE *set  CHAR c -- fl )
  >R
  COUNT 1-
  BEGIN DUP WHILE
      OVER 1+ C@ &- = OVER 2 > AND IF
         OVER DUP C@ SWAP 2 + C@ 1+ C@ ROT ROT WITHIN IF
            2DROP RDROP TRUE EXIT
         THEN
         3 /STRING
      ELSE
         OVER C@ R@ = IF 2DROP RDROP TRUE EXIT THEN
         1 /STRING
      THEN
   REPEAT
   2DROP RDROP FALSE
;

\ ***************************************************************************/
\                                                                           */
\  Returns "the string at ``lp'' matches the whole of the regular           */
\  expression ``ep''"                                                       */
\                                                                           */
\ ***************************************************************************/

: matchE            ( CHAR *lp   VARIABLE *ep -- fl )
   \ Each pass through this loop handles an element of the compiled
   \ expression (where ``ep'' points), including its closure (if any).
   \ Returns as soon as a mismatch is detected.
   BEGIN
      COUNT
      DUP CEND = IF DROP 2DROP TRUE EXIT
      ELSE DUP CEOF = IF DROP
         OVER >R COUNT  R> !SEPPOSITION
         0 >R
      ELSE DUP CCHAR = IF DROP
         \ Special action needed for matchee exhausted
         COUNT >R SWAP COUNT R> <> IF 2DROP FALSE EXIT THEN
         SWAP
         0 >R
      ELSE DUP CCHAR CLOS OR = IF DROP
         OVER >R
         COUNT >R SWAP
         BEGIN DUP C@ R@ = WHILE 1+ REPEAT RDROP
      ELSE DUP CANY = IF DROP
         SWAP COUNT 0= IF 2DROP FALSE EXIT THEN
         SWAP
         0 >R
      ELSE DUP CSYNC = IF DROP
         \ Special action needed for matchee exhausted
         BEGIN OVER C@ WHILE
            \ Adaption for 3 param matchE
            2DUP RECURSE IF 2DROP TRUE EXIT THEN
            SWAP 1+ SWAP
         REPEAT
         2DROP FALSE EXIT
      ELSE DUP CSET = IF DROP
         >R
         R@ C@ ismemberofsetE 0= IF RDROP 2DROP FALSE EXIT THEN
        COUNT + R> SWAP
         0 >R
      ELSE DUP CSET CLOS OR = IF DROP
         OVER >R
         >R
         BEGIN DUP R@ C@ ismemberofsetE WHILE R> 1+ >R REPEAT
        COUNT + R> SWAP
      ELSE DUP CSETC = IF DROP
         \ Special action needed for matchee exhausted
         OVER C@ 0= IF 2DROP FALSE EXIT THEN
         >R
         R@ C@ ismemberofsetE IF RDROP 2DROP FALSE EXIT THEN
        COUNT + R> SWAP
         0 >R
      ELSE DUP CSETC CLOS OR = IF DROP
         OVER >R
         >R
         BEGIN DUP R@ C@ ismemberofsetE 0= WHILE R> 1+ >R REPEAT
        COUNT + R> SWAP
      THEN THEN THEN THEN THEN THEN THEN THEN THEN THEN

         \ `lp' points to the first non-matching character
         \ At this point we have matched as much patterns as possible
         \ Backtrack if we have matched too much
         R@ IF
             BEGIN
                \ Adaption for 3 param matchE
                2DUP RECURSE IF RDROP 2DROP TRUE EXIT THEN
                OVER R@ > WHILE
                SWAP 1- SWAP
             REPEAT
             2DROP RDROP FALSE EXIT
         THEN
         RDROP
   AGAIN
;

EXIT

\ ***************************************************************************/
\                                                                           */
\   Return the start of the next record from buf[0..len-1].                 */
\   Records are supposed to end with the first match with the regular       */
\   expression that should have been compiled into `patternE' at the        */
\   installation time of this method. The precise separation of records is  */
\   where the expression contains the separation marker '$'.                */
\   Returns 0 if no match found. This may be caused by the buffer being     */
\   exhausted. The caller may decide to refill the buffer and go on. There  */
\  is a problem, if there could have been a match with more, e.g. a pattern */
\  ending in "\n*$" or any star. The caller can not easily detect this,     */
\  because it gets the fields served one at a time but it has too.          */
\                                                                           */
\ ***************************************************************************/

CHAR * Separators::findseparatorE(CHAR *buf ,  VARIABLE len )
{
   limitE = buf+len;

   if ( fieldpointer==maxfield ) fieldpointer = 0;
   if ( 0==fieldpointer )
   {
      seppositionE[maxfield] = NULL;   \ Reset

      \ Consider the pattern matched if
      \    1. matchE returned TRUE          or
      \    2. all fields matched and no context present after that
      if ( !matchE( buf, patternE ) &&
            seppositionE[maxfield] != buf+len )
         return 0;
   }
   return seppositionE[++fieldpointer];
}
