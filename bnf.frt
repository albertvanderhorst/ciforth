( $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

( Environmental dependancies :
( This is a fig Forth Program using standard fig Words                  )
( plus LINOS (par1, par2, par3, function# -- result/error               )
( plus everything in the book available in the screens of a fully       )
( loaded lina system. Plus the BNF system.                              )

( ERROR 201 : file too large > SIZE , currently 1 Megabyte             )

(   \ is not known by figforth, it can be used with impunity outside    )
(   of  definitions because it only results in "\ MSG #0", but          )
(   anyway:                                                             )
: \ 0 WORD ; IMMEDIATE

( Run as follows:                                                       )
\   #! /bin/sh
\ (echo 8 LOAD ;echo; echo 180 LOAD; cat pascal.frt $1 - )|lina| tee aap$$

(   What is used in particular:                                         )
(   1. The width system to denote characters by &X                      )
(   2. The BNF system                                                   )
(   3. The ministring package                                           )

( ################# Lexical analyser #################################  )
(   Splits the input into tokens, skips comment.                        )

( ASCII values for some blank characters.                               )
9 CONSTANT TAB
10 CONSTANT LF
13 CONSTANT \r  ( The name CR is taken )

\   : TEST 0 WORD ;                                                      
  : TEST CR ." TESTING " LATEST ID. ; IMMEDIATE                        

( ---------- Some categories of symbols ------------------------------- )

(    For the CHARACTER supplied, return it IS blank space               )
: IS-BLANK
    DUP BL = >R
    DUP TAB = >R
    DUP LF = >R
    DUP \r = >R
    DROP
    R> R> R> R> OR OR OR ;

TEST ." EXPECT 1:" BL IS-BLANK . ^

(    For the CHARACTER supplied, return it IS a letter.                 )
: IS-LETTER $20 OR ( Convert to lower case)
    DUP &a 1 - > >R
    DUP &z 1 + < >R
    DROP
    R> R> AND ;

TEST ." EXPECT 1:" &A IS-LETTER . ^

(    For the CHARACTER supplied, return it IS a digit.                  )
: IS-DIGIT
    DUP &0 1 - > >R
    DUP &9 1 + < >R
    DROP
    R> R> AND ;

TEST ." EXPECT 1:" &1 IS-DIGIT . ^

(  For the CHARACTER supplied, return it IS a valid in an identifier.   )
: IS-IDENT
    DUP IS-DIGIT >R
    DUP IS-LETTER >R
    DUP &_ = >R
    DROP
    R> R> R> OR OR ;

TEST ." EXPECT 1:" &_ IS-IDENT . ^

(    For the CHARACTER supplied, return it IS valid in comment.         )
: IS-COMMENT
    DUP &} = 0= >R
    DUP 0= >R
    DROP
    R> R> OR ;

TEST ." EXPECT 1:" &{ IS-COMMENT . ^

(    For the CHARACTER supplied, return it IS valid in a string.        )
: IS-STRING
    DUP &' = 0= >R
    DUP 0= >R
    DROP
    R> R> OR ;

TEST ." EXPECT 1:" &{ IS-STRING . ^

(    For the CHARACTER supplied, return it IS valid throughout.        )
: IS-ANY ( I.e. non zero ) ;

TEST ." EXPECT 1:" &' IS-ANY . ^

(  For the above categories scan a single character.                    )
BNF: blank-symbol    @TOKEN IS-BLANK DUP SUCCESS ! +TOKEN ;BNF
BNF: letter-symbol   @TOKEN IS-LETTER DUP SUCCESS ! +TOKEN ;BNF
BNF: digit-symbol    @TOKEN IS-DIGIT DUP SUCCESS ! +TOKEN ;BNF
BNF: ident-symbol    @TOKEN IS-IDENT DUP SUCCESS ! +TOKEN ;BNF
BNF: string-symbol   @TOKEN IS-STRING DUP SUCCESS ! +TOKEN ;BNF
BNF: any-symbol      @TOKEN IS-ANY DUP SUCCESS ! +TOKEN ;BNF
BNF: comment-symbol  @TOKEN IS-COMMENT DUP SUCCESS ! +TOKEN ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! comment-symbol @ SUCCESS ? ^

( ------------------- Without syntactic meaning ----------------------- )
(   Scan blank space.                                                   )
BNF: blank   blank-symbol { blank-symbol } ;BNF
TEST ." EXPECT 0 1:" 1 SUCCESS ! blank 1 SUCCESS ? . ^
(   Scan comment.                                                       )
( Advance POINTER to point at a bracket or after the end, return IT     )
: SKIP-TILL-) BEGIN DUP C@   DUP 0= >R &) = R> OR 0= WHILE 1+ REPEAT ;
( Parse a star-bracket pair                                             )
: SKIP-TILL-*) 
    SUCCESS @ 0= IF EXIT THEN 
    TIB @ IN @ +
    BEGIN SKIP-TILL-)
        DUP C@ 0= IF 0 SUCCESS ! TIB @ - IN ! EXIT THEN
        DUP 1 - C@ &* = IF 1 SUCCESS ! 1 + TIB @ - IN ! EXIT THEN
    1+ AGAIN ;
TEST ." EXPECT 1 1:"  1 SUCCESS ! SKIP-TILL-*)   * )S ABCD*)1 . SUCCESS ? ^

BNF: comment   '{' { comment-symbol } '}' [ 'CR' ] 
              | '(' '*' SKIP-TILL-*) [ 'CR' ]        ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! comment {fred} SUCCESS ? ^
TEST ." EXPECT 1:" 1 SUCCESS ! comment (*fred*) SUCCESS ? ^

(  Skip anything that has no semantics.                                 )
(  This name is special, it resolves a forward reference, used in       )
(  KEYWORD .                                                            )
BNF: skip   {  blank | comment } ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! skip  { somecomment} {more{_1} SUCCESS ? ^
( Scan an identifier )
BNF: identifier   skip letter-symbol { ident-symbol } ;BNF
TEST ." EXPECT 1 1:" 1 SUCCESS ! 1 identifier  Sp_2 SUCCESS ? . ^
BNF: digit-sequence  digit-symbol { digit-symbol } ;BNF
TEST ." EXPECT 1 1:" 1 SUCCESS ! 1 digit-sequence 0234 SUCCESS ? . ^
BNF: character-string skip ''' { string-symbol | ''' ''' } ''' ;BNF
TEST ." EXPECT 1 1:" 1 SUCCESS ! 1 character-string 'ape''ape' SUCCESS ? . ^

( ---------- Some special symbols ------------------------------------ )

BNF:  `+'    skip '+'                ;BNF
BNF:  `?'    skip '?'                ;BNF
BNF:  `['    skip '['                ;BNF
BNF:  `]'    skip ']'                ;BNF
BNF:  `.'    skip '.'                ;BNF
BNF:  `,'    skip ','                ;BNF
BNF:  `:'    skip ':'                ;BNF
BNF:  `;'    skip ';'                ;BNF
BNF:  `'''   skip '''                ;BNF
BNF:  `('    skip '('                ;BNF
BNF:  `)'    skip ')'                ;BNF
BNF:  `^'    skip '^'                ;BNF
BNF:  `='    skip '='                ;BNF
BNF:  `<'    skip '<'                ;BNF
BNF:  `>'    skip '>'                ;BNF
BNF:  `-'    skip '-'                ;BNF
BNF:  `#'    skip '#'                ;BNF
BNF:  `*'    skip '*'                ;BNF
BNF:  `/'    skip '/'                ;BNF
BNF:  `e'    skip [ 'e' | 'E' ]      ;BNF

BNF:  `**'   skip '*' '*'            ;BNF
BNF:  `<>'   skip '<' '>'            ;BNF
BNF:  `<='   skip '<' '='            ;BNF
BNF:  `>='   skip '>' '='            ;BNF
BNF:  `:='   skip ':' '='            ;BNF
BNF:  `..'   skip '.' '.'            ;BNF
BNF:  `><'   skip '>' '<'            ;BNF
BNF:  `=>'   skip '=' '>'            ;BNF

TEST ." EXPECT 1 2:" 1 SUCCESS ! `=>' =>2 SUCCESS ? . ^

( ---------- The keywords -------------------------------------------- )
KEYWORD  `and'        and
KEYWORD  `array'      array
KEYWORD  `begin'      begin
KEYWORD  `bindable'   bindable
KEYWORD  `case'       case

KEYWORD  `const'      const
KEYWORD  `div'        div
KEYWORD  `do'         do
KEYWORD  `downto'     downto
KEYWORD  `else'       else
KEYWORD  `end'        end
KEYWORD  `export'     export

KEYWORD  `file'       file
KEYWORD  `for'        for
KEYWORD  `function'   function
KEYWORD  `goto'       goto
KEYWORD  `if'         if
KEYWORD  `import'     import

KEYWORD  `in'         in
KEYWORD  `label'      label
KEYWORD  `mod'        mod
KEYWORD  `module'     module
KEYWORD  `nil'        nil
KEYWORD  `not'        not
KEYWORD  `of'         of

KEYWORD  `only'       only
KEYWORD  `or'         or
KEYWORD  `otherwise'  otherwise
KEYWORD  `packed'     packed
KEYWORD  `pow'        pow

KEYWORD  `procedure'  procedure
KEYWORD  `program'    program
KEYWORD  `protected'  protected
KEYWORD  `qualified'  qualified

KEYWORD  `record'     record
KEYWORD  `repeat'     repeat
KEYWORD  `restricted' restricted
KEYWORD  `set'        set
KEYWORD  `then'       then
KEYWORD  `to'         to

KEYWORD  `type'       type
KEYWORD  `until'      until
KEYWORD  `value'      value
KEYWORD  `var'        var
KEYWORD  `while'      while
KEYWORD  `with'       with

TEST ." EXPECT 1:" 1 SUCCESS ! `with'    {follows with} with SUCCESS @ 0= 0= . ^

BNF: `or_else'   `or' `else'  ;BNF
BNF: `and_then'  `and' `then' ;BNF



( ################# Input table ####################################### )

1000000 CONSTANT SIZE
0 VARIABLE FILE-BUFFER SIZE ALLOT

5 CONSTANT OPEN
3 CONSTANT READ
6 CONSTANT CLOSE
0 CONSTANT O_RDONLY

0 VARIABLE HANDLE
0 VARIABLE FILE-NAME 255 ALLOT
" paranoia.pas" FILE-NAME $! 0 FILE-NAME $C+

( Convert the STRING variable naar een Unix ZERO-ENDED-STRING )
: ZERO-ENDED COUNT OVER + 0 SWAP C! ;

( Open the file from `FILE-NAME' such that `HANDLE' becomes available )
: OPEN-LINUX FILE-NAME ZERO-ENDED O_RDONLY 0 OPEN LINOS
    DUP ?LINUX-ERROR HANDLE ! ;

( Turn `FILE-BUFFER' into a good input buffer, zero ended,               )
(   containing the file.                                                 )
: READ-FILE HANDLE @ FILE-BUFFER SIZE READ LINOS
    DUP ?LINUX-ERROR   DUP SIZE = 201 ?ERROR
    FILE-BUFFER + 0 SWAP !     ;

( Close the file again )
: CLOSE-FILE HANDLE @ 0 0 CLOSE LINOS ?LINUX-ERROR ;

( Get the file to the buffer )
: GET-FILE OPEN-LINUX READ-FILE CLOSE-FILE ;

( Prints the status of the parsing and the name of the executing        )
( word from its PARAMETER field. Intended to run at the end of a bnf.   )
: END-REPORT   CR ID. &: EMIT SUCCESS @ IF
     ."  MATCHED with ..." IN @ 20 - 0 MAX TIB @ + 20 TYPE &| EMIT 
   ELSE DROP ." FAILED" THEN ;

( Adorn the ;BNF word with a debug report. )
TEST : ;BNF 
TEST     LATEST [COMPILE] LITERAL COMPILE END-REPORT [COMPILE] ;BNF
TEST ; IMMEDIATE

( Prints what we are going to parse and the name of the executing       )
( word from its PARAMETER field. Intended to run at the start of a bnf. )
: START-REPORT CR ID. &: EMIT 
    ."  trying to match |" TIB @ IN @ + 20 TYPE ." ...  at "  IN ? ; 

( Adorn the BNF: word with a debug report. )
TEST : BNF: 
TEST     [COMPILE] BNF:
TEST     LATEST [COMPILE] LITERAL COMPILE START-REPORT 
TEST ; IMMEDIATE

( ################# Simple renames ####################################### )
( This must be done at the last minute, because it hides the Forth      )
( comment sign.                                                         )

: (   [COMPILE] (( ;  IMMEDIATE
: )   [COMPILE] )) ;  IMMEDIATE
: digit COMPILE digit-symbol ; IMMEDIATE
: letter COMPILE letter-symbol ; IMMEDIATE


