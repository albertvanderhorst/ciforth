Q $Id$ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

( Environmental dependancies : )
( This is a fig Forth Program using standard fig Words                  )
( plus LINOS (par1, par2, par3, function# -- result/error )             )
( plus everything in the book available in the screens of a fully       )
( loaded lina system. Plus the BNF system.                              )
(   \ is not known by figforth, it used with impunity outside of        )
(   definitions because it only results in \ MSG #0                     )

( Run as follows:                                                       )
\   #! /bin/sh
\   (echo 8 LOAD 180 LOAD ; cat pascal.frt $1 - )|lina| tee aap$$

(   What is used in particular:                                         )
(   1. The width system to denote characters by &X                      )
(   2. The BNF system                                                   )
(   3. The ministring package                                           )

180 LOAD

( ################# Lexical analyser #################################  )
(   Splits the input into tokens, skips comment.                        )

( ASCII values for some blank characters.                               )
9 CONSTANT TAB
10 CONSTANT LF
13 CONSTANT \r  ( The name CR is taken )

: TEST CR ." TESTING " LATEST ID. ;

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
: IS-FOLLOW
    DUP IS-DIGIT >R
    DUP IS-LETTER >R
    DUP &_ = >R
    DROP
    R> R> R> OR OR ;

TEST ." EXPECT 1:" &_ IS-FOLLOW . ^

(    For the CHARACTER supplied, return it IS valid in comment.         )
: IS-COMMENT
    DUP &} = 0= >R
    DUP 0= >R
    DROP
    R> R> OR ;

TEST ." EXPECT 1:" &{ IS-COMMENT . ^

(  For the above categories scan a single character.                    )
BNF: blank-symbol    @TOKEN IS-BLANK DUP SUCCESS ! +TOKEN ;BNF
BNF: letter-symbol   @TOKEN IS-LETTER DUP SUCCESS ! +TOKEN ;BNF
BNF: digit-symbol    @TOKEN IS-DIGIT DUP SUCCESS ! +TOKEN ;BNF
BNF: follow-symbol   @TOKEN IS-FOLLOW DUP SUCCESS ! +TOKEN ;BNF
BNF: comment-symbol   @TOKEN IS-COMMENT DUP SUCCESS ! +TOKEN ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! comment-symbol @ SUCCESS ? ^

(   Scan blank space.                                                   )
BNF: blank   blank-symbol { blank-symbol } ;BNF
TEST ." EXPECT 0 1:" 1 SUCCESS ! blank 1 SUCCESS ? . ^
(   Scan comment.                                                       )
BNF: comment   '{' { comment-symbol } '}' [ 'CR' ] ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! comment {fred} SUCCESS ? ^
(  Skip anything that has no semantics.                                 )
(  This name is special, it resolves a forward reference, used in       )
(  KEYWORD .                                                            )
BNF: skip   { blank | comment } ;BNF
TEST ." EXPECT 1:" 1 SUCCESS ! skip {somecomment}  { { more _1 comment } SUCCESS ? ^
BNF: identifier   skip letter-symbol { follow-symbol } ;BNF
TEST ." EXPECT 1 1:" 1 SUCCESS ! 1 identifier     Spartacus_in_circus_2 SUCCESS ? . ^

KEYWORD `if' if
TEST ." EXPECT 1:" 1 SUCCESS ! `if'    {follows if} if SUCCESS @ 0= 0= . ^

