\ Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License
\ $Id$

\ Diagnostic program.
\ Asks questions from the user to arrive at a diagnosis.
\ Example question: "Can it fly"
\ For example diagnose "horse" the answer most users give is probably "no".

\ #################### FILE ###########################################

\ File format :
\    number of diagnoses  ND
\    diagnose 1
\    ....
\    diagnose ND
\  number of questions NQ
\    question 1
\    ....
\    question NQ
\    Answer acceptable for type 1
\    ND answers for question 1
\ ......
\    ND answers for question NQ
\    Answer type 2
\    ND answers for question 1
\ ......
\    ND answers for question NQ      ^
\ ... all answers


VARIABLE #DIAGNOSES \ Number of diagnoses
VARIABLE #QUESTIONS \ Number of questions


\ Converts the STRING to a NUMBER. If there is more, throw.
: atoi -TRAILING 0. 2SWAP >NUMBER 1001 ?ERROR DROP DROP ;

\ From STRING split off N lines and put them in an array.
\ Leave REMAINDER.
\ The created word turns an INDEX into the ADDRESS of a string pointer.
: $ARRAY CREATE 0 DO ^J $S , , LOOP 100 ALLOT DOES> SWAP 2* CELLS + ;

\ STRING contains N numbers, else throw. Put the numbers in the dictionary.
: GET-NUMBERS 0 DO BL $S DUP 0= 1002 ?ERROR atoi , LOOP 2DROP ;

\ Create from a STRING an array of answers ``#ANSWERS'' by ``#QUESTIONS''
\ The answers for the same question are together on one line.
\ Leave REMAINDER.
\ The created word turns an INDEX into an ADDRESS.
: ANSWER-ARRAY
CREATE #DIAGNOSES @ 0 DO ^J $S #QUESTIONS @ GET-NUMBERS LOOP
DOES> SWAP CELLS + ;


"database" GET-FILE

    ^J $S atoi    DUP #DIAGNOSES !  $ARRAY DIAGNOSES
    ^J $S atoi    DUP #QUESTIONS !  $ARRAY QUESTIONS

    ^J $S CR TYPE   \ Show potential problems.

    ANSWER-ARRAY YESSES

    ^J $S CR TYPE   \ Show potential problems.

    ANSWER-ARRAY NOES

    ^J $S CR TYPE   \ Show potential problems.

    ANSWER-ARRAY ?ES

2DROP

\ Add STRING as a diagnosis.
: ADD-DIAGNOSIS $, $@ #DIAGNOSES @ DIAGNOSES 2! 1 #DIAGNOSES +! ;

\ Add STRING as a QUESTION.
: ADD-QUESTION $, $@ #QUESTIONS @ QUESTIONS 2! 1 #QUESTIONS +! ;

: (U.) 0 <# #S #> ;

\ Receive the output database.
VARIABLE OUTPUT$

\ Add a carriage return to the output.
: cr ^J OUTPUT$ @ $C+ ;

\ Add a blank to the output.
: bl BL OUTPUT$ @ $C+ ;

\ Add a NUMBER to the output.
: u. (U.) OUTPUT$ @ $+!    ;

\ Add the string of an ARRAY of N pointers to strings to the output.
\ The array is passed as an xt.
: .dot.   0 DO I OVER EXECUTE 2@ OUTPUT$ @ $+!  cr LOOP DROP ;

\ Add from an ARRAY of N numbers to a line in the output.
: PUT-NUMBERS 0 DO I CELLS OVER + @ u. bl LOOP cr DROP ;

: uk "------" OUTPUT$ @ $+! cr ;

\ Add an ARRAY of answers ``#ANSWERS'' by ``#QUESTIONS'' to the output.
: PUT-ANSWERS
    uk
    #DIAGNOSES @ 0 DO
        DUP #QUESTIONS @ PUT-NUMBERS #QUESTIONS @ CELLS +
    LOOP DROP ;

\ Write the database to the file "database2"
: WRITE-DATABASE
    HERE OUTPUT$ ! 0 , 10,000,000 ALLOT

    #DIAGNOSES @ DUP u. cr   'DIAGNOSES SWAP .dot.
    #QUESTIONS @ DUP u. cr   'QUESTIONS SWAP .dot.

    0 YESSES  PUT-ANSWERS
    0 NOES    PUT-ANSWERS
    0 ?ES     PUT-ANSWERS
    OUTPUT$ @ $@ "database2" PUT-FILE
;
