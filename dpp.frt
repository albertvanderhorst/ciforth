\ Copyright (2001): Albert van der Horst, HCC FIG Holland by GNU Public License
\ $Id$

\ Diagnostic program.
\ Asks questions from the user to arrive at a diagnosis.
\ Example question: "Can it fly"
\ For example diagnose "horse" the answer most users give is probably "no".

\ To be done:
\ 1. Case insensitive answers
\ 2. Clean first part of new question, start with upper case etc.
\ 3. Provide for unreadable database (or handle with THROW).
\ Not in old version
\ 101. Internet interface.
\ 102. Logging of answer vectors for disapproval.

\ #################### DATABASE #######################################

: \D ; \ Debug

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

"dstring.frt" INCLUDED

VOCABULARY DATABASE
DATABASE DEFINITIONS

VARIABLE #DIAGNOSES \ Number of diagnoses
VARIABLE #QUESTIONS \ Number of questions
\ SPARE diagnoses and SPARE questions can be added in one session.
50 CONSTANT SPARE

\ Converts the STRING to a NUMBER. If there is more, throw.
: atoi -TRAILING 0. 2SWAP >NUMBER 1001 ?ERROR DROP DROP ;

\ From STRING split off N lines and put them in an array.
\ Leave REMAINDER.
\ The created word turns an INDEX into the ADDRESS of a string pointer.
: $ARRAY    CREATE 0 DO ^J $S , , LOOP SPARE 2* CELLS ALLOT
            DOES> SWAP 2* CELLS + ;

\ STRING contains N numbers, else throw. Put the numbers in the dictionary.
: GET-NUMBERS 0 DO BL $S DUP 0= 1002 ?ERROR atoi , LOOP 2DROP ;

\ Create from a STRING an array of answers ``#ANSWERS'' by ``#QUESTIONS''
\ The answers for the same question are together on one line.
\ Leave REMAINDER.
\ The created word turns an INDEX into an ADDRESS.
: ANSWER-ARRAY
CREATE #DIAGNOSES @ 0 DO ^J $S #QUESTIONS @ GET-NUMBERS SPARE CELLS ALLOT LOOP
       #QUESTIONS @ SPARE + SPARE * CELLS ALLOT
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

\ Upper limits for arrays
#DIAGNOSES @ SPARE + CONSTANT MAX-DIAGNOSES
#QUESTIONS @ SPARE + CONSTANT MAX-QUESTIONS

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

\ Add the strings of an ARRAY of N pointers to strings to the output.
\ The array is passed as an xt.
: .$ARRAY   0 DO I OVER EXECUTE 2@ OUTPUT$ @ $+!  cr LOOP DROP ;

\ Add from an ARRAY of N numbers to a line in the output.
: PUT-NUMBERS 0 DO I CELLS OVER + @ u. bl LOOP cr DROP ;

: uk "------" OUTPUT$ @ $+! cr ;

\ Add an ARRAY of answers ``#ANSWERS'' by ``#QUESTIONS'' to the output.
: PUT-ANSWERS
    uk
    #DIAGNOSES @ 0 DO
        DUP #QUESTIONS @ PUT-NUMBERS MAX-QUESTIONS CELLS +
    LOOP DROP ;

ONLY FORTH DEFINITIONS
DATABASE

\ Write the database to the file "database2"
: WRITE-DATABASE
    HERE OUTPUT$ ! 0 , 10,000,000 ALLOT

    #DIAGNOSES @ DUP u. cr   'DIAGNOSES SWAP .$ARRAY
    #QUESTIONS @ DUP u. cr   'QUESTIONS SWAP .$ARRAY

    0 YESSES  PUT-ANSWERS
    0 NOES    PUT-ANSWERS
    0 ?ES     PUT-ANSWERS
    OUTPUT$ @ $@ "database2" PUT-FILE
;

\ For INDEX get the diagnostic STRING.
: DIAG DIAGNOSES 2@ ;

\ For INDEX get the diagnostic STRING.
: QUES QUESTIONS 2@ ;
PREVIOUS

\ #################### INTERACTION ####################################

VOCABULARY INTERACTION
INTERACTION DEFINITIONS

\ Possible values for what we call an answer.
0 CONSTANT A_YES
1 CONSTANT A_NO
2 CONSTANT A_AMB
3 CONSTANT A_NONE       \ No answer yet.

\ Convert a STRING to an ANSWER.
: TO-ANSWER 0= IF DROP A_NONE EXIT THEN
   C@
   Yes$ DROP C@ OVER = IF DROP A_YES EXIT THEN
   No$  DROP C@ OVER = IF DROP A_NO  EXIT THEN
   Amb$ DROP C@ OVER = IF DROP A_AMB EXIT THEN
   DROP A_NONE
;

\ Print a STRING as a question.
: .QUESTION CR 4 SPACES TYPE SPACE &? EMIT CR ;

\ For a question STRING get an ANSWER.
: (GET-ANSWER) .QUESTION (ACCEPT) TO-ANSWER ;

\ Disapprove ANSWER , tell user so.
: DISAPPROVE DROP NotAnAnswer$ TYPE ;

\ For a question STRING get an ANSWER. ``A_NONE'' is not accepted.
: GET-ANSWER
    BEGIN 2DUP (GET-ANSWER) DUP 3 = WHILE DISAPPROVE REPEAT >R 2DROP R>
;

\ For a STRING return a clean STRING, without a possible question mark.
: CLEAN-STRING -TRAILING 2DUP + 1- C@ &? = IF 1- THEN ;
\D "How ? " CLEAN-STRING ." EXPECT |How | :" &| EMIT TYPE &| EMIT

PREVIOUS DEFINITIONS

VOCABULARY D-MAIN
D-MAIN DEFINITIONS   DATABASE INTERACTION

\ Exit to linux with whatever ERROR was caught, hopefuly zero.
: ERROR-BYE NEGATE 0 0 1 LINOS ;

\ Do whatever need to be done if the user wants to stop.
: FINISH
     QuerySave$ GET-ANSWER A_NO = IF EXIT THEN
     Save$ TYPE CR 'WRITE-DATABASE CATCH ERROR-BYE
;
ONLY FORTH DEFINITIONS
