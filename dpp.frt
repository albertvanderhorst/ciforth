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

\ #################### CONFIGURATION ##################################

\ Leave a FRACTION (numerator/denominator) that decides whether
\ the difference between occurences of answers is significant.
: CRITERION 5 1 ;
\ This means yes-answers must outnumber the no-answers 5 to 1, or vv.

\ On a scale from 0 to 1000, the tendency to ask questions that
\ learn at the expense of faster retrieval.
: CURIOSITY 720 ;

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

\ The DISTANCE in address units between question for different diagnosis
: STRIDE #QUESTIONS @ SPARE + CELLS ;
\ Create from a STRING an array of answers ``MAX-DIAGNOSIS'' by ``MAX-QUESTIONS''
\ The answers for the same question are together on one line.
\ Leave REMAINDER.
\ The created word turns an DIAGNOSIS and QUESTION into an ADDRESS.
: ANSWER-ARRAY
CREATE #DIAGNOSES @ 0 DO ^J $S #QUESTIONS @ GET-NUMBERS SPARE CELLS ALLOT LOOP
       STRIDE SPARE * ALLOT
DOES> ROT STRIDE * + SWAP CELLS + ;


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
\D CR ." Expect 0 : " DEPTH . CR
\D 0 0 YESSES ." YESSES Expect 0 0 :" ? DEPTH .  CR
\D 0 0 NOES   ." NOES   Expect 1 0 :" ? DEPTH .  CR
\D 1 0 NOES   ." NOES   Expect 1 0 :" ? DEPTH .  CR
\D 1 1 NOES   ." NOES   Expect 1 0 :" ? DEPTH .  CR

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

\D "How ? " CLEAN-STRING
\D ." Expect |How |0 :" &| EMIT TYPE &| EMIT DEPTH . CR


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


VOCABULARY STRATEGY
STRATEGY DEFINITIONS

\ Divide a DOUBLE by a SINGLE, leaving a SINGLE quotient.
: M/ SM/REM SWAP DROP ;

\D ." Expect 12 0 : " 1234. 100 M/ . DEPTH . CR

\ Subtract from DOUBLE another DOUBLE.
: D- DNEGATE D+ ;
\D ." Expect 1200 : " 1234. 34. D- D. CR

: DOUBLE CREATE 2 CELLS ALLOT DOES> ;
\D ." Expect 1234 : " DOUBLE MONKEY 1234. MONKEY 2! MONKEY 2@ D. CR
\ D FORGET MONKEY

VARIABLE C_UNKNOWN  \ The number of questions that never got a definite answer
VARIABLE C_YES      \ The number of questions that got a yes answer
VARIABLE C_NO      \ The number of questions that got a no answer
VARIABLE C_AMB      \ The number of questions that is considered ambiguous


\ ##################################################################
\ NOTE: For the next words answers must have been ``ACCUMULATE''d.
\ ##################################################################

\ Return the total NUMBER of unambiguous questions
: C_UNAMB C_YES @ C_NO @ + ;
\ Return the total NUMBER of questions of the current focus.
: C_TOTAL C_UNAMB C_AMB @ + C_UNKNOWN @ + ;

: C_UN_CUR*1000 C_UNKNOWN @ CURIOSITY M* ;
\ Return the NUMBER of unambiguous questions where we count with.
\ Unknown questions are counted as unambiguous depending on curiosity.
: C_UN_CUR C_UN_CUR*1000 1000 M/ ;


\ Return the balance QUALITY. It is a number between 0 and 1000 and
\ it indicates how good the answers are divided between yes and no.
: BAL 1000 C_YES @ C_NO @ - ABS 1000 M* C_UNAMB C_UN_CUR + M/ - ;

\ Return the answerability QUALITY. It is a number between 0 and 1000 and
\ it indicates how definite the answers are. Taking into account curiosity.
\ Answers must have been ``ACCUMULATE''d.
: UNAMB C_UNAMB 1000 M*    C_UN_CUR*1000  D+ C_TOTAL M/ ;

\ Reinitalise the answer ballots.
: !ANSWERS 0 C_UNKNOWN !   0 C_YES !   0 C_NO !   0 C_AMB ! ;

\ The first NUMBER is compared to the SECOND. It IS apparently so
\ much greater that the first number represents the true answer.
: APPARENT? CRITERION */ > ;
\D ." Expect -1 : "  100 19 APPARENT? . CR
\D ." Expect 0  : "  0 1  APPARENT? . CR
\D ." Expect -1 0 : "  1 0  APPARENT? . DEPTH . CR

\ Create a simple array of N integers.
\ The created word turns an INDEX into the ADDRESS of the integer.
: ARRAY    CREATE CELLS ALLOT    DOES> SWAP CELLS + ;

\ The following two arrays define the current focus.

DATABASE
\ For DIAGNOSIS return the address of a flag whether is has been excluded.
MAX-DIAGNOSES ARRAY EXCLUSIONS
\ Initialise ``EXCLUSIONS''
: !EXCLUSIONS  0 EXCLUSIONS MAX-DIAGNOSES CELLS ERASE ;
\ For DIAGNOSIS return: it has BEEN excluded.
: ?EXCLUDED EXCLUSIONS @ ;
\D ." !EXCLUSIONS Expect 0 0 : " !EXCLUSIONS 0 ?EXCLUDED . DEPTH . CR

\ For QUESTION return the address of a flag whether is has been posed.
MAX-QUESTIONS ARRAY BEEN-POSED
\ Initialise ``BEEN-POSED''
: !BEEN-POSED  0 BEEN-POSED MAX-QUESTIONS CELLS ERASE ;
\ For QUESTION return: it is has BEEN posed.
: ?POSED BEEN-POSED @ ;
\D ." !BEEN-POSED Expect 0 0 : " !BEEN-POSED 0 ?POSED . DEPTH . CR
\D ." After PREVIOUS Expect DATABASE STRATEGY FORTH : " ORDER CR

\ Accumulate the answer of QUESTION for DIAGNOSIS into the variables above.
: ACCUMULATE
       DUP ?EXCLUDED IF 2DROP EXIT THEN
       2DUP NOES @ >R YESSES @ R>
       2DUP + 0=        IF 2DROP 1 C_UNKNOWN +! EXIT THEN
       2DUP APPARENT?   IF 2DROP 1 C_YES +! EXIT THEN
       SWAP APPARENT?   IF 1 C_NO +! EXIT THEN
       1 C_AMB +!
;
\D !ANSWERS ." !ANSWERS Expect 0 0 0 :" C_YES ? C_NO ? DEPTH . CR
\D 0 0 ACCUMULATE ." ACCUMULATE Expect 0 1 0 :" C_YES ? C_NO ? DEPTH . CR
\D 0 1 ACCUMULATE ." ACCUMULATE Expect 1 1 0 :" C_YES ? C_NO ? DEPTH . CR
\D 1 0 ACCUMULATE ." ACCUMULATE Expect 1 2 0 :" C_YES ? C_NO ? DEPTH . CR
\D 1 1 ACCUMULATE ." ACCUMULATE Expect 1 3 0 :" C_YES ? C_NO ? DEPTH . CR

\ Evaluates and returns the quality for question number INDEX
\ for the current focus. It is a number between 0 and 1000.
\ It is proportional to the number of diagnoses that can be eliminated
\ provided the user can answer the question. (More or less. )
: QUESTION-QUALITY
   !ANSWERS   #DIAGNOSES @ 0 DO DUP I ACCUMULATE LOOP DROP
   C_UNAMB 0= IF 0 ELSE BAL UNAMB 1000 */ THEN
;
\D 0 QUESTION-QUALITY ." QUESTION-QUALITY Expect 1000 0 :" . DEPTH . CR
\D 1 QUESTION-QUALITY ." QUESTION-QUALITY Expect 0 0 :" . DEPTH . CR

PREVIOUS
