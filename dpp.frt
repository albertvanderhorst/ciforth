\ Copyright 2001: Albert van der Horst, HCC FIG Holland by GNU Public License


\ Diagnostic program.
\ Asks questions from the user to arrive at a diagnosis.
\ Example question: "Can it fly"
\ For example diagnose "horse" the answer most users give is probably "no".

\ To be done:
\ 1. Case insensitive answers
\ 2. Clean first part of new question, start with upper case etc.
\ 3. Provide for unreadable database (or handle with THROW).
\ not sealing database in turnkey
\ Not in old version
\ 101. Internet interface.
\ 102. Logging of answer vectors for disapproval.

: Notice1$      "This program is Copyright 2001 by the foundation Dutch Forth Workshop" ;
: Notice2$      "dpp.frt (c) 2001 It may be freely copied under the GNU Public License" ;
: Notice3$      " $Id$ " ;


REQUIRE Z$@
REQUIRE -LEADING
REQUIRE COMPARE
REQUIRE POSTFIX

"dppconfig.frt" INCLUDED
LANGUAGE-FILE INCLUDED

\ #################### GENERAL ########################################

\ This is used as an invalid index into an array to indicate that
\ there was no array item at all.
-1 CONSTANT NONE

\ #################### DATABASE #######################################


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
\    ND answers for question NQ
\ ... all answers



: WELCOME
    Notice1$ TYPE CR Notice2$ TYPE CR Notice3$ TYPE CR CR CR
    Welcome$  TYPE CR CR
;

: VOCABULARY CREATE DOES> DROP ;
: PREVIOUS ;

\ ############################################################################
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
VARIABLE STRIDE
\ Create from a STRING an array of answers ``MAX-DIAGNOSIS'' by ``MAX-QUESTIONS''
\ The answers for the same question are together on one line.
\ Leave REMAINDER.
\ The created word turns an DIAGNOSIS and QUESTION into an ADDRESS.
: ANSWER-ARRAY
CREATE #DIAGNOSES @ 0 DO ^J $S #QUESTIONS @ GET-NUMBERS SPARE CELLS ALLOT LOOP
       STRIDE @ SPARE * ALLOT
DOES> ROT STRIDE @ * + SWAP CELLS + ;

"DIAGNOSES" (CREATE)
"QUESTIONS" (CREATE)
"YESSES"    (CREATE)
"NOES"      (CREATE)
"?ES"       (CREATE)

\ Read the database from file with NAME .
: READ-DATABASE
    GET-FILE

    ^J $S atoi    DUP #DIAGNOSES !
    "DIAGNOSES*" POSTFIX $ARRAY
    LATEST 'DIAGNOSES 3 CELLS MOVE
    ^J $S atoi    DUP #QUESTIONS !
    "QUESTIONS*" POSTFIX $ARRAY
    LATEST 'QUESTIONS 3 CELLS MOVE

    #QUESTIONS @ SPARE + CELLS STRIDE !

    ^J $S CR TYPE   \ Show potential problems.

    "YESSES*" POSTFIX ANSWER-ARRAY
    LATEST 'YESSES 3 CELLS MOVE

    ^J $S CR TYPE   \ Show potential problems.

    "NOES*" POSTFIX ANSWER-ARRAY
    LATEST 'NOES 3 CELLS MOVE

    ^J $S CR TYPE   \ Show potential problems.

    "?ES*" POSTFIX ANSWER-ARRAY
    LATEST '?ES 3 CELLS MOVE

    2DROP
;

\D DB-FILE READ-DATABASE
\D CR ." Expect 0 : " DEPTH . CR
\D 0 0 YESSES ." YESSES Expect 0 0 : " ? DEPTH .  CR
\D 0 0 NOES   ." NOES   Expect 2 0 : " ? DEPTH .  CR
\D 1 0 NOES   ." NOES   Expect 0 0 : " ? DEPTH .  CR
\D 1 1 NOES   ." NOES   Expect 2 0 : " ? DEPTH .  CR
\D ." Expect a diagnosis : " 0 DIAGNOSES 2@ TYPE CR
\D ." Expect a question : " 0 QUESTIONS 2@ TYPE CR
\D ." Expect 0 : " DEPTH .  CR

\D : .QUESTION CR DUP . DUP 0 #QUESTIONS @ WITHIN 0= ABORT" QUESTION OUT  OF BOUNDS"
\D   DUP QUESTIONS 2@ TYPE CR ;
\D : .DIAGNOSIS CR DUP . DUP 0 #DIAGNOSES @ WITHIN 0= ABORT" DIAGNOSIS OUT  OF BOUNDS"
\D   DUP DIAGNOSES 2@ TYPE CR ;

: (U.) 0 <# #S #> ;

\ Receive the output database.
VARIABLE OUTPUT$
\D : ?OUTPUT OUTPUT$ @ $@ TYPE ;

\ Add a STRING to the output.
: type  OUTPUT$ @ $+!    ;
\D HERE 0 , 100 ALLOT OUTPUT$ !
\D "OEPS" type ." Expect |OEPS| 0 : " &| EMIT ?OUTPUT &| EMIT DEPTH . CR

\ Add a carriage return to the output.
: cr ^J OUTPUT$ @ $C+ ;

\ Add a blank to the output.
: bl BL OUTPUT$ @ $C+ ;
\D bl ." Expect |OEPS | 0 : " &| EMIT ?OUTPUT &| EMIT DEPTH . CR

\ Add a NUMBER to the output.
: u. (U.) OUTPUT$ @ $+!    ;
\D 12 u. ." Expect |OEPS 12| 0 : " &| EMIT ?OUTPUT &| EMIT DEPTH . CR

\ Add the strings of an ARRAY of N pointers to strings to the output.
\ The array is passed as an xt.
: .$ARRAY   0 DO I OVER EXECUTE 2@ OUTPUT$ @ $+!  cr LOOP DROP ;

\ Add from an ARRAY of N numbers to a line in the output.
: PUT-NUMBERS 0 DO I CELLS OVER + @ u. bl LOOP cr DROP ;

: uk "------" type cr ;

\ Add an ARRAY of answers ``#ANSWERS'' by ``#QUESTIONS'' to the output.
: PUT-ANSWERS
    uk
    #DIAGNOSES @ 0 DO
        DUP #QUESTIONS @ PUT-NUMBERS STRIDE @ +
    LOOP DROP ;

ONLY FORTH DEFINITIONS
DATABASE
\D         : ^^  "OUTPUT :" TYPE ?OUTPUT CR ;
\ Write the database to the file "database2"
: WRITE-DATABASE
    HERE OUTPUT$ ! 0 , 10,000,000 ALLOT

    #DIAGNOSES @ DUP u. cr    'DIAGNOSES SWAP .$ARRAY
    #QUESTIONS @ DUP u. cr    'QUESTIONS SWAP .$ARRAY

    0 0 YESSES  PUT-ANSWERS
    0 0 NOES    PUT-ANSWERS
    0 0 ?ES     PUT-ANSWERS
    OUTPUT$ @ $@ DB-FILE PUT-FILE
;

\ ############################################################################
\ For a STRING return a STRING without leading spaces.
\D "   X ? " -LEADING ." Expect |X ? |0 : " &| EMIT TYPE &| EMIT DEPTH . CR

\ For a STRING return a clean STRING, without spaces or the question mark.
: CLEAN-STRING -LEADING -TRAILING 2DUP + 1- C@ &? = IF 1- THEN -TRAILING ;

\D " How ? " CLEAN-STRING
\D ." Expect |How|0 : " &| EMIT TYPE &| EMIT DEPTH . CR

\ Convert a STRING VARIABLE to lower case.
: $TO-LOWER $@ OVER + SWAP DO I C@ &A &Z 1+ WITHIN IF 32 I +! THEN LOOP ;
\D "AAP" $, DUP $TO-LOWER ." Expect aap : " $@ TYPE   CR

\ Add STRING as a diagnosis.
: ADD-DIAGNOSIS CLEAN-STRING $, DUP $TO-LOWER $@
    #DIAGNOSES @ DIAGNOSES 2! #DIAGNOSES @ 1 #DIAGNOSES +! ;
\D " Ape" ADD-DIAGNOSIS
\D 2 DIAGNOSES 2@ ." ADD-D Expect |ape|2 0 :" &| EMIT TYPE &| EMIT . DEPTH . CR
\D -1 #DIAGNOSES +!

\ Add STRING as a QUESTION.
: ADD-QUESTION CLEAN-STRING $, $@ #QUESTIONS @ QUESTIONS 2! 1 #QUESTIONS +! ;

\ Find a diagnosis STRING and return its INDEX (or ``NONE'').
: FIND-DIAGNOSIS CLEAN-STRING PAD $! PAD $TO-LOWER
    NONE PAD $@
    #DIAGNOSES @ 0 DO 2DUP I DIAGNOSES 2@ COMPARE 0= IF
        2DROP DROP I UNLOOP EXIT
    THEN LOOP
    2DROP ;
\D "RENDIER " FIND-DIAGNOSIS ." FIND-D Expect 0 0 : " . DEPTH . CR
\D "PANDIER " FIND-DIAGNOSIS ." FIND-D Expect -1 0 : " . DEPTH . CR


\ For INDEX get the diagnostic STRING.
: DIAG DIAGNOSES 2@ ;

\ For INDEX get the diagnostic STRING.
: QUES QUESTIONS 2@ ;
PREVIOUS

\ #################### INTERACTION ####################################

VOCABULARY INTERACTION
INTERACTION DEFINITIONS

\ Possible values for what we call an answer.
 0 CONSTANT A_NONE       \ No answer yet.
 1 CONSTANT A_YES
 2 CONSTANT A_NO
 3 CONSTANT A_AMB
 4 CONSTANT MAX-ANSWER
 CREATE MAY-BE-DISTINGUISHING
 \ A_NONE A_YES A_NO A_AMB  outcome
  1 ,     1 ,     1 ,     0 ,    \  A_NONE   db diagnosis
  1 ,     0 ,     1 ,     0 ,    \  A_YES    "
  1 ,     1 ,     0 ,     0 ,    \  A_NO     "
  0 ,     0 ,     0 ,     0 ,    \  A_AMB    "

\ Convert a STRING to an ANSWER.
: TO-ANSWER 0= IF DROP A_NONE EXIT THEN
   C@
   Yes$ DROP C@ OVER = IF DROP A_YES EXIT THEN
   No$  DROP C@ OVER = IF DROP A_NO  EXIT THEN
   Amb$ DROP C@ OVER = IF DROP A_AMB EXIT THEN
   &?           OVER = IF DROP A_AMB EXIT THEN
   DROP A_NONE
;

\ Print a STRING as a question.
: POSE-QUESTION CR 4 SPACES TYPE SPACE &? EMIT CR ;

\ For a question STRING get an ANSWER.
: (GET-ANSWER) POSE-QUESTION (ACCEPT) TO-ANSWER ;

\ Disapprove ANSWER , tell user so.
: DISAPPROVE DROP NotAnAnswer$ TYPE ;

\ For a question STRING get an ANSWER. ``A_NONE'' is not accepted.
: GET-ANSWER
    BEGIN 2DUP (GET-ANSWER) DUP A_NONE = WHILE DISAPPROVE REPEAT >R 2DROP R>
;

PREVIOUS DEFINITIONS

VOCABULARY D-MAIN
D-MAIN DEFINITIONS   DATABASE INTERACTION

\ Exit to linux with whatever ERROR was caught, hopefuly zero.
: ERROR-BYE NEGATE 0 0 1 LINOS ;

\ Do whatever need to be done if the user wants to stop.
: FINISH
     QuerySave$ GET-ANSWER A_NO = IF EXIT THEN
     Save$ TYPE CR WRITE-DATABASE
;
ONLY FORTH

INTERACTION DEFINITIONS
DATABASE

\ Create a simple array of N integers.
\ The created word turns an INDEX into the ADDRESS of the integer.
: ARRAY    CREATE CELLS ALLOT    DOES> SWAP CELLS + ;

\ The following two arrays define the current focus.
\ For DIAGNOSIS return the address of a flag whether is has been excluded.
MAX-DIAGNOSES ARRAY EXCLUSIONS
\ Initialise ``EXCLUSIONS''
: !EXCLUSIONS  0 EXCLUSIONS #DIAGNOSES @ CELLS ERASE ;
\ For DIAGNOSIS return: it has BEEN excluded.
: ?EXCLUDED EXCLUSIONS @ ;
\D ." !EXCLUSIONS Expect 0 0 : " !EXCLUSIONS 0 ?EXCLUDED . DEPTH . CR

\ For QUESTION return the address of the user answer or A_NONE if it is has
\ not been posed.
MAX-QUESTIONS ARRAY ANSWER-VECTOR
\ Initialise ``ANSWER-VECTOR''
: !ANSWER-VECTOR #QUESTIONS @ 0 DO A_NONE I ANSWER-VECTOR ! LOOP ;

\ For QUESTION return: it is has BEEN posed.
: ?POSED ANSWER-VECTOR @ A_NONE <> ;
\D ." !ANSWER-VECTOR Expect 0 0 : " !ANSWER-VECTOR 0 ?POSED . DEPTH . CR
\ \D ." After PREVIOUS Expect DATABASE STRATEGY FORTH : " ORDER CR

\ Auxiliary type vertical lines connecting questions with answers.
: .BLOCKS 0 ?DO "|     " TYPE LOOP ;
\ Print all information that can still influence the outcome.
: PRINTTABLE
  "______________________________________________" TYPE CR

  0 #QUESTIONS @ 0 DO I ?POSED 0= IF
    21 SPACES DUP .BLOCKS I QUESTIONS 2@ TYPE 1+ CR
  THEN LOOP DROP
  #DIAGNOSES @ 0 DO I ?EXCLUDED 0= IF
      I 3 .R SPACE I DIAGNOSES 2@ DUP >R TYPE 15 R> - SPACES
      #QUESTIONS @ 0 DO I ?POSED 0= IF
          J I YESSES @ 3 .R J I NOES @ 3 .R
      THEN LOOP CR
  THEN LOOP
  "______________________________________________" TYPE CR
;

PREVIOUS

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

DATABASE
\ Accumulate the answer of QUESTION for DIAGNOSIS into the variables above.
: ACCUMULATE
       DUP ?EXCLUDED IF 2DROP EXIT THEN
       SWAP     \ All databases use [ D, Q ]
       2DUP NOES @ >R YESSES @ R>
       2DUP + 0=        IF 2DROP 1 C_UNKNOWN +! EXIT THEN
       2DUP APPARENT?   IF 2DROP 1 C_YES +! EXIT THEN
       SWAP APPARENT?   IF 1 C_NO +! EXIT THEN
       1 C_AMB +!
;
\D !ANSWERS ." !ANSWERS Expect 0 0 0 : " C_YES ? C_NO ? DEPTH . CR
\D 0 0 ACCUMULATE ." ACCUMULATE Expect 0 1 0 : " C_YES ? C_NO ? DEPTH . CR
\D 0 1 ACCUMULATE ." ACCUMULATE Expect 1 1 0 : " C_YES ? C_NO ? DEPTH . CR
\D 1 0 ACCUMULATE ." ACCUMULATE Expect 1 2 0 : " C_YES ? C_NO ? DEPTH . CR
\D 1 1 ACCUMULATE ." ACCUMULATE Expect 1 3 0 : " C_YES ? C_NO ? DEPTH . CR

\ Evaluates and returns the quality for question number INDEX
\ for the current focus. It is a number between -1 and 1000.
\ It is proportional to the number of diagnoses that can be eliminated
\ provided the user can answer the question. (More or less. )
\ -1 is reserved for the situation that none of the questions can
\ lead to any exclusions, where 0 means that with luck there may be
\ conclusions.
-1 CONSTANT REAL-BAD
: QUESTION-QUALITY
   !ANSWERS   #DIAGNOSES @ 0 DO DUP I ACCUMULATE LOOP DROP
   C_UNAMB 0= IF REAL-BAD ELSE BAL UNAMB 1000 */ THEN
;
\D 0 QUESTION-QUALITY ." QUESTION-QUALITY Expect 1000 0 : " . DEPTH . CR
\D 1 QUESTION-QUALITY ." QUESTION-QUALITY Expect 0 0 : " . DEPTH . CR

\ From I1 Q1 I2 Q2 leave the pair with highest Q.
: BEST-PAIR DUP >R 2SWAP DUP R> > IF 2SWAP THEN 2DROP ;
\D 0 100 3 200 BEST-PAIR ." BP Expect 3 300 :" SWAP . . CR
\D 3 200 0 100 BEST-PAIR ." BP Expect 3 300 :" SWAP . . CR

\ Select the best question still available. Return its INDEX.
: SELECT-QUESTION NONE 0  \ Initial INDEX and QUALITY.
    #QUESTIONS @ 0 DO I ?POSED 0= IF
       I  I QUESTION-QUALITY BEST-PAIR
    THEN LOOP
    REAL-BAD = IF DROP NONE THEN   \ The best question is worthless.
;
\D SELECT-QUESTION ." SELECT-QUESTION Expect 0 0 : " . DEPTH . CR

PREVIOUS

ONLY FORTH DEFINITIONS
VOCABULARY CONSULTING
CONSULTING DEFINITIONS INTERACTION DATABASE STRATEGY

VARIABLE POSSIBILITIES  \ Number of diagnoses left
\ The ANSWER is ambiguous, i.e. cannot be used to exclude diagnoses.
: ?AMBIGUOUS DUP A_YES = SWAP A_NO = OR 0= ;
\D A_NONE ?AMBIGUOUS ." Expect -1 0 : " . DEPTH . CR
\D A_NO ?AMBIGUOUS ." Expect 0 0 : " . DEPTH . CR

\ Return for DIAGNOSIS and QUESTION what the answer is according
\ to the database. All ambiguous answers are mapped to ``A_NONE''.
: ANSWER-FOR
       2DUP NOES @ >R YESSES @ R>
       2DUP + 0=        IF 2DROP A_NONE EXIT THEN
       2DUP APPARENT?   IF 2DROP A_YES EXIT THEN
       SWAP APPARENT?   IF A_NO EXIT THEN
       A_NONE ;
\D 0 0 ANSWER-FOR ." ANSWER-FOR Expect 2 2 0 : " . A_NO  . DEPTH . CR
\D 1 0 ANSWER-FOR ." ANSWER-FOR Expect 1 1 0 : " . A_YES . DEPTH . CR
\D 0 1 ANSWER-FOR ." ANSWER-FOR Expect 2 2 0 : " . A_NO . DEPTH . CR
\D 1 1 ANSWER-FOR ." ANSWER-FOR Expect 2 2 0 : " . A_NO . DEPTH . CR

\ Eliminate, if uncompatible with ANSWER to QUESTION, a DIAGNOSIS.
\ The diagnosis was not yet excluded and the answer is unambiguous.
: ELIMINATE-ONE
\D      ." ELIMINATING" .DIAGNOSIS SWAP .QUESTION SWAP
        DUP >R
        SWAP ANSWER-FOR DUP ?AMBIGUOUS IF
            2DROP
        ELSE
            <> IF 1 R@ EXCLUSIONS ! -1 POSSIBILITIES +! THEN
        THEN
        RDROP
;
\D A_NO  0 0 ELIMINATE-ONE ." E-ONE Expect 0 0 : " 0 EXCLUSIONS ? DEPTH . CR
\D A_YES 0 0 ELIMINATE-ONE ." E-ONE Expect 1 0 : " 0 EXCLUSIONS ? DEPTH . CR

\ Eliminate all diagnoses, uncompatible with ANSWER to QUESTION.
: ELIMINATE OVER ?AMBIGUOUS 0= IF
    #DIAGNOSES @ 0 DO I ?EXCLUDED 0= IF 2DUP I ELIMINATE-ONE THEN LOOP
  THEN 2DROP ;
\D !EXCLUSIONS
\D A_NO  0 ELIMINATE ." ELIMINATE  Expect 0 1 : " 0 EXCLUSIONS ? 1 EXCLUSIONS ? CR
\D !EXCLUSIONS
\D A_YES  0 ELIMINATE ." ELIMINATE  Expect 1 0 : " 0 EXCLUSIONS ? 1 EXCLUSIONS ? CR
\D !EXCLUSIONS
\D ." Expect 0 : " DEPTH . CR

ONLY FORTH DEFINITIONS

INTERACTION DEFINITIONS
DATABASE CONSULTING STRATEGY
\ For DIAGNOSIS: return IT if it is the outcome else ``NONE''.
: CONFIRM-DIAGNOSIS
    DUP ?EXCLUDED IF DROP NONE EXIT THEN
    IsIt$ PAD $! DUP DIAGNOSES 2@ PAD $+!
    PAD $@ GET-ANSWER A_YES = IF SmartEeh$ TYPE ELSE DROP NONE THEN
;

\ Return the DIAGNOSIS that comes out. Always ask the
\ user for confirmation, especially if there are more
\ possibilities. Return ``NONE'' for no confirmed diagnosis.
: POSE-DIAGNOSIS
    POSSIBILITIES @ DUP 0= IF DROP NONE EXIT THEN
    1 = IF IThinkIKnow$ ELSE ThereAreSeveral$ THEN TYPE
    NONE #DIAGNOSES @ 0 DO I CONFIRM-DIAGNOSIS DUP NONE <> IF SWAP DROP LEAVE THEN DROP LOOP
;

\ The answer for QUESTION is turned into a corrected ANSWER.
\ This may led to more exclusions and this must be done before
\ we ask the user to help us with new questions. (On the other hand
\ falsely excluded answers are not changed. This is too complicated.)
\ mAYBE THIS DOESN'T MAKE SENSE BECAUSE WE KNOW THE OUTCOME BUT WE GUESSED
\ WRONG SO THERE IS NOTHING TO DISCRIMINATE AGAINST.
\ : EXCLUDE-MORE
\     DUP ?AMBIGUOUS 0= IF
\         #DIAGNOSES @ 0 DO OVER I SWAP ANSWER-FOR
\             DUP ?AMBIGUOUS 0= >R >R OVER R> = R> AND IF
\                 1 I EXCLUSIONS ! LEAVE
\             THEN
\         LOOP
\    2DROP
\ ;


: CONFLICTING?
    DUP ?AMBIGUOUS IF 2DROP 0 EXIT THEN
    OVER ?AMBIGUOUS IF 2DROP 0 EXIT THEN
    <> ;

\ Have the user confirm for this OUTCOME that the answer for the
\ QUESTION that the he gave was intended. If it isn't, correct the
\ ``ANSWER-VECTOR'' for that question to ``A_NONE''.
\ (Do not ask to correct the answer, this would be unlawful influencing.)
: CONFIRM-ONE-ANSWER
    AreYouSure1$ TYPE CR
    8 SPACES DUP QUESTIONS 2@ TYPE CR
    AreYouSure2$ TYPE
    OVER DIAGNOSES 2@ TYPE
    AreYouSure2a$ TYPE CR
    8 SPACES
    DUP ANSWER-VECTOR @ A_YES = IF Yes$ ELSE No$ THEN TYPE CR CR
    AreYouSure3$  TYPE CR CR
    DUP QUESTIONS 2@ GET-ANSWER   OVER  ANSWER-VECTOR !
    2DROP ;

\ The current answer vector and the vector for the outcome DIAGNOSIS
\ confirmed by the user are in conflict, because this outcome was apparently
\ excluded along the way.
\ Give the user an opportunity to change the answer vector before it is added
\ to the database.
\ This helps keeping the database clean from typo's and lousy answering.
: CONFIRM-ANSWERS
    #QUESTIONS @ 0 DO
        I ANSWER-VECTOR @   OVER I ANSWER-FOR CONFLICTING? IF
            DUP I CONFIRM-ONE-ANSWER
        THEN
    LOOP DROP
;

\ Ask the user to introduce a new diagnosis, because no diagnosis
\ known is compatible with his answers. However sometimes the new
\ diagnosis is known already, but the answers are unexpected.
\ Always the DIAGNOSIS is returned.
: NEW-DIAGNOSIS
    DumbEeh$ TYPE CR
    PleaseLearn$ POSE-QUESTION (ACCEPT)
    2DUP FIND-DIAGNOSIS DUP NONE = IF
        DROP ADD-DIAGNOSIS
    ELSE  \ Should be an exceptional case
        GotItWrong$ TYPE CR CR CR
        >R 2DROP  R@ CONFIRM-ANSWERS R>
    THEN
;

\ For two diagnoses D1 and D2, tell whether there IS some
\ question in the database to make a distinction between them.
: ?DISTINGHUISABLE 0 >R
   BEGIN 2DUP R@ ANSWER-FOR SWAP R@ ANSWER-FOR
       OVER A_NONE <>  OVER A_NONE <> AND >R
       <> R> AND IF 2DROP RDROP 1 EXIT THEN
   #DIAGNOSES @ R@ <> WHILE
       R> 1+ >R
   REPEAT 2DROP RDROP 0
;
\D ." ?DIST Expect 0 0 : " 0 0 ?DISTINGHUISABLE . DEPTH . CR
\D ." ?DIST Expect 1 0 : " 0 1 ?DISTINGHUISABLE . DEPTH . CR
\D ." ?DIST Expect 1 0 : " 1 0 ?DISTINGHUISABLE . DEPTH . CR

\ For the GUESSED diagnosis and QUESTION : it MAKES sense to ask
\ it to distinguish between diagnosis and the current outcome.
: ?SELECTABLE
    DUP >R ANSWER-FOR   R> ANSWER-VECTOR @ ^
    MAX-ANSWER * + CELLS MAY-BE-DISTINGUISHING + @
;
\D ." ?SELECTABLE Expect 0 0 : " 1 0 ?SELECTABLE . DEPTH . CR

\ Print a QUESTION with an identification for selection.
: PRINT-FOR-SELECT DUP 3 .R 5 SPACES   QUESTIONS 2@ TYPE   CR ;

\ Accept a number from the terminal and return IT.
\ If not a valid number return NONE.
: GET-NUMBER 0. (ACCEPT) >NUMBER  IF 2DROP DROP NONE ELSE 2DROP THEN ;

\ For the GUESSED diagnosis and QUESTION : it IS validated as a selectable question.
: VALID? DUP 0 #QUESTIONS @ WITHIN >R ?SELECTABLE R> AND ;

\ For GUESSED diagnosis return return the QUESTION the user selects.
: GET-EXISTING ^
    BEGIN CR CR GiveIdentification$ TYPE CR GET-NUMBER 2DUP VALID? 0= WHILE
       DROP ThatIsNoGood$ TYPE CR
    REPEAT
    SWAP DROP  ^
;


\ For GUESSED diagnosis return return the number of possible questions.
: #SELECTABLE 0 #QUESTIONS @ 0 DO OVER I ?SELECTABLE IF 1+ THEN LOOP SWAP DROP ;

\ For GUESSED diagnosis return an existing QUESTION that
\ will make a distinction between it and the answer vector.
\ Or ``NONE''.
: SELECT-EXISTING ^
     DUP #SELECTABLE 0 > IF
    CR PossibleExisting$ TYPE CR CR
    #QUESTIONS @ 0 DO DUP I ?SELECTABLE IF I PRINT-FOR-SELECT THEN LOOP
    CR CR AnyGoodQuestion$ GET-ANSWER A_YES = IF GET-EXISTING ELSE DROP NONE THEN
    ELSE DROP NONE THEN ^
;

\ For GUESSED diagnosis and CORRECT diagnosis ask for a new
\ question that will make a distinction between the two. Return
\ IT.
: NEW-QUESTION
    QuerySepar1$ TYPE CR      QuerySepar2$ TYPE CR
     DIAGNOSES 2@ TYPE CR   DIAGNOSES 2@ TYPE CR ( d1 d2 --)
    QuerySepar3$ TYPE CR (ACCEPT) ADD-QUESTION
    #QUESTIONS @ 1- ;

\ For GUESSED diagnosis and CORRECT diagnosis return a QUESTION
\ that will make a distinction between the two. Or ``NONE''.
: GENERATE-QUESTION
     NeedQuestion1$ TYPE CR      NeedQuestion2$ TYPE CR
     OVER DIAGNOSES 2@ TYPE CR   DUP DIAGNOSES 2@ TYPE CR
     OVER SELECT-EXISTING
     DUP NONE = IF DROP 2DUP NEW-QUESTION THEN
     >R 2DROP R>
;

\ For QUESTION and ANSWER store in the database for the outcome diagnosis.
\ (The answer vector is later added to the database.)
: FILL-IN-NEW SWAP ANSWER-VECTOR ! ;

\ For DIAGNOSIS QUESTION and ANSWER store in the database.
: FILL-IN-DB >R
    R@ A_YES = IF NOES 2 SWAP +! ELSE
    R@ A_NO = IF YESSES 2 SWAP +! ELSE
    2DROP NoGoodEeh1$ TYPE CR  NoGoodEeh2$ TYPE CR
    THEN THEN
RDROP ;

\ Eliminate the ambiguity for GUESSED diagnosis and CORRECT diagnosis by
\ having a question answered by the user.
: (EL-AM)
    2DUP GENERATE-QUESTION ( d o -- d o q )
    AnswerQuestion$ TYPE CR    SWAP DIAGNOSES 2@ TYPE CR ( o q -- q)
    DUP QUESTIONS 2@ GET-ANSWER ( d q -- d q a)
    2DUP FILL-IN-NEW
    FILL-IN-DB                  ( d q a -- )
;

\ The outcome was the diagnosis INDEX. (Maybe just added.)
\ Ask the operator for help to make sure this diagnosis can be
\ distinguished from other diagnoses by some question.
\ Of course we need only inspect those not excluded, (except in the
\ case of wrong answers that were corrected afterwards.)
: ELIMINATE-AMBIGUITY
\D  .DIAGNOSIS
    #DIAGNOSES @ 0 DO
       DUP I <>   I ?EXCLUDED 0= AND   OVER I ?DISTINGHUISABLE 0= AND
       IF I OVER (EL-AM) THEN
    LOOP DROP ;

\ Convert an ANSWER to an PAIR of integers to be added to the
\ ``YES'' and ``NOES' arrays.
: ADDABLE
        >R
        R@ A_YES = IF 2 0 ELSE
        R@ A_NO =  IF 0 2 ELSE
        R@ A_AMB = IF 1 1 ELSE
                      0 0
        THEN THEN THEN RDROP
;

\D ." Expect 2 0 0 : " A_YES ADDABLE SWAP . . DEPTH . CR

\ For a DIAGNOSIS and INDEX add the PAIR to the
\ ``YES'' and ``NOES' arrays.
: ADDIT >R >R 2DUP YESSES R> SWAP +! NOES R> SWAP +! ;
\D ." ADDIT Expect 0 5 0 : " 0 0 0 3 ADDIT  0 0 YESSES ? 0 0 NOES ? DEPTH . CR
\D ." ADDIT Expect 7 2 0 : " 1 1 7 0 ADDIT  1 1 YESSES ? 1 1 NOES ? DEPTH . CR

\ Add the answer vector for the OUTCOME diagnosis to the database
: ADD-ANSWERS
    #QUESTIONS @ 0 DO
        DUP I I ANSWER-VECTOR @ ADDABLE   ADDIT
    LOOP DROP
;
\D !ANSWER-VECTOR PRINTTABLE
\D A_YES 0 ANSWER-VECTOR ! A_NO 1 ANSWER-VECTOR ! 0 ADD-ANSWERS
\D ." For diagnosis 0 we add yes no to the database."
\D PRINTTABLE
\D !ANSWER-VECTOR PRINTTABLE
\D A_NO 0 ANSWER-VECTOR ! A_YES 1 ANSWER-VECTOR ! 1 ADD-ANSWERS
\D ." For diagnosis 1 we add 'no yes' to the database."
\D !ANSWER-VECTOR PRINTTABLE A_NO 0 ANSWER-VECTOR ! A_YES 1 ANSWER-VECTOR ! 1 ADD-ANSWERS
\D ." ADD-A Expect 2 2 0 : " 0 1 YESSES ? 0 1 NOES ? DEPTH . CR
\D ." ADD-A Expect 7 2 0 : " 1 1 YESSES ? 1 1 NOES ? DEPTH . CR

: INIT DB-FILE READ-DATABASE ;


\ Ask as many question as makes sense to zoom in onto the diagnosis.
: INTERROGATION
    BEGIN SELECT-QUESTION DUP REAL-BAD <> POSSIBILITIES @ 1 > AND WHILE
        GotLeft1$ TYPE POSSIBILITIES @ . GotLeft2$ TYPE CR
        DUP QUESTIONS 2@ GET-ANSWER   SWAP
        2DUP ANSWER-VECTOR !    ELIMINATE
\D      PRINTTABLE
    REPEAT DROP
;

: ONE-DIAGNOSIS
    !EXCLUSIONS !ANSWER-VECTOR  #DIAGNOSES @ POSSIBILITIES !
    INTERROGATION
    POSE-DIAGNOSIS DUP NONE = IF
        DROP NEW-DIAGNOSIS DUP ELIMINATE-AMBIGUITY
    THEN
    ADD-ANSWERS
    !ANSWER-VECTOR
;

D-MAIN DEFINITIONS
\ Now doit.
: DOIT
    INIT
    WELCOME ?STACK
    BEGIN ONE-DIAGNOSIS GoOn$ GET-ANSWER A_YES = WHILE REPEAT
    FINISH
;

ONLY FORTH DEFINITIONS
D-MAIN
: MAIN 'DOIT CATCH ERROR-BYE ;
