( Copyright 2001: Albert van der Horst, HCC FIG Holland by GPL          )
( $Id$           )
(                                                                       )
( Diagnostic program.                                                   )
( Configuration file.                                                   )
( See dpp_language_application.frt for textual customization.           )
(                                                                       )
( All customization is done by colon definitions.                       )

( FOR THOSE UNFAMILIAR WITH FORTH                                       )
( A colon defines some new executable mini- program ending with         )
( a semi colon as per                                                   )
(   : S@>MY-NAME$ 123 ;                                                 )
( This defines a program that has the number 123 as a result. Names can )
( have any characters except blanks. The above example doesn't          )
( even look weird for an experienced Forther.                           )

(                                                                       )
( ################ FIXME ############################################## )
( Upper limits for arrays                                               )
( This is ugly and hopefully temporary.                                 )
: MAX-DIAGNOSES 1000 ;
: MAX-QUESTIONS 200 ;

( #################### CONFIGURATION ################################## )

( Leave a FRACTION  numerator/denominator  that decides whether         )
( the difference between occurences of answers is significant.          )
( This means yes-answers must outnumber the no-answers 5 to 1, or vv.   )
: CRITERION 5 1 ;

( On a scale from 0 to 1000, the tendency to ask questions that         )
( learn at the expense of faster retrieval.                             )
( Over 1000 means that the program becomes more interested in           )
( having unknown questions answered than arriving at a diagnosis.       )
: CURIOSITY 720 ;

( The name of the file containing the database.                         )
: DB-FILE "database" ;

( The name of the file containing the language/application              )
( customization. The line choosen is the one that is not                )
( between Klammern.                                                     )
    : LANGUAGE-FILE "dpp_dutch_dieren.frt" ;
(   : LANG-FILE "dpp_english.frt"      ;                                )

( Choose the first line to debug, the second line to not debug.         )
( The line choosen is the one that is not between Klammern.             )
      : \D ;
(     : \D ^J WORD DROP ; IMMEDIATE                                     )
