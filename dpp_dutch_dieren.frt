( Copyright  2001 : Albert van der Horst, HCC FIG Holland by GPL        )
( $Id$      )
(                                                                       )
(                                                                       )
( Definieer de strings die nodig zijn om het diagnose programma         )
( d++ te gebruiken om dieren te vinden, in de Nederlandse taal.         )
(                                                                       )
( ####################################################################  )
( If replaced by some other plug in, another language can be supported. )
( ####################################################################  )

( Note that ciforth supports multiple line strings, which may not be    )
( common.                                                               )

: Welcome$
"Welkom bij het experimentele AI program om dieren te
determineren. Dit is een lerend programma. Het programma leert
niet van biologische experts, maar van jou als surfer op het
Internet. Daarom kun je rare dingen tegenkomen. Een deel van
het AI experiment is om te kijken of het program deze eventuele
onjuiste kennis ook weer kan afleren door antwoorden van
verschillende mensen te vergelijken.

IF YOU ARE NOT A NATIVE DUTCH SPEAKER, PLEASE WAIT FOR THE ENGLISH VERSION!

OTHERWISE YOU MAY SPOIL THE OUTCOME OF THIS ARTIFICIAL INTELLIGENCE
EXPERIMENT.
" ;

: Yes$          "ja" ;
: No$           "nee" ;
: Amb$          "onbekend" ;
: NotAnAnswer$  "Sorry, ik verwacht ja/nee/onbekend (of ?)/. Een letter volstaat." ;
: QuerySave$    "Wil je de database met de nieuwe kennis weer opslaan" ;
: Save$         "Bezig met weg schrijven van de database naar een nieuwe file." ;

( Tijdelijk en experimenteel.                                            )
: Save$2        "Ik sla op in de file 'database2', vernoem naar 'database' om verder te gebruiken." ;

: IThinkIKnow$  "Ik denk dat ik weet wat je in gedachten had" ;
: ThereAreSeveral$ "Er is meer dan een mogelijkheid." ;
: IsIt$         "Is het een " ;
: SmartEeh$     "Slim, he?" ;
: DumbEeh$      "Ik moet toegeven dat ik het niet weet." ;
: PleaseLearn$  "Vertel asjeblief wat je in gedachten had" ;
: GotItWrong$   "Ik had het dus fout" ;
: NeedQuestion1$ "Nu heb ik een vraag nodig om verschil te maken" ;
: NeedQuestion2$ "tussen de twee volgende mogelijkheden" ;
: QuerySepar1$  "Je moet een nieuwe vraag opgeven" ;
: QuerySepar2$  "om de volgende twee mogelijkheden uit elkaar te houden." ;
: QuerySepar3$  "Type het hier:" ;
: AnswerQuestion$ "Beantwoord de vraag voor :" ;
: NoGoodEeh1$   "Dus de vraag is eigenlijk toch niet goed, he?" ;
: NoGoodEeh2$   "Ik laat het er maar bij." ;

: PossibleExisting$
"Misschien is een van de vragen die ik al ken daar goed voor.
Ik laat ze zien.
" ;

: AnyGoodQuestion$ "Is een van die vragen geschikt" ;
: GiveIdentification$ "Geef het nummer dat voor de beste vraag staat" ;
: ThatIsNoGood$  "Dat nummer staat er niet bij" ;
: AreYouSure1$  "De meeste mensen beantwoorden de vraag :" ;
: AreYouSure2$  "voor een " ;
: AreYouSure2a$  " *niet* met " ;

: AreYouSure3$
"Voor de zekerheid vraag ik het nog een keer.
Je mag bij twijfel ook onbekend (of ?) zeggen."
;

: GotLeft1$     "Er zijn er nog " ;
: GotLeft2$     " over" ;
: GoOn$         "Wil je nog een keer" ;
