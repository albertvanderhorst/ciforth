( DEFER IS ) \ AvdH A2apr22

\ Default action for not filled in deferred word.
: DEFER-ERROR    -1 13 ?ERROR ;

\ Define a word, with a changable behaviour. "deferred" word".
: DEFER CREATE ' DEFER-ERROR , DOES> @ EXECUTE ;

\ Fill XT in as the behaviour of the named deferred word.
: IS   (WORD) FOUND >BODY ! ;





