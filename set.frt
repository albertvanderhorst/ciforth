( @+ SET !SET SET? SET+! .SET set_utility) \ AvdH 2K2may15
'$@ ALIAS @+
( Build a set "x" with X items. )
: SET   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
: !SET   DUP CELL+ SWAP ! ;   ( Make the SET empty )
: SET+!   DUP >R @ ! 0 CELL+ R> +! ;   ( Add ITEM to the SET )
: .SET   @+ SWAP ?DO I ? 0 CELL+ +LOOP ;   ( Print SET )
( For VALUE and SET return the POSITION of value in set, or a null. )
: WHERE-IN-SET $@ SWAP ?DO
   DUP I @ = IF DROP I UNLOOP EXIT THEN 0 CELL+ +LOOP DROP 0 ;
( For VALUE and SET return "the value IS present" )
: IN-SET? WHERE-IN-SET 0= 0= ;

( SET? 2SET+! SET+@ SET-REMOVE set_utility_aux ) \ AvdH 2K2sep16
: SET?   @+ = 0= ;   ( For the SET : it IS non-empty )
: 2SET+! >R SWAP R@ SET+! R> SET+! ;
( Retract from SET in same order. Leave ITEM. Use after !SET )
: SET+@   DUP >R @ @ 0 CELL+ R> +! ;
( Remove entry at ADDRESS from SET. )
: SET-REMOVE   >R   DUP CELL+ SWAP  R@ @ OVER -   MOVE
    -1 CELLS R> +! ;
