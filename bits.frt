\ -----------------------------------------------------------------------
\                      Bit sets
\ -----------------------------------------------------------------------
REQUIRE NEW-IF

CREATE BIT-MASK-TABLE 1   8 0 DO DUP C, 1 LSHIFT LOOP DROP
: BIT-MASK   CHARS BIT-MASK-TABLE + C@ ;

\ For BIT and BITSET return ADDRESS  and MASK.
: BIT-WHERE SWAP 8 /MOD SWAP BIT-MASK >R + R> ;

\ Set BIT in BITSET
: SET-BIT BIT-WHERE OVER C@ OR SWAP C! ;

\ Clear  BIT in BITSET
: CLEAR-BIT BIT-WHERE INVERT OVER C@ AND SWAP C! ;

\ For BIT in BITSET , return "it IS set".
: BIT? BIT-WHERE SWAP C@ AND 0= 0= ;
