( $Id: )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Generate postscript data sheet. )
DECIMAL
: PRELUDE
  ." SNIP TILL HERE" CR
  ." %!" CR
  ." /Helvetica findfont 10 scalefont setfont " CR
  ." 60 60 translate /wt 60 def /ht 23 def " CR
  9 0 DO 
    I . ." wt mul 0 ht mul moveto 0 ht 32 mul rlineto stroke" CR 
  LOOP
  8 0 DO 
    I . ." .3 add wt mul ht 32.3 mul moveto" CR 
    HEX &( EMIT I U. &/ EMIT SPACE I 8 + U. &) EMIT ." show" CR DECIMAL
  LOOP
  33 0 DO 
    ." 0 wt mul " I . ." ht mul moveto wt 8 mul 0 rlineto stroke" CR 
  LOOP
  32 0 DO 
    I . ." .3 add wt mul ht 32.3 mul moveto" CR 
    ." -15 " I . ." .5 add ht mul 5 sub moveto" CR 
    HEX &( EMIT 8 31 I - * 2 .R &) EMIT ." show" CR DECIMAL
  LOOP
;
: POSTLUDE 
  ." 0 wt mul 33 ht mul moveto " CR 
  ." /Times-Roman findfont 14 scalefont setfont" CR 
  ." (QUICK REFERENCE PAGE FOR 8086 ASSEMBLER) show" CR
  ." /Times-Roman findfont 6 scalefont setfont" CR 
  ." 6.5 wt mul -12 moveto " CR 
  ." (By Albert van der Horst DFW Holland) show" CR
  ." showpage" CR 
;

: OPCODE
   8 /MOD 31 SWAP - SWAP
   . ." .15 add wt mul " . ." .5 add ht mul 5 sub moveto " CR                   
   &( EMIT ID. &) EMIT ." show" CR
;       
: OPCODES
  256 0 DO 
    DUP >MASK I AND  OVER >INST XOR $FF AND 0= IF
       DUP I OPCODE                                       
    THEN
  LOOP
  DROP
;

: QUICK-REFERENCE
    PRELUDE
    !DISS   !TALLY
    STARTVOC BEGIN
        DUP IS-PI IF
           DUP OPCODES
        THEN
        >NEXT%
    DUP VOCEND? UNTIL DROP
    POSTLUDE
;

