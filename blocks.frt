                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
  HEX                                                          
: LC@ L@ FF AND ;                                              
: LC! OVER OVER L@ FF00 AND >R ROT R> OR ROT ROT L! ;          
 : VV B800 0 ;   VARIABLE BLUE 17 BLUE !                       
: VERTICAL-FRAME 20 0 DO BLUE @ VV I 50 * + 81 + LC! 2 +LOOP ; 
 : HORIZONTAL-FRAME 82 1 DO BLUE @ VV A00 + I + LC! 2 +LOOP ;  
 : FRAME 17 BLUE ! VERTICAL-FRAME HORIZONTAL-FRAME ;           
 : BLACK 7 BLUE ! VERTICAL-FRAME HORIZONTAL-FRAME ;            
 DECIMAL                                                       
: ?LEAVE-BLOCK IF SRC CELL+ @ IN ! THEN ;                      
: ?16 ;         : ?32 -1 ?LEAVE-BLOCK ;                        
: ?PC "BIOS" FOUND 0= ?LEAVE-BLOCK ;                           
: ?LI "LINOS" FOUND 0= ?LEAVE-BLOCK ;                          
"INCLUDED" FOUND 0= ?LEAVE-BLOCK                               
: INCLUDE &" (PARSE) INCLUDED ;                                
                                                               
COPYRIGHT (c) 2000 STICHTING DFW , THE NETHERLANDS             
           I have a beautiful disclaimer,                      
     but this screen is too small to contain it.               
THE FILE COPYING THAT IS SUPPLIED WITH THIS PROGRAM APPLIES.   
AN EXCERPT FOLLOWS. BECAUSE THE PROGRAM IS LICENSED FREE OF    
CHARGE THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT     
ALLOWED BY APPLICABLE LAW. UNLESS OTHERWISE STATED IN WRITING  
THE PROGRAM IS SUPPLIED "AS IS". IN NO EVENT                   
UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING      
WILL THE COPYRIGHT OWNER BE LIABLE FOR DAMAGES OF ANY KIND     
RELATED TO USE OF, ABUSE OF OR INABILITY TO USE THIS PROGRAM.  
THIS IS COPYRIGHTED SOFTWARE. YOUR RIGHTS (IN PARTICULAR YOUR  
RIGHTS TO RESTRICT THE RIGHTS OF OTHERS) ARE RESTRICTED.       
                                                               
           THIS IS A WARNING ONLY.                             
 THE CONTENT OF THE FILE COPYING IS LEGALLY BINDING.           
( Commands aplicable to 32 bit mode A0jun29 )                  
  HEX   ( This block only loaded for a 32 bits Forth.)         
 0 CONSTANT CS_START                                           
 : LC@ SWAP 10 * + CS_START - C@ ;                             
 : LC! SWAP 10 * + CS_START - C! ;                             
 : VV B800 0 ;   VARIABLE BLUE 17 BLUE !                       
: VERTICAL-FRAME 20 0 DO BLUE @ VV I 50 * + 81 + LC! 2 +LOOP ; 
 : HORIZONTAL-FRAME 82 1 DO BLUE @ VV A00 + I + LC! 2 +LOOP ;  
 : FRAME 17 BLUE ! VERTICAL-FRAME HORIZONTAL-FRAME ;           
 : BLACK 7 BLUE ! VERTICAL-FRAME HORIZONTAL-FRAME ;            
 DECIMAL   : ?LEAVE-BLOCK IF SRC CELL+ @ IN ! THEN ;           
: ?16 -1 ?LEAVE-BLOCK ;         : ?32 ;                        
: ?PC "BIOS" FOUND 0= ?LEAVE-BLOCK ;                           
: ?LI "LINOS" FOUND 0= ?LEAVE-BLOCK ;                          
"INCLUDED" FOUND 0= ?LEAVE-BLOCK                               
: INCLUDE &" (PARSE) INCLUDED ;                                
                                                               
 MSG # 1 : EMPTY STACK                                         
 MSG # 2 : DICTIONARY FULL                                     
 MSG # 3 : HAS INCORRECT ADDRESS MODE                          
 MSG # 4 : ISN'T UNIQUE                                        
 MSG # 5 : INVALID NUMBER, STRING OR OTHER DENOTATION          
 MSG # 6 : DISK RANGE ?                                        
 MSG # 7 : FULL STACK                                          
 MSG # 8 : DISC ERROR !                                        
 MSG # 9 : UNRESOLVED FORWARD REFERENCE                        
 MSG # 10 : IMPROPER CHARACTER DENOTATION                      
 MSG # 11 : WORD IS NOT FOUND                                  
 MSG # 12 : NOT RECOGNIZED                                     
 MSG # 13 : UNKNOWN                                            
 MSG # 14 : SAVE/RESTORE MUST RUN FROM FLOPPY                  
 ( FIG FORTH FOR CP/M 2.0 ADAPTED BY A. VD HORST HCCH )        
 ( ERROR MESSAGES   )                                          
 MSG # 17 : COMPILATION ONLY, USE IN DEFINITION                
 MSG # 18 : EXECUTION ONLY                                     
 MSG # 19 : CONDITIONALS NOT PAIRED                            
 MSG # 20 : DEFINITION NOT FINISHED                            
 MSG # 21 : IN PROTECTED DICTIONARY                            
 MSG # 22 : USE ONLY WHEN LOADING                              
 MSG # 23 : OFF CURRENT EDITING SCREEN                         
 MSG # 24 : DECLARE VOCABULARY                                 
 MSG # 25 : LIST EXPECTS DECIMAL                               
 MSG # 26 : AS: PREVIOUS INSTRUCTION INCOMPLETE                
 MSG # 27 : AS: INSTRUCTION PROHIBITED IRREGULARLY             
 MSG # 28 : AS: UNEXPECTED FIXUP/COMMAER                       
 MSG # 29 : AS: DUPLICATE FIXUP/UNEXPECTED COMMAER             
 MSG # 30 : AS: COMMAERS IN WRONG ORDER                        
 MSG # 31 : AS: DESIGN ERROR, INCOMPATIBLE MASK                
 (  DEBUG   SCR#6     A0JUN21 AVDHORST DFW HOLLAND)            
( MSG # 32 : AS: PREVIOUS OPCODE PLUS FIXUPS INCONSISTENT )    
0 IVAR BASE'                                                   
 : <HEX   BASE @ BASE' ! HEX ;       ( 0/1  SWITCH TO HEX)     
 : HEX>   BASE' @ BASE !     ;       ( 1/0  AND BACK)          
( Add a . after 4 digits )                                     
 : 4?  1+ 4 MOD 0= IF &. HOLD THEN ;                           
( Generate string with hex format of DOUBLE of LEN digits)     
 : (DH.) <HEX <# 1- 0 DO # I 4? LOOP # #> HEX> ;               
 : DH. 16 (DH.) TYPE ; (  print DOUBLE in hex )                
 : H.  S>D 8 (DH.) TYPE ; ( print SINGLE in hex )              
 : B.  S>D 2 (DH.) TYPE ; ( print BYTE in hex )                
                                                               
 : BASE?  BASE @ B. ;                ( 0/0 TRUE VALUE OF BASE) 
 : ALIAS  (WORD) (CREATE) LATEST 3 CELLS MOVE ;                
 : HIDE (WORD) FOUND DUP 0= 11 ?ERROR HIDDEN ;                 
 <HEX ( DEBUG SCR#7 )                                          
 : TO-PRINT DUP DUP BL < SWAP 7F > OR IF DROP [CHAR] . THEN ;  
 : .CHARS  [CHAR] | EMIT 0 DO DUP I + C@ TO-PRINT EMIT LOOP    
       [CHAR] | EMIT ;                                         
 : BYTES 0 DO                                                  
            DUP I + C@ B.                                      
            I 2 MOD IF SPACE THEN                              
        LOOP ;                                                 
:  DUMP   ( 2/0  DUMPS FROM ADDRESS-2 AMOUNT-1 BYTES)          
    OVER + SWAP                                                
    DO                                                         
        CR I H. ." : "                                         
        I 0F AND DUP 5 2 */ SPACES 10 SWAP -                   
        I   OVER BYTES   OVER .CHARS   DROP DROP               
    10 I 0F AND - +LOOP         CR                             
;    HEX>                                                      
 ." SYSTEM ELECTIVE CP/M FIGFORTH EXTENSIONS 3.43    AH"       
 : IVAR CREATE , ;   : ^ .S ;                                  
  -1 CELL+ LOAD  ( 16/32 BIT DEPENDANCIES)                     
 ( MAINTENANCE )  100 LOAD   34 LOAD                           
( HEX CHAR DUMP)  6 LOAD 32 LOAD 7 LOAD 39 LOAD ( i.a. editor) 
( STRINGS      )  35 LOAD 36 LOAD 37 LOAD                      
 ( EDITOR ) 109 LOAD                                           
 ( CP/M READ WRITE LOAD    17 LOAD 21 LOAD 24 LOAD 21: BUGS)   
 ( KRAKER         10 16 THRU )                                 
 ( CRC             71 LOAD   )                                 
 ( ASSEMBLER 8080  74 LOAD   )                                 
 ( ASSEMBLER 80x86 SAVE-BLOCKS) EXIT   120 LOAD   97 98 THRU   
                                                               
 2 LIST    : TASK ;                                            
 ( OLD:  NEW SYSTEM      23 LOAD   )                           
( STAR PRINTER 31 LOAD ) ( CP/M CONVERT 80 LOAD )              
 ." QUADRUPLE ARITHMETIC 08-02-84 "                            
 : ADC ( n1,n2-n,c  add, leave sum and carry)                  
    0 SWAP 0 D+ ;                                              
 : 2M+ ( d1,d2-d,c  add double )                               
   >R SWAP >R    ADC R> ADC   R> SWAP >R                       
   ADC R> + ;                                                  
 : 3M+ ROT >R 2M+ R> ADC ;                                     
 : 4M+ ROT >R 3M+ R> ADC ;                                     
 : 2U*  ( d1,d2-q unsigned product)                            
 ROT ( l1,l2,h2,h1)    OVER OVER UM* >R >R .S                  
 ROT ( l1,h2,h1,l2)    OVER OVER UM* >R >R .S                  
 SWAP DROP ROT ROT ( l2,l1,h2) OVER OVER UM* >R >R .S          
 DROP ( l1,l2)    UM* .S R> ADC .S R> ADC .S                   
  IF ( carry) R> R> 2M+ 1+ ." C" ELSE                          
              R> R> 2M+    ." NC" THEN  .S                     
  R> R> 2M+ DROP .S ;                                          
 CR ." A0MAR30  FORTH KRAKER >1<  ALBERT VAN DER HORST "       
 0 IVAR SELTAB 60 CELLS ALLOT   SELTAB IVAR SELTOP             
 : T,  ( N--. Put N in select table)                           
     SELTOP @ !  0 CELL+ SELTOP +!  ;                          
 : CFOF ( --N Get dea of word following )                      
    [COMPILE] ' CFA> ;                                         
                                                               
 : ID.. CFA> ID. ; ( cfa--. Print a words name )               
 : ID.+ DUP @ ID.. CELL+ ; ( dip -- dip' Print a words name )  
 : SEL@    ( N--M,F F="value N present in table" )             
    ( if F then M is vector address else M=N)                  
       0 SWAP ( initialise flag)                               
       SELTOP @ SELTAB DO                                      
           DUP I @ = IF ( FOUND!) DROP DROP 1 I CELL+ @ THEN   
       0 CELL+ CELL+  +LOOP        SWAP   ( get flag up)  ;    
                                                               
 CR ." 84NOV24  FORTH KRAKER >1a<  ALBERT VAN DER HORST "      
                                                               
                                                               
                                                               
                                                               
( DEA--DEA Get the DEA of the word defined after the CFA one)  
                                                               
: NEXTD CURRENT @ BEGIN ( CR OVER ID. OVER ID.) 2DUP >LFA @ <> 
WHILE >LFA @ DUP 0= IF 1000 THROW THEN REPEAT SWAP DROP ;      
                                                               
                                                               
 : NEXTC ( DEA--CFA Like previous definition, giving CFA)      
   NEXTD >CFA ;                                                
                                                               
 VARIABLE LIM   : H.. BASE @ >R HEX . R> BASE ! ;  : B.. H.. ; 
                                                               
  CR ." A0MAR30  FORTH KRAKER >2<  ALBERT VAN DER HORST "      
 : (KRAAK) ( DEA--. Decompile a word from its DEA)             
  (  DUP NEXTD >NFA @ LIM ! Get an absolute limit)             
    DUP @ SEL@ IF ( Is content of CFA known?)                  
       EXECUTE ( Assuming CFA also on stack)                   
    ELSE                                                       
        DROP CR                                                
        DUP >CFA @ OVER >PHA = IF                              
           ." Code definition : " ELSE ." Can't handle : "     
       THEN ID.. CR                                            
    THEN ;                                                     
: KRAAK  ( Use KRAAK SOMETHING to decompile the word SOMETHING)
     CFOF (KRAAK) ;                                            
 : ?IM  ( CFA--f tests whether word IMMEDIATE )                
      >FFA @ 4 AND ;                                           
 : ?Q KEY? IF QUIT THEN ; ( NOODREM)                           
 CR ." A0apr11  FORTH KRAKER >3<  ALBERT VAN DER HORST "       
 : BY ( DEA --. the CFA word is decompiled using : )           
   T, CFOF T, ; ( a word from the input stream )               
 ( Example of a defining word decompilation)                   
 ( It is done by examples of the defined words )               
 : -co DUP CFA> >DFA @ CR H.. ." CONSTANT " ID.. CR ;          
        CFOF BL @ BY -co                                       
 : -va DUP CFA> >DFA @ @ CR H.. ." VARIABLE " ID.. CR ;        
        CFOF RESULT @ BY -va                                   
 : -us DUP CFA> >DFA C@ CR B.. ."  USER " ID.. CR ;            
        CFOF FENCE @ BY -us                                    
 : ITEM ( 1/1 Desinterpret next item, increments pointer)      
     DUP @ SEL@ ( Something special ?)                         
     IF EXECUTE ( The special) ELSE                            
        DUP ?IM IF ." [COMPILE] " THEN ID.. CELL+              
     THEN ;                                                    
 CR ." A0MAR30  FORTH KRAKER >4<  ALBERT VAN DER HORST "       
 CFOF TASK @ CONSTANT DOCOL ( Get the  DOCOLON address )       
 ( Decompilation of special high level words)                  
  : -hi CR ." : " DUP DUP ID.. CELL+ @ CR                      
   BEGIN ?Q DUP @  LIT EXIT <> ( >R DUP LIM @ < R> AND ) WHILE 
        ITEM REPEAT                                            
   CR DROP ." ;" ?IM IF ."  IMMEDIATE " THEN CR ;              
       CFOF TASK @  BY -hi                                     
 ( for all -words: 1/1 pointer before afd after execution)     
 : -lit CELL+ DUP @ H.. CELL+ ;                                
     CFOF LIT BY -lit                                          
 : -0br CR ." 0BRANCH [ " -lit ." , ] " ;                      
     CFOF 0BRANCH BY -0br                                      
 : -br  CR ." BRANCH  [ " -lit ." , ] " ;                      
     CFOF BRANCH BY -br                                        
                                                               
 CR ." A0APR11  FORTH KRAKER >5<  ALBERT VAN DER HORST "       
  : -sk CELL+ CR ." [ " &" EMIT DUP $@ TYPE &" EMIT            
         ."  ] DLITERAL " $@ + 4 CELLS + ;                     
                      CFOF SKIP BY -sk                         
: -sq CELL+ DUP $@ CR [CHAR] " EMIT BL EMIT TYPE [CHAR] " EMIT 
  BL EMIT $@ + ;                     CFOF ($) BY -sq           
  : -do CR ." DO " CELL+ CELL+ ;     CFOF (DO) BY -do          
  : -qdo CR ." ?DO " CELL+ CELL+ ;   CFOF (?DO) BY -qdo        
  : -lo CR ." LOOP " CELL+ CELL+ ;   CFOF (LOOP) BY -lo        
  : -pl CR ." +LOOP " CELL+ CELL+ ;  CFOF (+LOOP) BY -pl       
                                                               
  (  : -cm ID.+ ID.+ ;                CFOF COMPILE BY -cm )    
  : -pc CR ." ;CODE plus code (suppressed)"                    
  ( DIRTY TRICK FOLLOWING :                                    
make decompile pointer point to exit!)                         
    DROP ' TASK >DFA @ ;             CFOF (;CODE) BY -pc       
 CR ." KRAAKER"                                                
 : -dd CFA> ." CREATE DOES> word " ID.. CR ;                   
        CFOF FORTH @ BY -dd                                    
 : KRAAK-FROM ( .--. Kraak, starting with following word)      
   CFOF                                                        
   BEGIN                                                       
      DUP NEXTD LATEST < WHILE                                 
      NEXTC DUP (KRAAK)                                        
   REPEAT DROP ;    EXIT Remainder is broke                    
 0 IVAR aux                                                    
 : PEMIT $7F AND 5 BDOS DROP ;                                 
 : TO-LP-KRAAK-FROM                                            
   ' EMIT >CFA >R       ' PEMIT >CFA ' EMIT >DFA !             
   KRAAK-FROM           R> ' EMIT >DFA ! ;                     
                                                               
                                                               
 ( DISK IO SCREEN 17 SCHRIJVEN >1< VERSIE #1)                  
 <HEX  0 IVAR FCB2   21 ALLOT  ( BUG: 2nd goes wrong)          
 : CLEAN-FCB DUP 21 0 FILL  1+ 0B 20 FILL ;                    
 : FILL-FCB 22 WORD                                            
    1+ HERE  COUNT ROT SWAP CMOVE  ;                           
 : SET-DMA  1A BDOS DROP ;                                     
 : ?PRES   FCB2 0F BDOS 0FF - IF ." ALREADY PRESENT" QUIT THEN 
    FCB2 10 BDOS DROP ;                                        
   18 19 THRU                                                  
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
 ( SCR # 18 SCHRIJVEN >2<   )                                  
 0 IVAR DISK-BUFFER-W 100 ALLOT                                
 DISK-BUFFER-W IVAR POINTER-W                                  
 : .OPENW FCB2 CLEAN-FCB FCB2 FILL-FCB ?PRES                   
   FCB2 16 BDOS 0FF = IF ." DISK FULL " QUIT THEN              
   DISK-BUFFER-W POINTER-W ! ;                                 
 : .CLOSEW                                                     
      DISK-BUFFER-W SET-DMA FCB2 15 BDOS . ." LAST RECORD" CR  
            FCB2 10 BDOS . ." CLOSE STATUS" CR ;               
 0A0D IVAR CRLF    1A IVAR CTRLZ                               
 : MOVE-DOWN   -80 POINTER-W +!                                
               DISK-BUFFER-W 80 OVER + SWAP 80 CMOVE ;         
 : TO-DISK DUP >R POINTER-W @ SWAP CMOVE                       
           R> POINTER-W +!                                     
           POINTER-W @ DISK-BUFFER-W -                         
           80 >  IF                                            
  ( SCREEN #19 SCHRIJVEN  >3<)                                 
              DISK-BUFFER-W SET-DMA FCB2 15 BDOS .             
              MOVE-DOWN                                        
          THEN ;                                               
                                                               
 : .WRITE  ( 2/0 WRITE SCREEN-1 .. SCREEN-2 TO DISK)           
      1+ B/SCR * SWAP B/SCR * ( GET START BUFFER #'S)          
        DO I BLOCK DUP                                         
        40 -TRAILING TO-DISK  CRLF 2 TO-DISK                   
        40 + 40 -TRAILING TO-DISK CRLF 2 TO-DISK               
      LOOP CTRLZ 1 TO-DISK                                     
 ;   HEX>                                                      
                                                               
                                                               
                                                               
                                                               
 ( DISK IO, LEZEN )                                            
 <HEX  ( BUG: 64 char lines go wrong)                          
 0 IVAR DISK-BUFFER-R  80 ALLOT 0 IVAR POINTER-R               
  0A CONSTANT "LF"  0D CONSTANT "CR"                           
  1A CONSTANT ^Z    DISK-BUFFER-R 80 + CONSTANT END-BUF        
  0 IVAR EOF                                                   
 : .OPENR   FCB2 DUP CLEAN-FCB FILL-FCB                        
       END-BUF POINTER-R !                                     
       FCB2 0F BDOS 0FF = IF ." NOT PRESENT" QUIT THEN         
       0 EOF ! ;                                               
 : .CLOSER   FCB2 10 BDOS . ." CLOSE STATUS" CR ;              
                                                               
                                                               
               21 22 THRU                                      
                                                               
                                                               
 ( SCR # 21,  TWEEDE SCREEN VAN CP/M READ)                     
 : ?EMPTY ( POINTER -- CORRECTED PNR, READ SECTOR IF AT END)   
     DUP END-BUF = IF DISK-BUFFER-R SET-DMA  FCB2 14 BDOS .    
                    DROP DISK-BUFFER-R THEN  ;                 
 : GET-CHAR                                                    
    POINTER-R @                                                
      ?EMPTY                   ( GET NEW BUFFER IF NEEDED)     
      DUP C@ "LF" = IF 1+ ?EMPTY THEN ( SKIP OVER LF)          
      DUP C@ SWAP              ( GET CHAR, POINTER ON TOP)     
      OVER ^Z =                                                
      IF 1 EOF ! ELSE 1+ THEN ( INCREMENT POINTER UNLESS AT ^Z)
    POINTER-R !  ;                                             
                                                               
                                                               
                                                               
                                                               
 CR ." READ CP/M files >3< AH   84/06/13"                      
 : GET-LINE ( ADR -- . reads a line to ADR )                   
      DUP 40 20 FILL ( preset spaces )                         
      41 OVER + SWAP ( max $41 char to a line, CR!)            
      DO  GET-CHAR                                             
          DUP "CR" = IF DROP 20 LEAVE THEN                     
          DUP ^Z   = IF DROP 20 LEAVE THEN                     
          I C! ( may leave spurious 81th space)                
      LOOP  ;                                                  
 : .READ ( 2/0 READ SCREEN-2 TO SCREEN -1)                     
      1+ B/SCR * SWAP B/SCR * ( get start buffer #'s)          
      DO  I BLOCK DUP GET-LINE                                 
          DUP 40 + GET-LINE  81 + 0 SWAP C! UPDATE             
          I #BUFF MOD 0= IF ( full load of buffers) FLUSH THEN 
      LOOP                                                     
; HEX>                                                         
 ( 01-APR-83 LADEN VAN CP/M FILE  #1 )                         
 ( EXAMPLE: .OPENR TEMP" 25 26 .LOAD .CLOSER )                 
 <HEX  0 IVAR LBUF 3E ALLOT 0 C,                               
 : I-F-A ( ADRES -- . ,INTERPRET FROM ADDRESS )                
     TIB @ >R  IN @ >R  ( SAVE CURRENT INTERPRET POSITION)     
     TIB !     0 IN !   ( NEW POSITION)                        
     0 INTERPRET                                               
     >R IN !   >R TIB ! ( RESTORE)  ;                          
                                                               
 : .LOAD ( LOAD THE CPM FILE SPECIFIED IN FCB2 )               
         BEGIN   LBUF DUP GET-LINE I-F-A                       
         EOF @ UNTIL ;                                         
                                                               
                                                               
    HEX>                                                       
                                                               
( CORE EXTENSION WORDS ASSUMED BY HAYDN, OTHERS TEST NEEDS)    
 -1 CONSTANT TRUE       0 CONSTANT FALSE                       
 -1 CELL+ LOAD                                                 
: STOPIT IF LEAVE-BLOCK THEN ;                                 
"!CSP" FOUND STOPIT                                            
 : !CSP ; : ?CSP ;                                             
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
  ( EXTENDING THE FORTH SYSTEM #1 84/4/12 A.H.)                
( <HEX  OLD EXAMPLE                                            
(  : NEW-SYSTEM   ( Generates a new FORTH system, )            
(                 ( using the CP/M SAVE command)               
(       LATEST N>P NFAO 10C ! ( Define new topmost word)       
(       ( Initial value for VOC-LINK and FENCE:)               
(       HERE DUP 11C ! 11E !                                   
(       HERE 100 / DECIMAL CR                                  
(       CR ." TYPE: SAVE" . ." NEWFORTH.COM"                   
(       BYE                                                    
(  ;     HEX>                                                  
                                                               
                                                               
                                                               
                                                               
                                                               
 ." GILBREATH's benchmark - BYTE jan 83 "  ( VERSIE #1)        
 8190 CONSTANT SIZE                                            
 0 IVAR FLAGS      SIZE ALLOT                                  
                                                               
 : DO-PRIME                                                    
     FLAGS SIZE 1 FILL                                         
     0 ( 0 COUNT ) SIZE 0                                      
     DO FLAGS I + C@                                           
        IF I DUP + 3 +  ( DUP . )                              
           DUP I +                                             
           BEGIN DUP SIZE <                                    
           WHILE 0 OVER FLAGS +  C!  OVER + REPEAT             
           DROP DROP 1+                                        
        THEN                                                   
     LOOP                                                      
     . ." PRIMES" ;                                            
 ." ERATOSTHENES >1< Variables - A. van der Horst"  CR         
 ( User specified variables:)                                  
 ( 52) 80 IVAR CH/L  ( Characters per line)                    
 ( 22) 24 IVAR LN/P  ( Lines per page)                         
  1 IVAR PAUSE ( Boolean: pause between pages)                 
                                                               
 ( Other:)                                                     
 6250 CONSTANT SIZE ( 16 numbers pro byte)                     
 0 IVAR FLAGS      SIZE ALLOT                                  
 FLAGS SIZE + CONSTANT END-FLAGS                               
 0 IVAR LIM     ( part of FLAGS considered)                    
 0 IVAR C#      0 IVAR L#  ( char and line counter)            
 0 IVAR THOUSANDS ( #  thousand to be sieved)                  
 0 IVAR MILS      ( Contains current thousand)                 
 0 IVAR MANTISSA  ( The current thousands is to be printed)    
 28 31 THRU                                                    
 ." ERATOSTHENES >2< Pretty printing - A. van der Horst"  CR   
 : FFEED  PAUSE @ IF CR ." KEY FOR NEXT SCREEN" KEY DROP THEN  
     12 EMIT CR ." ERATOSTHENES SIEVE -- PRIMES LESS THAN"     
     THOUSANDS @ 5 .R ."  000" CR 2 L# ! 1 MANTISSA ! ;        
 : ?P ( LENGTH -- . , give FF if LENGTH lines don't fat)       
      DUP L# +! L# @ LN/P @ > IF FFEED L# +! ELSE DROP THEN ;  
 : NEWLINE  ( Start at a new line, maybe with a mantissa)      
     1 ?P CR ( Checks first)                                   
     MANTISSA @ IF MILS @ 6 .R ELSE 6 SPACES THEN              
     6 C# !   0 MANTISSA ! ;                                   
 : ?L ( LENGTH -- . , give LF if LENGTH char's don't fit)      
      DUP C# +! C# @ CH/L @ >                                  
      IF NEWLINE C# +! ELSE DROP THEN ;                        
 : .P   4 ?L SPACE 0 <# # # # #> TYPE ;                        
 : INIT-P  FFEED NEWLINE  ;                                    
                                                               
 ." ERATOSTHENES >3< Bit manipulation - A. van der Horst " CR  
   HEX                                                         
 : NOT   0FF XOR ( N -- N  FLIP ALL BITS OF N) ;               
 0 IVAR S-MASK -1 CELLS ALLOT                                  
01 C, 02 C, 04 C, 08 C, 10 C, 20 C, 40 C, 80 C,                
 0 IVAR C-MASK -1 CELLS ALLOT                                  
             01 NOT C, 02 NOT C, 04 NOT C, 08 NOT C,           
             10 NOT C, 20 NOT C, 40 NOT C, 80 NOT C,           
 : INIT-T   FLAGS SIZE 0FF FILL ; ( Preset to 'prime')         
 DECIMAL                                                       
 : 8/MOD   0 8 UM/MOD ; ( May be redefined in assembler )      
 : CLEAR-B ( BIT# --  clears the specified bit)                
           8/MOD FLAGS + SWAP  ( Address in flags table)       
           C-MASK + C@         ( Get mask)                     
           OVER C@ AND SWAP C! ( Clear the bit)  ;             
                                                               
 ." ERATOSTHENES >4< Bit manipulation - A. van der Horst " CR  
 : SET-B ( BIT# --  sets the specified bit)                    
           8/MOD FLAGS + SWAP  ( Address in flags table)       
           S-MASK + C@         ( Get mask)                     
           OVER C@ OR SWAP C!  ( Store with bit set)  ;        
 : TEST-B ( BIT# -- FLAG  Gets a FLAG testable by IF)          
           8/MOD FLAGS + C@ SWAP  ( Get flag)                  
           S-MASK + C@ AND        ( Result: =0 or #0)     ;    
 : FLIP ( PRIME,START -- .  , marks multiples of PRIME as      
        (  non prime starting with START)                      
           BEGIN  DUP LIM @ U<  WHILE                          
                  DUP CLEAR-B  OVER +                          
           REPEAT   DROP DROP ;                                
 : CHECK SIZE 16 UM* 1000 UM/MOD  THOUSANDS @ U< IF            
       ." INCREASE SIZE " ABORT ELSE DROP DROP THEN ;          
                                                               
 ." ERATOSTHENES >5< Main program - A. van der Horst " CR      
 : BATCH1 ( First batch of 500 numbers)                        
      500 1 ( Only odd numbers)                                
     DO I TEST-B                                               
        IF I DUP + 1 + DUP .P ( get prime number)              
           I FLIP THEN ( Mark multiple as non-prime)           
     LOOP ;                                                    
 : BATCH ( OFFSET --  every  following batch )                 
      500 0                                                    
      DO DUP I + TEST-B IF I DUP + 1 + .P THEN                 
      LOOP DROP ;                                              
 : SIEVE  ( N --  Makes list of primes <N*1000 )               
     DUP THOUSANDS !   DUP CHECK   500 * LIM !  0 MILS !       
     INIT-T INIT-P 2 .P BATCH1                                 
     THOUSANDS @ 1                                             
     DO I MILS !  1 MANTISSA !  NEWLINE I 500 * BATCH LOOP ;   
 ." DEFINIEER ^. EN  ''.  85JAN06 ALBERT VAN DER HORST"        
 <HEX                                                          
  : TOH 30 - DUP 9 > IF 7 - THEN ;                             
(    1 WIDTH !                                                 
( : &. ( 0/1 Leaves ASCII character at .  f.i. &A leaves 41H)  
(   HERE 2 + C@ [COMPILE] LITERAL ; IMMEDIATE                  
( : ^. ( 0/1 leaves control character at . f.i. ^A leaves 01H) 
(   HERE 2 + C@ 1F AND [COMPILE] LITERAL ; IMMEDIATE           
( : $.. ( 0/1 leaves hex number f.i. $0A leaves 0AH)           
(   HERE 2 + C@ TOH 10 * HERE 3 + C@ TOH + [COMPILE] LITERAL ; 
( IMMEDIATE                                                    
( : $.... ( 0/1 leaves hex number 16 bits)                     
(    0 HERE 6 + HERE 2 + DO 10 * I C@ TOH + LOOP               
(    [COMPILE] LITERAL ; IMMEDIATE                             
(    1F WIDTH ! )                                              
  1B CONSTANT ESC    0F CONSTANT SI   0E CONSTANT SO HEX>      
 ." 84NOV25 Initialize STAR-printer AH "  <HEX                 
 : PEMIT 7F AND 5 BDOS DROP ;                                  
 : PCR   0D PEMIT   0A PEMIT ;                                 
 : INIT-STAR ( N--. N is lines per pages)                      
    ESC PEMIT "@ PEMIT ESC PEMIT "C PEMIT ( TOS) PEMIT ;       
 : CONDENSED  ESC PEMIT "P PEMIT "3 PEMIT ;                    
 : EMPHASIZED ESC PEMIT  "E PEMIT ;                            
 : DOUBLE ESC PEMIT "G PEMIT ;                                 
 : BOLD EMPHASIZED DOUBLE ;                                    
 ( 137 CH/L !    60 LN/P !     0 PAUSE ! )                     
  : PSPACES  ( 1/0 print N-1 spaces)                           
    0 DO 20 PEMIT LOOP ;                                       
  : PTYPE  ( ADDRESS,LENGTH -- . PRINT LENGTH CHAR AT ADDRESS) 
          ?DUP IF                                              
          OVER + SWAP DO I C@ PEMIT LOOP THEN ;                
  : P."  "" WORD COUNT PTYPE ;       34 LOAD                   
 ( SUPER-QUAD)  ?PC                                            
   0 IVAR L                                                    
 : CONDENSED 34 MODE ;                                         
 :  HEADER  CR DUP 2 + SWAP                                    
    DO 3  SPACES ." SCR #" I 4 .R 54  SPACES LOOP ;            
 : 1LINE  L @ OVER (LINE)  TYPE                                
   L @ 2 .R SPACE  L @ 16 + SWAP (LINE)  TYPE CR ;             
  : SUPER-DUPE                                                 
    2 /MOD SWAP DROP 2 *                                       
    DUP HEADER CR                                              
    16 0 DO  I L ! DUP 1LINE                                   
    LOOP  ;                                                    
 : SQ CONDENSED SUPER-DUPE 2 + SUPER-DUPE DROP ;               
                                                               
                                                               
                                                               
( Elementary string: $@ $! $+! $C+     A0apr03-AH)             
( All this should probably be low level )                      
                                                               
 : $. TYPE ;                                                   
 : C+! >R R@ C@ + R> C! ;                                      
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( PARSING  & STRINGS                   A0apr03-AH)             
( HANDY & Preparation for ANSI-fication)                       
: PARSE WORD COUNT ;     : ($) R@ $@ DUP CELL+ R> + >R ;       
                                                               
                                                               
                                                               
: C"  HERE DENOTATION POSTPONE " FORTH                         
    DUP @ SWAP CELL+ 1 - C! ( Make it brain damaged)           
    POSTPONE DROP POSTPONE 1+ ; IMMEDIATE                      
                                                               
: STRING CREATE &" (PARSE) $, DROP DOES> $@ ;                  
                                                               
                                                               
                                                               
                                                               
                                                               
 ( STRING MANIPULATIONS : $I $S A0APR04-AH) ( HORRIBLE!)       
 : $I ( cs, del - Index   Index is the first place del is found
in the string else 0.                                          
It is assumed del cannot be a valid addr )                     
OVER 0= IF DROP DROP DROP 0 ELSE  DUP >R                       
     ROT ROT OVER + SWAP DO                                    
     DUP I C@ = IF DROP I LEAVE THEN                           
   LOOP R> OVER = IF DROP 0 THEN  ( Tricky)                    
THEN ;                                                         
 : $S ( cs, del -- cs2 , cs1 )  ( Splits the text at the del ) 
   ( in two, if not present, cs2 is a null string )            
   >R OVER OVER R> $I  DUP IF                                  
     >R OVER R@ SWAP - ( Length before delimiter )             
     SWAP OVER - 1 - ( Length after delimiter)                 
     R> 1+ SWAP                                                
  ELSE ( DROP 0) 0 THEN  2SWAP ;                               
(  -HEADING                   89jul16-AH )                     
 : -HEADING ( $T,$C -$T,$C   Like -TRAILING, removes)          
    BEGIN                        ( heading blanks )            
      OVER C@ BL = OVER 0= 0=  AND                             
    WHILE                                                      
      1 - SWAP 1 + SWAP                                        
    REPEAT  ;                                                  
 : $= ROT 2DUP SWAP - >R                                       
     MIN CORA DUP IF RDROP ELSE DROP R> THEN ;                 
 : $@=  ( S1 S2 --F string at address S1 equal to other one)   
   >R $@ R> $@ $=                                              
;                                                              
                                                               
                                                               
                                                               
                                                               
?PC  <HEX ( DEBUG SCR#7 )                                      
:  DUMP2   ( SEG ADDRESS AMOUNT - ..)                          
    OVER + SWAP FFF0 AND                                       
    DO                                                         
        CR DUP H. I H. ." : "                                  
        I                                                      
        10 0 DO                                                
            2DUP I + L@ B.                                     
            I 2 MOD IF SPACE THEN                              
        LOOP  [CHAR] | EMIT                                    
        10 0 DO 2DUP I + L@ FF AND TO-PRINT EMIT LOOP          
        [CHAR] | EMIT DROP                                     
    10 +LOOP CR DROP                                           
;    HEX>                                                      
                                                               
                                                               
?32                                                            
HEX                                                            
-100000 10 +ORIGIN +!                                          
FLUSH COLD                                                     
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Experiment with GDT etc.) HEX                                
( 32 K GDT AT 0001.8000 ) 2800 CONSTANT GDT-SEGMENT            
7FFF IVAR GDT 2.8000 SWAP , ,                                  
7C0 CONSTANT CODE-SEGMENT ( The same for real and prot)        
CODE-SEGMENT  10 * CONSTANT CODE-START                         
: GDT! GDT-SEGMENT SWAP L! ;   1800 CONSTANT DATA-SEGMENT      
: CODE! CODE-SEGMENT + GDT! ;   : DATA! DATA-SEGMENT + GDT! ;  
: PREPARE-CS                                                   
  FFFF 0 CODE!   CODE-START 2 CODE!                            
  9A00 4 CODE!   008F 6 CODE! ;                                
: PREPARE-DS                                                   
  FFFF 0 DATA! CODE-START 2 DATA!                              
  9200 4 DATA! 008F 6 DATA! ;                                  
 CODE LOAD-GDT CLI, 0F C, 01 C, 10 C, MEM| GDT MEM,            
NEXT C; DECIMAL                                                
                                                               
( Experiment with GDT etc.) HEX                                
7C8 CONSTANT CS-32 ( 32 BITS CODE SEGMENT)                     
10  CONSTANT DS-32 ( 32 BITS DATA SEGMENT)                     
: CS32! CS-32 + GDT! ;   : DS32! DS-32 + GDT! ;                
: PREPARE-CS32                                                 
  FFFF 0 CS32!   CODE-START 2 CS32!                            
  9A00 4 CS32!   00CF 6 CS32! ;                                
: PREPARE-DS32                                                 
  FFFF 0 DS32!   CODE-START 2 DS32!                            
  9200 4 DS32!   00CF 6 DS32! ;                                
PREPARE-DS PREPARE-CS PREPARE-CS32 PREPARE-DS32                
                                                               
DECIMAL                                                        
                                                               
                                                               
                                                               
( Experimenting with drive parameters ) HEX                    
B/BUF SEC/BLK / CONSTANT SEC-LEN                               
0 IVAR RW-BUFFER B/BUF ALLOT                                   
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,                         
HERE 1 - SEC-LEN / , SEC-LEN , 7C0 ,                           
( We use the two l.s. bytes of 64 bit number)                  
              1 , 0 , 0 , 0 ,                                  
 CODE WRITE-SYSTEM                                             
  PUSHX, SI|                                                   
  MOVXI, AX| 4300 W,                                           
  MOVXI, DX| 0080 W,                                           
  MOVXI, SI| PARAM-BLOCK W,                                    
  INT, 13  B,                                                  
  POPX, SI|                                                    
  PUSHF,                                                       
  NEXT C;            DECIMAL                                   
( EXPERIMENT: SWITCH TO PROTECTED MODE AND BACK )              
  90 LOAD 41 42 THRU HEX     LOAD-GDT                          
CODE TO-PROT1                                                  
  CLI, PUSHS, DS|  TO-PROT,                                    
    JMPFAR, HERE 4 + MEM, CS-32 SEG,                           
    MOVI, W| R| AX| DS-32 , 0 ,                                
    MOVSW, T| DS| R| AX|                                       
    MOVI, W| R| AX| 61 , 0 ,                                   
    XCHGX, AX| XCHGX, AX| XCHGX, AX| XCHGX, AX|                
    MOVFA, B| B.0400 SWAP , ,                                  
    JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG,                
 TO-REAL, STI, POPS, DS|  OS, PUSHX, AX|                       
 NEXT C; DECIMAL                                               
                                                               
                                                               
                                                               
( SWITCH TO PROTECTED MODE AND BACK TIMING TEST )              
CODE TO-PROT2                                                  
  CLI, TO-PROT,                                                
    JMPFAR, HERE 4 + MEM, CS-32 SEG,                           
    JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG,                
 TO-REAL, STI,                                                 
 NEXT C; DECIMAL                                               
CODE TO-PROT3                                                  
  CLI, TO-PROT,   TO-REAL, STI,                                
 NEXT C; DECIMAL                                               
                                                               
: TEST2 0 DO TO-PROT2 LOOP ;                                   
: TEST3 0 DO TO-PROT3 LOOP ;                                   
: Q2 0 DO 10000 TEST2 LOOP ;                                   
: Q3 0 DO 10000 TEST3 LOOP ;                                   
                                                               
( SWITCH TO PROTECTED MODE AND BACK REPLACEMENT FOR DOCOL )    
  90 LOAD 41 42 THRU HEX     LOAD-GDT                          
CODE NEW-DOCOL                                                 
 (  JMPFAR, HERE 6 + MEM, 0 , CODE-SEGMENT SEG, )              
 (  TO-REAL,) STI,   CLI,   ( TO-PROT,)                        
(  JMPFAR, HERE 4 + MEM, CS-32 SEG, )                          
  LEA, BP'| DB| [BP] -2 B,                                     
  MOV, W| F| SI'| DB| [BP] 0 B,                                
  LEA, SI'| DB| [DI] 2 B,                                      
 NEXT C; DECIMAL                                               
 : A0 ; ' A0 >CFA @ CONSTANT 'DOCOL                            
CODE X JMP,  ' NEW-DOCOL >DFA 'DOCOL 3 + - , C;                
 CODE SWITCH  ' X >DFA 'DOCOL  CP, CP, CP, DROP DROP           
CLI,  ( TO-PROT, MOVXI, AX| DATA-SEGMENT MEM,                  
 MOVSW, T| DS| R| AX|  MOVSW, T| ES| R| AX|  MOVSW, T|         
SS| R| AX| ) NEXT C;  DECIMAL                                  
( SWITCH TO PROTECTED MODE AND BACK REPLACEMENT FOR DOCOL )    
CODE NEW-BIOS                                                  
  POPX, AX|   MOVFA, B| HERE 0 ,    ( PATCH THE INTERRUPT #)   
  POPX, DX|  POPX, CX|  POPX, BX|  POPX, DI|                   
PUSHX, SI|   PUSHX, BP| ( TO-REAL,) STI, XCHGX, DI|            
  INT, HERE SWAP ! 0 C, ( PATCH THE ADDRESS WHERE TO PATCH )   
  PUSHF, POPX, DI|   ( SAVE FLAGS BEFORE THEY ARE DESTROYED)   
  XCHGX, SI| ( FREE AX)  CLI,  ( TO-PROT,)                     
  ( NOW ALL REGISTERS ARE TIED UP EXCEPT ax| [!])              
POPX, BP|  POPX, AX|  XCHGX, SI|  ( RESTORE FORTH REGISTERS)   
PUSHX, AX|    PUSHX, BX|    PUSHX, CX|    PUSHX, DX|           
PUSHX, DI|    NEXT C;                                          
CODE HLT HLT, C;                                               
: PATCH-BIOS ' NEW-BIOS >DFA ' BIOS >CFA ! ;                   
: PATCH PATCH-BIOS SWITCH ;                                    
KRAAKER                                                        
 : A0 ;                                                        
 : A1 A0 A0 ;   : A2 A1 A1 ;    : A3 A2 A2 ;                   
 : A4 A3 A3 ;   : A5 A4 A4 ;    : A6 A5 A5 ;                   
 : A7 A6 A6 ;   : A8 A7 A7 ;    : A9 A8 A8 ;                   
 : AA A9 A9 ;                                                  
                                                               
: TEST 0 DO AA LOOP ;                                          
: Q 0 DO 10000 TEST LOOP ;                                     
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( testing of the block mechanism )                             
: FOR-BLOCKS >R PREV @                                         
    BEGIN DUP R@ EXECUTE +BUF WHILE REPEAT R> DROP DROP ;      
: SHOW-BLOCK                                                   
    DUP STALEST @ = IF CR ." STALEST:" THEN                    
    DUP CR H.                                                  
    DUP @ IF                                                   
        ." #" DUP ?                                            
        CELL+ DUP @ IF ." LOCKED" ELSE ." NOT LOCKED" THEN     
        CELL+ &: | EMIT 50 TYPE &: | EMIT                      
    ELSE                                                       
        ." FREE" DROP                                          
    THEN ;                                                     
: .B ' SHOW-BLOCK >CFA FOR-BLOCKS ;                            
    .B                                                         
                                                               
( A0dec21 www STRING AND PARSING words )                       
FORGET TASK      : TASK ;  CHAR " CONSTANT DLM   36 38 THRU    
: CREATE 0 IVAR 0 CELL+ NEGATE ALLOT ; ( Like ANSI)            
( All strings are copied to here, increase if needed)          
CREATE POOL 1000 ALLOT                                         
POOL IVAR PP ( LIKE DP)  : pd POOL PP @ OVER - DUMP ;          
: POOL-HERE PP @ ;                                             
: POOL-ALLOT PP +! ;                                           
: $SAVE DUP >R POOL-HERE $!   POOL-HERE R> 1+ POOL-ALLOT ;     
( WORD HAS SCANNED NOTHING )                                   
: EMPTY?  1 + C@  0= ;                                         
( AS WORD BUT WITH AUTOMATIC REFILL )                          
: WORD+ BEGIN DUP WORD DUP EMPTY?  WHILE DROP QUERY REPEAT     
 SWAP DROP ;    : PARSE WORD+ COUNT $SAVE ;                    
0 0 $SAVE CONSTANT NILL ( EMPTY STRING)                        
BL PARSE ##  CONSTANT ##   ( END-SENTINEL) 51 59 THRU          
( A0dec21 www SET words )                                      
: SET+! DUP >R @ ! 0 CELL+ R> +! ;                             
: SET CREATE HERE CELL+ , ## , 100 CELLS ALLOT DOES> ;         
( This construct presents ## for a set with 0 members)         
: SET-FOR-ALL POSTPONE DUP POSTPONE @ POSTPONE SWAP            
POSTPONE CELL+ POSTPONE (DO) HERE POSTPONE I POSTPONE @ ;      
IMMEDIATE                                                      
: END-S-F-A  POSTPONE 0 POSTPONE CELL+ POSTPONE                
(+LOOP) BACK ; IMMEDIATE                                       
: duse SET-FOR-ALL CR . END-S-F-A ;                            
: #IN-SET -1 ROT ROT SET-FOR-ALL                               
  OVER = IF SWAP DROP I SWAP THEN END-S-F-A DROP ;             
: ?IN-SET #IN-SET -1 <> ;                                      
: #IN-SET-$ -1 ROT ROT SET-FOR-ALL OVER                        
$@= IF SWAP DROP I SWAP THEN END-S-F-A DROP ;                  
: ?IN-SET-$ #IN-SET-$ -1 <> ;                                  
( A0apr12 www META-SET's van files sets and relations )        
SET META-HTML        ( ALL SETS OF HTML FILES )                
: REGISTER-HTML META-HTML SET+! ;                              
SET META-RELATION         ( ALL RELATIONS  )                   
: REGISTER-RELATION META-RELATION SET+! ;                      
SET FILES                                                      
: REGISTER-FILE DUP FILES ?IN-SET-$ 0=                         
IF FILES SET+! ELSE DROP THEN ;                                
: ?HTML-SET META-HTML ?IN-SET ;                                
( CREATE NAME-BUFFER 256 ALLOT  )                              
: PAR-TO-DATA CELL+ ; ( GO FROM N>P TO WHERE DOES> IS)         
: NAME-TO-SET ( SS -- 0/SET)                                   
 LATEST (FIND) IF                                              
   DROP PAR-TO-DATA DUP ?HTML-SET IF ELSE DROP 0 THEN          
ELSE 0 THEN ;                                                  
: du$ SET-FOR-ALL CR $@ TYPE END-S-F-A ;                       
( A0apr12 www COLLECT NAMES FOR A SET )                        
: COLLECT-REST ( OF LINE, MAY CONTAIN SPACES ## MEANS EMPTY)   
 DLM WORD ## $@= IF NILL ELSE HERE COUNT $SAVE THEN , ;        
: FILE/SET DUP NAME-TO-SET DUP IF CELL+ ( SKIP TEXT)           
 SET-FOR-ALL ( DUP COUNT TYPE KEY DROP ) , END-S-F-A DROP      
ELSE DROP DUP REGISTER-FILE , THEN ;                           
: COLLECT   BEGIN BL PARSE DUP ## $@= 0= WHILE  FILE/SET REPEAT
  DROP ;                                                       
( A set of html files with reference)                          
: SET-HTML CREATE HERE REGISTER-HTML COLLECT-REST              
HERE 0 , COLLECT HERE SWAP ! ( make it a set) DOES> ;          
: EXPECT-SET BL PARSE DUP NAME-TO-SET 0= 18 ?ERROR ;           
: RELATION-HTML CREATE HERE REGISTER-RELATION COLLECT-REST     
EXPECT-SET , BL PARSE ,  DOES> ;                               
                                                               
                                                               
( Randomize a set)                                             
: #SET CELL+ DUP @ SWAP CELL+ - 0 CELL+ / ;                    
89 LOAD 60 LOAD                                                
: CHOOSE-FROM-SET DUP #SET CHOOSE 2 + CELLS + ;                
: RANDOMIZE-SET DUP                                            
CELL+ DUP @ SWAP CELL+ DO                                      
DUP CHOOSE-FROM-SET I @SWAP                                    
0 CELL+ +LOOP DROP ;                                           
                                                               
( SET SET -- )                                                 
: CHECK# NAME-TO-SET #SET SWAP NAME-TO-SET #SET - 19 ?ERROR ;  
SET META-1-1      : REGISTER-1-1 META-1-1 SET+! ;              
: 1-1-HTML CREATE HERE REGISTER-1-1 COLLECT-REST               
EXPECT-SET DUP , EXPECT-SET DUP ,  CHECK# DOES> ;              
                                                               
                                                               
( output words for testing )                                   
: $? $@ $. ;                                                   
: WRITE TYPE ;                                                 
: PEEK DUP @ $? CELL+  ;                                       
: .set CR PEEK SET-FOR-ALL CR $? END-S-F-A ;                   
: .rel CR PEEK CR PEEK CR PEEK DROP ;                          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Define the texts used in html)                               
STRING A1 <A HREF="  STRING A2 >"    STRING A3 </A>"           
: CHANGE-IF-LAST   ( 'F S - 'F2 )                              
OVER OVER @ = IF SWAP DROP CELL+ ELSE DROP THEN ;              
: NEXT-IN-SET  ( F S -- a/-1 )                                 
OVER OVER #IN-SET CELL+ SWAP CHANGE-IF-LAST SWAP DROP @ ;      
: REF-WRITE ( SS SS -- )                                       
 SWAP CR A1 WRITE $@ WRITE  A2 WRITE $@ WRITE A3 WRITE ;       
: REF-SET ( SET FILE -- )                                      
 OVER CELL+ NEXT-IN-SET SWAP @ REF-WRITE ;                     
: DO-SETS META-HTML SET-FOR-ALL  ( F-F)                        
   OVER OVER CELL+ ?IN-SET-$ IF OVER REF-SET ELSE DROP THEN    
END-S-F-A ;                                                    
                                                               
                                                               
                                                               
( SHOW RELATION'S AND 1-1 RELATIONS )                          
: FILE/SET DUP NAME-TO-SET DUP IF CHOOSE-FROM-SET @            
SWAP DROP ELSE  DROP THEN ;                                    
: REF-REL ( REL -- )                                           
DUP CELL+ CELL+ @ FILE/SET SWAP @ REF-WRITE ;                  
: DO-RELATIONS META-RELATION SET-FOR-ALL   ( F -- F)           
  OVER OVER CELL+ @ NAME-TO-SET CELL+ ?IN-SET-$ IF             
      REF-REL ELSE DROP THEN END-S-F-A ;                       
                                                               
: REF-1-1 ( 1-1 FILE ) OVER CELL+ @ NAME-TO-SET - OVER         
CELL+ CELL+ @ NAME-TO-SET + @ SWAP @ REF-WRITE ;               
: DO-1-1 META-1-1 SET-FOR-ALL   ( F -- F)                      
   OVER OVER CELL+ @ NAME-TO-SET CELL+ #IN-SET-$ DUP -1 = IF   
      DROP DROP ELSE REF-1-1 THEN END-S-F-A ;                  
                                                               
                                                               
( Show all html references for all files )                     
: DO-FILE DO-SETS DO-RELATIONS DO-1-1 DROP ;   ( f-- )         
: DO-ALL FILES SET-FOR-ALL  CR DUP $? DO-FILE END-S-F-A ;      
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Examples of usage. )                                         
SET-HTML JONGENS VOOR EEN ANDERE FORTH FILE"                   
JAN   PIET  KLAAS   ##                                         
SET-HTML MEISJES NOG ZO'N DOM BLONDJE"                         
MARIEKE HELMA DORIENTJE ##                                     
SET-HTML OPIE Zomaar een ander kind"                           
JONGENS MEISJES ##  OPIE RANDOMIZE-SET  OPIE .set              
RELATION-HTML O-SEX OF HEB JE LIEVER EEN MEISJE"               
JONGENS MEISJES                                                
RELATION-HTML A-SEX OF HEB JE LIEVER EEN JONGEN"               
MEISJES JONGENS                                                
RELATION-HTML NO-SEX OF HEB JE LIEVER EEN JOPIE"               
MEISJES JOPIE                                                  
1-1-HTML VR OF WIL JE HAAR VRIENTJE" MEISJES JONGENS           
1-1-HTML RV OF WIL JE ZIJN VRIENDIN" JONGENS MEISJES           
NO-SEX .rel   MEISJES .set   FILES du$                         
( RAND EDN 1991JAN21, pg 151 ) HEX                             
                                                               
 0 IVAR SEED                                                   
                                                               
( . -- . ) ( Use the nanosecond counter to start)              
: RANDOMIZE DROP TIME SEED ! ;                                 
                                                               
( -- N  Leave a random number )                                
: RAND SEED @ 107465 * 234567 + DUP SEED ! ;                   
                                                               
( N -- R Leave a random number < N)                            
: CHOOSE RAND UM* SWAP DROP ;                                  
( Swap the number at R with a number 1..N cells away)          
: @SWAP  OVER @   OVER @   SWAP   ROT !   SWAP ! ;             
( RANDOM-SWAP ( R N -- )                                       
( 1 - CHOOSE 1+ CELLS OVER + @SWAP ;)  DECIMAL                 
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( WRITE THE CURRENT SYSTEM TO HARD DISK ) HEX                  
B/BUF SEC/BLK / CONSTANT SEC-LEN                               
 : WRITE-SYSTEM HERE 1+ 0 DO                                   
  I   I SEC/LEN /   1 R/W                                      
  SEC-LEN +LOOP ;                                              
                                                               
DECIMAL  (  For 32 bits, but not yet schecked)                 
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
." CRC CHECK FOR FIG  85JAN06 ALBERT VAN DER HORST"            
( Adapted from FORTH DIMENSIONS IV-3 ) HEX                     
 : ACCUMULATE ( oldcrc/char -- newcrc )                        
   0100 * XOR                                                  
   8 0 DO                                                      
      DUP 0< IF 4002 XOR DUP + 1+ ELSE DUP + THEN              
   LOOP ;                                                      
 : DISPOSE ( crcvalue/adres/len -- newcrcvalue)                
    OVER DUP C@ "( = SWAP 1+ C@ BL = AND OVER 1 = AND IF       
       ( comment; skip it) DROP DROP ") WORD DROP              
    ELSE                                                       
       1+ OVER + SWAP DO I C@ ACCUMULATE LOOP                  
    THEN ;                                                     
 : MORE ( -- adr f  Leaves flag if there is more in the block) 
    BL WORD DUP C@ 2 < OVER 1+ C@ "! < AND 0=                  
 ;                                                             
 ." CRC 2 "                                                    
 : VERIFY-BLOCK ( oldcrc/blnr -- newcrc)                       
   BLK @ >R IN @ >R   BLK !  0 IN !                            
   BEGIN MORE WHILE                                            
       BL OVER COUNT + C! COUNT DISPOSE                        
   REPEAT DROP ( drop the address left by MORE)                
   R> IN ! R> BLK ! ;                                          
 : VERIFY ( scrnr/crc)                                         
   0 SWAP B/SCR * DUP B/SCR + SWAP DO                          
      I VERIFY-BLOCK                                           
   LOOP                                                        
 ;                                                             
 : VER   SCR @ VERIFY U. ;                                     
                                                               
                                                               
                                                               
( Test screen)                                                 
     For program exchange, the medium of hard copy is cheap,   
convenient, and machine-independent. Its primary disadvantages 
are the time required for hand-typing the source code and the  
possibility of human error in the process. Even if the screens 
LOAD without error messages, some errors may pass undetected   
until run-time, when the system crashes mysteriously.          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >1<"                   
HEX VOCABULARY ASSEMBLER IMMEDIATE  ASSEMBLER DEFINITIONS      
 : 8* DUP + DUP + DUP + ; ( ;CODE see screen3 )                
: CODE ?EXEC CREATE [COMPILE] ASSEMBLER !CSP ; IMMEDIATE       
: C; CURRENT @ SEARCH ! ?EXEC ?CSP ; IMMEDIATE                 
: LABEL ?EXEC 0 IVAR SMUDGE -2 ALLOT [COMPILE] ASSEMBLER       
    !CSP ; IMMEDIATE     ASSEMBLER DEFINITIONS                 
4 CONSTANT H    5 CONSTANT L     7 CONSTANT A    6 CONSTANT PSW
2 CONSTANT D    3 CONSTANT E     0 CONSTANT B    1 CONSTANT C  
6 CONSTANT M 6 CONSTANT SP ' EXIT >CFA 0B + @ CONSTANT (NEXT)  
: 1MI CREATE C, DOES> C@ C, ;  : 2MI CREATE C, DOES> C@ + C, ; 
 : 3MI CREATE C, DOES> C@ SWAP 8* +  C, ;                      
: 4MI CREATE C, DOES> C@ C, C, ;                               
: 5MI CREATE C, DOES> C@ C, , ;  : PSH1 C3 C, (NEXT) 1 - , ;   
: PSH2 C3 C, (NEXT) 2 - , ;       : NEXT C3 C, (NEXT) , ;      
75 76 THRU                                                     
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >2<"                   
00 1MI NOP     76 1MI HLT     F3 1MI DI     FB 1MI EI          
07 1MI RLC     0F 1MI RRC     17 1MI RAL    1F 1MI RAR         
E9 1MI PCHL    F9 1MI SPHL    E3 1MI XTHL   EB 1MI XCHG        
27 1MI DAA     2F 1MI CMA     37 1MI STC    3F 1MI CMC         
80 2MI ADD     88 2MI ADC     90 2MI SUB    98 2MI SBB         
A0 2MI ANA     A8 2MI XRA     B0 2MI ORA    B8 2MI CMP         
09 3MI DAD     C1 3MI POP     C5 3MI PUSH   02 3MI STAX        
0A 3MI LDAX    04 3MI INR     05 3MI DCR    03 3MI INX         
0B 3MI DCX     C7 3MI RST     D3 4MI OUT    DB 4MI IN          
C6 4MI ADI     CE 4MI ACI     D6 4MI SUI    DE 4MI SBI         
E6 4MI ANI     EE 4MI XRI     F6 4MI ORI    FE 4MI CPI         
22 5MI SHLD    2A 5MI LHLD    32 5MI STA    3A 5MI LDA         
CD 5MI CALL    C3 5MI JMP                                      
               ( CZ,CNZ,CCY,CNC)                               
                                                               
CR ." CASSADY'S 8080 ASSEMBLER 81AUG17  >3<"                   
C9 1MI RET                   C2 CONSTANT 0=  D2 CONSTANT CS    
E2 CONSTANT PE  F2 CONSTANT 0<   : NOT 8 + ;                   
: MOV 8* 40 + + C, ;   : MVI 8* 6 + C, C, ;  : LXI 8* 1+ C, , ;
: THEN HERE SWAP ! ;               : IF C, HERE 0 , ;          
: ELSE C3 IF SWAP THEN ;           : BEGIN HERE ;              
: UNTIL C, , ;                     : WHILE IF ;                
: REPEAT SWAP C3 C, , THEN ;                                   
EXIT                                                           
                                                               
: ;CODE                                                        
?CSP   POSTPONE   (;CODE)   [COMPILE] [   [COMPILE] ASSEMBLER  
; IMMEDIATE                                                    
                                                               
                                                               
                                                               
CR ." SIMPLE PROFILER AH   85FEB15" HEX                        
LABEL NEXT2      ( REPLACES NEXT!)                             
   B LDAX   B INX   A L MOV                                    
   B LDAX   B INX   A H MOV   (NEXT) 6 + JMP C;                
   (NEXT) 3 + JMP C;                                           
CODE PROFILE  ( PATCHES THE CODE AT NEXT FOR PROFILING)        
   C3 A MVI  (NEXT) STA                                        
   NEXT2 H LXI    (NEXT) 1+ SHLD     NEXT C;                   
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
 ( STREAM READ ROUTINES CP/M 85/012/08  AH )                   
 : F_READ ( B,N-N2 Tries to read N char's to buffer B)         
          ( N2 is number actually read, 0 for EOF)             
      ( NOT  YET: NOW IT IS FILLED WITH ^Z, NOTHING RETURNED ) 
  BEGIN                                                        
     SWAP GET-CHAR                                             
     OVER C! 1+ SWAP 1 -                                       
     DUP 0=                                                    
  UNTIL                                                        
 ;                                                             
 : F_WRITE ( B,N-N2 Tries to write N char's from buffer B)     
      ( N2 is the number actually written to disk )            
      ( NOT  YET: NOW IT IS UNCLEAR, NOTHING RETURNED )        
   TO-DISK                                                     
 ;                                                             
                                                               
 ( DISC IO SCREEN 15 GENERAL  >1<   85/12/08 AH )              
 <HEX                                                          
 0A0D IVAR CRLF    1A IVAR CTRLZ                               
 : CLEAN-FCB DUP 21 0 FILL  1+ 0B 20 FILL ;                    
                                                               
 : FILL-FCB 22 WORD                                            
    1+ HERE  COUNT ROT SWAP CMOVE  ;                           
                                                               
 : SET-DMA  1A BDOS DROP ;                                     
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
 ( DISC IO SCREEN 16 WRITE    >1<   85/12/08 AH )              
 0 IVAR DISK-BUFFER-W 100 ALLOT                                
 DISK-BUFFER-W IVAR POINTER-W                                  
0 IVAR FCB2   21 ALLOT  ( BUG: 2nd goes wrong)                 
 : ?PRES ( .-. Aborts whether the entry is already present )   
    FCB2 0F BDOS 0FF - IF ." ALREADY PRESENT" QUIT THEN        
    FCB2 10 BDOS DROP ;                                        
 : .OPENW FCB2 CLEAN-FCB FCB2 FILL-FCB ?PRES                   
   FCB2 16 BDOS 0FF = IF ." DISK FULL " QUIT THEN              
   DISK-BUFFER-W POINTER-W ! ;                                 
 : .CLOSEW                                                     
      DISK-BUFFER-W SET-DMA FCB2 15 BDOS . ." LAST RECORD" CR  
            FCB2 10 BDOS . ." CLOSE STATUS" CR ;               
 : MOVE-DOWN   -80 POINTER-W +!                                
               DISK-BUFFER-W 80 OVER + SWAP 80 CMOVE ;         
                                                               
 ( DISC IO SCREEN 17 WRITE    >2<   85/12/08 AH )3<)           
 : TO-DISK DUP >R POINTER-W @ SWAP CMOVE                       
           R> POINTER-W +!                                     
           POINTER-W @ DISK-BUFFER-W -                         
           80 >  IF   -->                                      
              DISK-BUFFER-W SET-DMA FCB2 15 BDOS .             
              MOVE-DOWN                                        
          THEN ;                                               
                                                               
 : .WRITE  ( 2/0 WRITE SCREEN-1 .. SCREEN-2 TO DISK)           
      1+ B/SCR * SWAP B/SCR * ( GET START BUFFER #'S)          
        DO I BLOCK DUP                                         
        40 -TRAILING TO-DISK  CRLF 2 TO-DISK                   
        40 + 40 -TRAILING TO-DISK CRLF 2 TO-DISK               
      LOOP CTRLZ 1 TO-DISK                                     
 ;   HEX>                                                      
 ( DISC IO SCREEN 18 READ     >1<   85/12/08 AH )              
 <HEX  ( BUG: 64 char lines go wrong)                          
 0 IVAR DISK-BUFFER-R  80 ALLOT 0 IVAR POINTER-R               
  0A CONSTANT "LF"  0D CONSTANT "CR"                           
  1A CONSTANT ^Z    DISK-BUFFER-R 80 + CONSTANT END-BUF        
  0 IVAR EOF                                                   
 : .OPENR   FCB2 DUP CLEAN-FCB FILL-FCB                        
       END-BUF POINTER-R !                                     
       FCB2 0F BDOS 0FF = IF ." NOT PRESENT" QUIT THEN         
       0 EOF ! ;                                               
 : .CLOSER   FCB2 10 BDOS . ." CLOSE STATUS" CR ;              
                                                               
                                                               
                                                               
                                                               
                                                               
 ( DISC IO SCREEN 19 READ     >2<   85/12/08 AH )              
 : ?EMPTY ( POINTER -- CORRECTED PNR, READ SECTOR IF AT END)   
     DUP END-BUF = IF DISK-BUFFER-R SET-DMA  FCB2 14 BDOS .    
                    DROP DISK-BUFFER-R THEN  ;                 
 : GET-CHAR                                                    
    POINTER-R @                                                
      ?EMPTY                   ( GET NEW BUFFER IF NEEDED)     
      DUP C@ "LF" = IF 1+ ?EMPTY THEN ( SKIP OVER LF)          
      DUP C@ SWAP              ( GET CHAR, POINTER ON TOP)     
      OVER ^Z =                                                
      IF 1 EOF ! ELSE 1+ THEN ( INCREMENT POINTER UNLESS AT ^Z)
    POINTER-R !  ;                                             
                                                               
                                                               
                                                               
                                                               
 ( DISC IO SCREEN 20 READ     >3<   85/12/08 AH )              
 : GET-LINE ( ADR -- . reads a line to ADR )                   
      DUP 40 20 FILL ( preset spaces )                         
      41 OVER + SWAP ( max $41 char to a line, CR!)            
      DO  GET-CHAR                                             
          DUP "CR" = IF DROP 20 LEAVE THEN                     
          DUP ^Z   = IF DROP 20 LEAVE THEN                     
          I C! ( may leave spurious 81th space)                
      LOOP  ;                                                  
 : .READ ( 2/0 READ SCREEN-2 TO SCREEN -1)                     
      1+ B/SCR * SWAP B/SCR * ( get start buffer #'s)          
      DO  I BLOCK DUP GET-LINE                                 
          DUP 40 + GET-LINE  81 + 0 SWAP C! UPDATE             
          I #BUFF MOD 0= IF ( full load of buffers) FLUSH THEN 
      LOOP                                                     
; HEX>                                                         
 ( DISC IO SCREEN 21  LOAD    >1<   85/12/08 AH )              
 ( EXAMPLE: .OPENR TEMP" 25 26 .LOAD .CLOSER )                 
 <HEX  0 IVAR LBUF 3E ALLOT 0 C,                               
 : I-F-A ( ADRES -- . ,INTERPRET FROM ADDRESS )                
     TIB @ >R  IN @ >R  ( SAVE CURRENT INTERPRET POSITION)     
     TIB !     0 IN !   ( NEW POSITION)                        
     0 INTERPRET                                               
     >R IN !   >R TIB ! ( RESTORE)  ;                          
                                                               
 : .LOAD ( LOAD THE CPM FILE SPECIFIED IN FCB2 )               
         BEGIN   LBUF DUP GET-LINE I-F-A                       
         EOF @ UNTIL ;                                         
                                                               
                                                               
    HEX>                                                       
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Using the internal timer for testing A0JUN28 AH) ?32 HEX     
 CODE TIME 0F C, 31 C, ( 90 C, 90 C, 90 C, 90 C, )             
 50 C, 52 C,  NEXT C;                                          
                                                               
 DECIMAL                                                       
 ." What is the speed of your Pentium (in Mhz)?"               
 PAD DUP 80 ACCEPT EVALUATE CONSTANT SPEED                     
 : MARK-TIME TIME ;                                            
 : .mS SPACE 0 <# # # # [CHAR] . HOLD #S #> TYPE ." mS "  ;    
 : ELAPSED DNEGATE TIME D+ SPEED SM/REM SWAP DROP ;  DECIMAL   
 ( EXIT REMOVE THIS LINE IF YOU WANT A TEST )                  
 : TASK ; 26 LOAD                                              
 : MEASURE TIME DO-PRIME ELAPSED ;                             
  MEASURE                                                      
CR  ." THE BYTE BENCHMARK LASTED " .mS                         
                                                               
 CR ." TAARTEN AUTOMATISERING DOOR DRS HENK" CR                
 ." EEN VOORBEELD UIT BRODIE"     CR                           
 ." TYPE HELP VOOR DE GLOSSARY"  CR                            
 0 IVAR TAARTEN     0 IVAR DIEP-VRIES                          
 : HELP CR ." GLOSSARY:" CR ." BAK-TAART"                      
        CR ." EET-TAART" CR ." VRIES-IN" CR ." ONTDOOI"        
        CR ." START" CR ." STATUS" CR ;                        
 : START 0 TAARTEN ! 0 DIEP-VRIES ! ;                          
 : BAK-TAART 1 TAARTEN +! ;                                    
 : EET-TAART TAARTEN @ DUP                                     
       IF -1 TAARTEN +! CR ." DANKJEWEL !" CR ELSE             
         CR ." WELKE TAART ?" CR DROP THEN ;                   
 : VRIES-IN TAARTEN @ DIEP-VRIES +! 0 TAARTEN ! ;              
 : ONTDOOI DIEP-VRIES @ TAARTEN +! 0 DIEP-VRIES ! ;            
 : STATUS CR ." AANTAL AANWEZIGE TAARTEN: " TAARTEN ?          
   CR ." EN NOG " DIEP-VRIES ? ." IN DE DIEP VRIES " ;         
." HET IS GOED MIS"                                            
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Only valid for 16 bits real mode  A0JUL04 AvdH )             
 ( C7) 6 1FI MEM| ( Overrules other modes!)                    
( 07) 1 0 8 1FAMILY| [BX+SI] [BX+DI] [BP+SI] [BP+DI]           
[SI] [DI] [BP] [BX]                                            
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( 8086 assembler fix ups a0jul05  AvdH HCCFIG HOLLAND)         
 8 0 4 1FAMILY| ES| CS| SS| DS|    1 6 2 1FAMILY, PUSHS, POPS, 
 8 26 4 1FAMILY, ES:, CS:, SS:, DS:,                           
 8 27 4 1FAMILY, DAA, DAS, AAA, AAS,                           
 1 0 2 1FAMILY| B1| W1|    08 04 8 1FAMILY, ADDAI, ORAI, ADCAI,
  SBBAI, ANDAI, SUBAI, XORAI, CMPAI,                           
 2 A0 2 1FAMILY, MOVTA, MOVFA,                                 
1 0 2 1FAMILY| Y| N|   2 0 8 1FAMILY| O| C| Z| CZ| S| P| L| LE|
 70 1PI J,  ( As in J, L| Y| <CALC> S, )                       
 1 0 8 1FAMILY| AX| CX| DX| BX| SP| BP| SI| DI|                
 08 40 4 1FAMILY, INCX, DECX, PUSHX, POPX,    90 1PI XCHGX,    
 ( C0) 40 00 4 1FAMILY| D0| DB| DW| R|                         
 ( 38) 08 00 8 1FAMILY| AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
 1 0 8 1FAMILY| AL| CL| DL| BL| AH| CH| DH| BH|                
                                                               
                                                               
( 8086 ASSEMBLER OPCODES PART 1, A0jul05 AvdH HCCFIG HOLLAND)  
1 0 2 2FAMILY| B| W|   2 0 2 2FAMILY| F| T|                    
8 0 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,          
2 84 2 2FAMILY, TEST, XCHG,   0 88 2PI MOV,                    
( 00FD) 0 8C 2PI MOVSW,   ( 00FE) 0 8D 2PI LEA,                
( IRR,egular) ( FF) 9A 1PI CALLFAR,  ( FE) A8 1PI TESTAI, ( FF)
 1 98 8 1FAMILY, CBW, CWD, IR2, WAIT, PUSHF, POPF, SAHF, LAHF, 
( FE) 2 A4 6 1FAMILY, MOVS, CMPS, IR3, STOS, LODS, SCAS,       
08 B0 2 1FAMILY, MOVRI, MOVXI,                                 
8 C2 2 1FAMILY, RET+, RETFAR+,  8 C3 2 1FAMILY, RET,  RETFAR,  
1 C4 2 1FAMILY, LES, LDS,  0 C6 2PI MOVI,   CD 1PI INT,        
1 CC 4 1FAMILY, INT3, IRR, INTO, IRET,                         
1 D4 4 1FAMILY, AAM, AAD, IL1, XLAT,                           
1 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,                     
2 E4 2 1FAMILY, INAP, OUTAP,  2 EC 2 1FAMILY, INAD, OUTAD,     
1 E8 2 1FAMILY, CALL, JMP,  EA 1PI JMPFAR,  EB 1PI JMPS,       
( 8086 ASSEMBLER OPCODES PART 2, A0jul05 AvdH HCCFIG HOLLAND)  
1 F0 6 1FAMILY, LOCK, ILL, REP, REPZ, HLT, CMC,                
1 F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD, ( 38FE)          
800 80 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI,     
 CMPI, 800 83 8 2FAMILY, ADDSI, IL3, ADCSI, SBBSI, IL4, SUBSI, 
 IL5, CMPSI,   2 0 2 2FAMILY| 1| V|                            
800 D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, IL6, RAR,      
800 10F6 6 2FAMILY, NOT, NEG, MUL, IMUL, DIV, IDIV,            
00 F6 2PI TESTI, 800 FE 2 2FAMILY, INC, DEC,                   
( 38FF) 00 8F 2PI POP,  30 FE 2PI PUSH,                        
800 10FF 4 2FAMILY, CALLO, CALLFARO, JMPO, JMPFARO,            
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Basic block manipulapions ) HEX ( ASSUMES BLOCK 90)          
ALIGN 0 IVAR RW-BUFFER B/BUF ALLOT                             
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,                         
2 , ( 2 sectors/block) RW-BUFFER , 0 ,                         
HERE 2 ALLOT  0 , 0 , 0 , CONSTANT BL#                         
 : R/W-BLOCK  ASSEMBLER                                        
  POPX, AX|            ADD, W| R| AX'| AX|                     
  MOVFA, W1| BL# W,    PUSHX, SI|                              
  MOVXI, BX| ( FUNCTION CODE ) W,   MOVXI, DX| 0080 W,         
  MOVXI, SI| PARAM-BLOCK SWITCH_DS 10 * -  W,                  
  TO-REAL, SWITCH_DS COPY-SEG                                  
 XCHGX, BX| INT, 13 B, PUSHF, POPX, BX|                        
 TO-PROT, GDT_DS COPY-SEG                                      
  POPX, SI|   PUSHX, BX|  NEXT ;                               
CODE READ-BLOCK 4200 R/W-BLOCK  C;                             
CODE WRITE-BLOCK 4300 R/W-BLOCK  C;     DECIMAL                
?PC HEX ( copy a hd system, wa written to a floppy to the      
hard disk. Done by a Forth booted from another floppy.)        
40 CONSTANT HD-OFFSET                                          
: SAFE HD-OFFSET OFFSET @ = 17 ?ERROR ;                        
: COPY-FLOPPY SAFE EMPTY-BUFFERS  ." Put hd floppy and key"    
 KEY DROP 0 0 0 0 13 BIOS  80 0 0 0 13 BIOS                    
  HD-OFFSET 0 DO                                               
    RW-BUFFER I 1 R/W   I WRITE-BLOCK 1 AND .                  
  LOOP ;                                                       
: RESTORE-BLOCKS SAFE 100 0 DO                                 
   RW-BUFFER I OFFSET @ + 1 R/W                                
   I HD-OFFSET + WRITE-BLOCK 1 AND .                           
LOOP ;                                                         
: SAVE-BLOCKS SAFE 100 0 DO                                    
   I HD-OFFSET + READ-BLOCK 1 AND .                            
   RW-BUFFER I OFFSET @ + 0 R/W LOOP ;  DECIMAL                
( Experimenting with drive parameters ) HEX                    
B/BUF SEC/BLK / CONSTANT SEC-LEN                               
0 IVAR RW-BUFFER B/BUF ALLOT                                   
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,                         
HERE 1 - SEC-LEN / , SEC-LEN , 7C0 ,                           
( We use the two l.s. bytes of 64 bit number)                  
              1 , 0 , 0 , 0 ,                                  
 CODE WRITE-SYSTEM                                             
  PUSHX, SI|                                                   
  MOVXI, AX| 4300 W,                                           
  MOVXI, DX| 0080 W,                                           
  MOVXI, SI| PARAM-BLOCK W,                                    
  INT, 13  B,                                                  
  POPX, SI|                                                    
  PUSHF,                                                       
  NEXT C;            DECIMAL                                   
( BASIC STYLE MINI EDITOR - FORTH DIM iii/2 @ SHAPIN)          
HEX : TEXT HERE C/L 1+ BLANK WORD PAD C/L 1+ CMOVE ;           
: LINE DUP FFF0 AND 17 ?ERROR SCR @ (LINE) DROP ;              
: -MOVE LINE C/L CMOVE UPDATE ;                                
: P 1 TEXT PAD 1+ SWAP -MOVE ; DECIMAL                         
( Usage : to change line 1 of screen 3 )                       
( 3 SCR ! 1 P <CONTENT> )                                      
: CLEAN BLOCK B/BUF OVER + SWAP DO                             
  I C@ 0= IF BL I C! THEN  LOOP ;                              
(   : THRU 1+ SWAP DO ." LOADING " I . I LOAD LOOP ;  )        
: L-S SCR @ LIST ; : LO-S SCR @ LOAD ;                         
: C-S SWAP BLOCK SWAP BLOCK B/BUF CMOVE UPDATE FLUSH ;         
: LIST' BASE @ 10 - 25 ?ERROR LIST ;  : LIST LIST' ;           
?PC : BIOSI BIOS DROP DROP DROP DROP DROP ;  ( Ignore result)  
: MODE 0 0 0 16 BIOSI ;                                        
: DISK-INIT 0 0 0 0 19 BIOSI ;                                 
( MINI EDITOR FOR MSDOS ) HEX                                  
B800 CONSTANT VID   050 CONSTANT VW   19 CONSTANT VH           
VH VW * CONSTANT VL                                            
: A-L SCR @ (LINE) ;                                           
: VA ( I - VS,VO ) DUP + VID SWAP ;                            
: >V SWAP 0 DO  ( $, OFFSET - )                                
        OVER I + C@ 700 OR   OVER I + VA LC!                   
     LOOP DROP DROP ;                                          
: V> ( BUF -LEN OFFSET- )                                      
SWAP 0 DO OVER I +   OVER I + VA                               
 LC@ SWAP C!                                                   
LOOP DROP DROP ;                                               
: PAGE PAD VL 2DUP BLANK 0 >V ;                                
: PG PAD 10 VW * 2DUP BLANK 0 >V ;                             
 DECIMAL                                                       
                                                               
HEX                                                            
: GET-S 10 0 DO I A-L I VW * >V LOOP ;                         
: PUT-S 10 0 DO I A-L I VW * V> LOOP UPDATE ;                  
: GET-L PAD VW ROT VW * >V ;                                   
: PUT-L PAD VW ROT VW * V> ;                                   
( 19 CONSTANT PB )   13 CONSTANT HW                            
: PUSH-D 10 SWAP DO I 1+ PUT-L I GET-L LOOP ;                  
: PUSH-DOWN VH SWAP DO I 1+ PUT-L I GET-L LOOP ;               
: DEL-L DUP PUT-L VH  GET-L PUSH-DOWN ;                        
: PUSH-UP VH DO I PUT-L I 1+ GET-L -1 +LOOP ;                  
: UDL-L DUP PUSH-UP VH PUT-L GET-L ;                           
: PUSH-U HW DO I PUT-L I 1+ GET-L -1 +LOOP ;                   
: UFL-L DUP PUSH-U VH 1 - PUT-L GET-L ;                        
: DUP-L DUP DEL-L DUP UFL-L UDL-L ;                            
                                                               
 DECIMAL                                                       
HEX ( CURSOR E.D.)                                             
0 IVAR CURSOR                                                  
 : CURL CURSOR @ VW / ; : CP CURSOR @ VW MOD  ;                
 : BIOS-CURSOR CURSOR @ VW /MOD 100 * + ;                      
: SET 200 0 0 BIOS-CURSOR 10 BIOS DROP DROP DROP DROP DROP ;   
: MOVE-CURSOR   ( WORD STAR)                                   
DUP ^D = IF  1 ELSE   DUP ^E = IF 0 VW - ELSE                  
DUP ^I = IF  8 ELSE   DUP ^M = IF VW CP - ELSE                 
DUP ^S = IF -1 ELSE   DUP ^X = IF VW ELSE                      
0    THEN THEN THEN THEN THEN THEN CURSOR +! ;                 
: DELSTORING DUP ^Y  = IF CURL DEL-L ELSE                      
      DUP ^P = IF CURL UDL-L ELSE                              
      DUP ^U = IF CURL UFL-L                                   
      THEN THEN THEN ;                                         
: DUP-L CURL DUP-L ;                                           
  DECIMAL                                                      
HEX                                                            
: PAD-B PAD VW BLANK  ;                                        
: GET-R PAD VW CP - CURL VW * CP + >V ;                        
: GET-P PAD    CP   CURL VW *      >V ;                        
: PUT-R PAD VW CP - CURL VW * CP + V> ;                        
: RUBOUT-M PUT-R NEGATE CURSOR +! GET-R ;                      
: INSERT-M PUT-R DUP CURSOR +! GET-R NEGATE CURSOR +! ;        
: RUBOUT 1 RUBOUT-M ; : INSERT 1 INSERT-M ;                    
: RUB-C PAD-B RUBOUT SET ;                                     
: DEL-C 1 CURSOR +! RUB-C ;                                    
: INS-C INSERT ;                                               
: EOL PAD-B GET-R ;  : FOL PAD-B GET-P ;                       
: SPL DUP-L PAD-B PUT-R CURL 1+ GET-L EOL ;                    
: JOL PAD-B CURL 1+ PUT-L GET-R CURL 1+ PUSH-D ;               
DECIMAL                                                        
                                                               
HEX                                                            
 0 IVAR I-MODE                                                 
: INSELETING                                                   
      DUP ^H = IF RUB-C ELSE                                   
      DUP ^G = IF DEL-C ELSE                                   
      DUP ^V = IF I-MODE 1 TOGGLE ELSE                         
      THEN THEN THEN ;                                         
: JOINITTING                                                   
      DUP ^J = IF JOL   ELSE                                   
      DUP ^O = IF SPL   ELSE THEN THEN ;                       
: EM-C EMIT 1 CURSOR +! ;                                      
: PRINT ( C --C . Print it if printable)                       
  DUP 1F > IF DUP 7F < IF                                      
  I-MODE @   IF INS-C THEN  DUP EM-C                           
  THEN THEN ;                                                  
DECIMAL                                                        
 ( Finding the next word A0MAY25 )                             
 : BL? VA LC@ $FF AND BL = ;                                   
: BOUNDARY? BL? 0= OVER 1 - BL? AND ;                          
: >SEARCH BEGIN 1 + DUP BOUNDARY? UNTIL  ;                     
: <SEARCH BEGIN 1 - DUP BOUNDARY? UNTIL  ;                     
: NEXT-W CURSOR @ >SEARCH CURSOR ! ;                           
: BACK-W CURSOR @ <SEARCH CURSOR ! ;                           
: DEL-W  CURSOR @ NEXT-W SPL                                   
   CURSOR ! SET SPL CURL 1+ DEL-L JOL ;                        
: VTYPE CURSOR @ >V ;                                          
: GET-W VH 1 - PUT-L PAD VW -TRAILING 1+ ;                     
: UFW GET-W INSERT-M DROP GET-W VTYPE ;                        
: WORDING                                                      
  DUP ^F = IF NEXT-W ELSE   DUP ^A = IF BACK-W ELSE            
  DUP ^T = IF DEL-W ELSE    DUP ^Z = IF UFW ELSE               
THEN THEN THEN THEN ;                                          
( DISPATCHER )    HEX                                          
: AT-END VH 1 - VW * CURSOR ! SET ;                            
: DEBUG CURSOR @ AT-END ^ CURSOR ! ;                           
: EXITING KEY 51 - IF PUT-S THEN ;                             
: ROUTE BEGIN KEY                                              
PRINT DELSTORING                                               
INSELETING JOINITTING                                          
WORDING MOVE-CURSOR SET                                        
( DEBUG)                                                       
ESC = UNTIL ;                                                  
: E-S  ( EDIT CURRENT SCREEN )                                 
1 I-MODE ! FRAME 0 CURSOR ! SET   PG                           
GET-S ROUTE EXITING  AT-END BLACK ;                            
:  EDIT SCR ! E-S ;                                            
: E-R 3 MODE EDIT ;                                            
DECIMAL  ( Attempts at comamnd line editor)                    
: FROM-BIOS 100 /MOD VW * + CURSOR ! ;                         
: GET 300 0 0 0 10 BIOS DROP FROM-BIOS  DROP DROP DROP ;       
: P ( AT-END) GET ROUTE CURL PUT-L                             
  ( PAD SCRATCH BLOCK LL CMOVE )                               
   AT-END CR PAD 50 1 - -TRAILING TYPE SPACE                   
   PAD TIB @ 50 CMOVE 0 IN !                                   
; ( IMMEDIATE  DOESN'T WORK VIA S)                             
DECIMAL                                                        
                                                               
( TROEP SCEHRM EDITOR)                                         
: EKEY 1000 0 0 0 16 BIOS DROP DROP DROP DROP ;                
                                                               
                                                               
                                                               
                                                               
                                                               
( editor elective screen)                                      
?PC                                                            
   101 107 THRU                                                
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( ASSSEMBLER 32 BIT ELECTIVES A0JUL03 AH)                      
 111 112 HEX THRU DECIMAL  ( Load either 16 or 32 bit stuff)   
 93 96  HEX THRU  DECIMAL  ( 8086 level instructions )         
 113 119 HEX THRU DECIMAL  ( 80386 level instructions )        
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( 16 BITS PROTECTED MODE A0jul04  AvdH HCC HOLLAND)  ?16       
( C7) 6 1FI MEM|  ( OVERRULES D0| BP| )                        
( 07) 1 0 8 1FAMILY| [BX+SI] [BX+DI] [BP+SI] [BP+DI]           
[SI] [DI] [BP] [BX]                                            
                                                               
( Use remainder only with AS: prefix )                         
( For explanation see next screen, but unprimed.)              
 ( 07) 1 0 8 1FAMILY| [AX]' [CX]' [DX]' [BX]'                  
[SIB]' [BP]' [SI]' [DI]'                                       
( FF) C4 1FI MEM|'                                             
( 0) C0 1PI MEM,'                                              
( 3F) 04 1FI SIB|'                                             
( 0) 0 1PI SIB,'                                               
( F8) 04 1FI +0|'                                              
( C0) 40 0 4 1FAMILY| +1*| +2*| +4*| +8*|                      
                                                               
( 32 BITS PROTECTED MODE A0jul04  AvdH HCC HOLLAND)  ?32       
( FF) C4 1FI MEM| ( MEM| MEM, OVERRULES D0| SIB| SIB, BP| )    
( 07) 05 1PI MEM, ( REQUIRED AFTER MEM|)                       
( COMBINES WITH D0| DB| DX| )                                  
 ( 07) 1 0 8 1FAMILY| [AX] [CX] [DX] [BX] [SIB] [BP] [SI] [DI] 
                                                               
( 0) 0 1PI SIB, ( REQUIRED AFTER SIB|)                         
( F8) 04 1FI +0| ( OVERRULES +1*| SP'| )                       
( C0) 40 0 4 1FAMILY| +1*| +2*| +4*| +8*|                      
( Example : MOVI, X| D0| SIB|   SIB, AX| +1*| SI'|   117 X, )  
( Example : MOVI, X| MEM|   MEM, +0| VAR X, 117 X, )           
                                                               
( Use remainder only with AS: prefix . See previous screen )   
( C7) 6 1FI MEM|'                                              
( 07) 1 0 8 1FAMILY| [BX+SI]' [BX+DI]' [BP+SI]' [BP+DI]'       
[SI]' [DI]' [BP]' [BX]'                                        
( OPERAND AND ADDRESS SIZE OVERWRITE a0jul03 AvdH HCC HOLLAND) 
1 60 2 1FAMILY, PUSHA, POPA,                                   
1 62 2 2FAMILY, BOUND, ARPL,                                   
1 64 4 1FAMILY, FS:, GS:, OS:, AS:,                            
( OPERAND AND ADDRESS SIZE OVERWRITE)                          
66 1PI OS,   67 1PI AS,  ( Keep a short while)                 
2 68 2 2FAMILY, PUSHI|X, PUSHI|B,                              
2 69 2 2FAMILY, IMULI|X, IMULI|B,                              
2 6C 2 1FAMILY, INS, OUTS,   C8 1PI ENTER,   C9 1PI LEAVE,     
1 F02 2 2FAMILY, LAR, LSL,                                     
06 0F 2PI CLTS,    C0 20 0F 3PI MOV|CD,                        
1 0 4 2FAMILY| CR0| ILL| CR1| CR2|                             
1 0 8 2FAMILY| DR0| DR1| DR2| DR3| ILL| ILL| DR6| DR7|         
80 0F 2PI J|X,         ( FFF08C) 00 90 0F 3PI SET,             
100 0 2 1FAMILY| Y'| N'|                                       
200 0 8 1FAMILY| O'| C'| Z'| CZ'| S'| P'| L'| LE'|             
                                                               
1 0FA0 3 2FAMILY, PUSH|FS, POP|FS, CPUID,                      
800 FA300 4 3FAMILY, BT, BTS, BTR, BTC,                        
800 FA400 2 3FAMILY, SHLDI, SHRDI,                             
800 FA500 2 3FAMILY, SHLD|C, SHRD|C,                           
1 0FB200 4 3FAMILY, LSS, HOLE LFS, LGS,                        
800 0FB600 2 3FAMILY, MOVZX|B, MOVSX|B,                        
800 0FB700 2 3FAMILY, MOVZX|W, MOVSX|W,                        
                                                               
1 0FA8 2 2FAMILY, PUSH|GS, POP|GS,                             
800 0F0000 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,        
800 0FBA20 4 3FAMILY, BTI, BTSI, BTRI, BTCI,                   
800 0F0F00 7 3FAMILY, SGDT, SIDT, LGDT, LIDT, SMSW, HOLE LMSW, 
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( PROTECTED MODE  SWITCHING MACROS a0JUL03 AvdH HCC HOLLAND)   
ASSEMBLER DEFINITIONS                                          
: GET-CR0   MOV|CD, F| CR0| R| AX| ;                           
: PUT-CR0   MOV|CD, T| CR0| R| AX| ;                           
: TO-PROT,  GET-CR0  INCX, AX|  PUT-CR0 ;                      
: TO-REAL,  GET-CR0  DECX, AX|  PUT-CR0 ;                      
: COPY-SEG  MOVXI, AX| ( DAT -- ) W,   MOVSW, T| DS| R| AX|    
            MOVSW, T| ES| R| AX|   MOVSW, T| SS| R| AX|  ;     
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( PROTECTED MODE  SWITCHING MACROS a0JUL03 AvdH HCC HOLLAND)   
: NOP, XCHGX, AX| ;                                            
: CP, MOVTA, B| SWAP DUP , 1 + MOVFA, SWAP DUP , 1 + ;         
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( GENERAL PROTECTED MODE 16/32 ASSEMBLER ELECTIVES 0AJUL03 AH )
VOCABULARY ASSEMBLER IMMEDIATE                                 
: ;CODE                                                        
?CSP   POSTPONE   (;CODE)   [COMPILE] [   [COMPILE] ASSEMBLER  
; IMMEDIATE                                                    
 121 124 HEX THRU  DECIMAL ( Common code , prelude)            
ASSEMBLER DEFINITIONS                                          
 DECIMAL 110 LOAD  ( PROTECTED MODE 16/32)                     
 125 129 HEX THRU  DECIMAL ( Common code, postlude)            
FORTH DEFINITIONS                                              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( POST-IT/FIX-UP 8086 ASSEMBLER , POSTLUDE AvdH HCCFIG HOLLAND)
ASSEMBLER DEFINITIONS                                          
 0 IVAR ISS ( Instruction start )                              
: X, , ;    ( Cell size du jour)                               
: MEM, X, ;                                                    
: DAT, X, ;                                                    
: REL, ISS @ - X, ;                                            
: B, C, ;                                                      
( To be used when overruling, e.g. prefix)                     
: lsbyte, 0 100 UM/MOD SWAP C, ;                               
: W, lsbyte, lsbyte, DROP ;                                    
: L, lsbyte, lsbyte, lsbyte, lsbyte, DROP ;                    
: RELL, ISS @ - L, ;  : RELW, ISS @ - W, ;                     
: RELB, ISS @ - B, ;                                           
: SEG, W, ;                                                    
                                                               
( AUXILIARY DEFINITIONS )                                      
: <POST HERE ISS ! ;                                           
 0 IVAR IDP                                                    
: <FIX HERE IDP ! ; : IHERE IDP @ ;                            
: C|, -1 IDP +! IHERE C@ OR IHERE C! ;  ( c.f. C, )            
: C@+ COUNT ;  : C@- 1 - DUP C@ ; ( : C!+ >R R@ ! R> 1+ ;)     
: POST, C@+ C, ;       : FIX| C@- C|, ;                        
                                                               
: 1PI CREATE C, DOES> <POST POST, DROP ;                       
: 2PI CREATE C, C, DOES> <POST POST, POST, DROP ;              
: 3PI CREATE C, C, C, DOES> <POST POST, POST, POST, DROP ;     
                                                               
: 1FI CREATE C, DOES> 1+ <FIX FIX| DROP ;                      
: 2FI CREATE C, C, DOES> 2 + <FIX FIX| FIX| DROP ;             
: 3FI CREATE C, C, C, DOES> 3 + <FIX FIX| FIX| FIX| DROP ;     
                                                               
( PROTECTED MODE  SWITCHING a0jun20        AvdH HCC HOLLAND)   
: SPLIT 0 100 UM/MOD ; ( To handle two bytes at once )         
: SPLIT2 SPLIT SPLIT ; ( To handle three bytes at once )       
( INCREMENT, OPCODE , COUNT -- )                               
: 1FAMILY, 0 DO DUP 1PI OVER + LOOP DROP DROP ;                
: 2FAMILY, 0 DO DUP SPLIT SWAP 2PI OVER + LOOP DROP DROP ;     
: 3FAMILY, 0 DO DUP SPLIT2 SWAP ROT 3PI OVER + LOOP DROP DROP ;
                                                               
: 1FAMILY| 0 DO DUP 1FI OVER + LOOP DROP DROP ;                
: 2FAMILY| 0 DO DUP SPLIT SWAP 2FI OVER + LOOP DROP DROP ;     
: 3FAMILY| 0 DO DUP SPLIT2 SWAP ROT 3FI OVER + LOOP DROP DROP ;
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( POST-IT/FIX-UP 8086 ASSEMBLER , POSTLUDE AvdH HCCFIG HOLLAND)
ASSEMBLER DEFINITIONS                                          
: NEXT                                                         
     LODS, W1|                                                 
     MOV, W| F| AX'| R| BX|                                    
     JMPO, D0| [BX]                                            
 ;                                                             
FORTH DEFINITIONS                                              
: CODE ?EXEC (WORD) (CREATE) [COMPILE] ASSEMBLER !CSP ;        
IMMEDIATE                                                      
: C; CURRENT @ SEARCH ! ?EXEC ?CSP ; IMMEDIATE                 
                                                               
                                                               
                                                               
                                                               
                                                               
( POST-IT/FIX-UP 8086 ASSEMBLER , POSTLUDE AvdH HCCFIG HOLLAND)
ASSEMBLER DEFINITIONS                                          
 7C0 CONSTANT SWITCH_DS 17C0 CONSTANT GDT_DS                   
 10 CONSTANT GDT_CS                                            
: JMP-PROT, JMPFAR, HERE 4 + , GDT_CS W, ;                     
: JMP-REAL, JMPFAR, HERE 4 + SWITCH_DS 10 * - , SWITCH_DS W, ; 
: TO-REAL, JMP-REAL,  TO-REAL, ;                               
: TO-PROT,  TO-PROT, JMP-PROT, ;                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( 2DROP ALIGN                         A0JUL03 AvdH HCC HOLLAND)
FORTH DEFINITIONS                                              
: 2DROP DROP DROP ;                                            
: ALIGN HERE 1 AND IF 0 C, THEN ;                              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( 8086 ASSEMBLER TESTS    A0JUL05 AvdH HCC FIG HOLLAND)        
( Tests applicable always )                                    
  CODE TEST-NEXT NEXT  C;                                      
  TEST-NEXT                                                    
  ." HOERA WE ZIJN NIET GECRASHT"                              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( PROTECTED MODE  SWITCHING MACROS a0JUL03 AvdH HCC HOLLAND)   
?32  ( Test applicable to 32 bit mode)                         
FORTH DEFINITIONS DECIMAL                                      
CODE TEST-JUMP JMP-REAL, JMP-PROT, NEXT C;                     
                                                               
( CODE TEST-MORE TO-REAL,   TO-PROT, NEXT C;               )   
( CODE TEST-SWITCH   TO-REAL,   SWITCH_DS COPY-SEG   TO-PROT,) 
( GDT_DS COPY-SEG   NEXT C;                                  ) 
RSP@ 100 DUMP DSP@ 100 DUMP                                    
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( TEST OF HARD DISK ) ?16 HEX                                  
CODE READ-BLOCK2 4200 R/W-BLOCK  C;  ( D - . )                 
 CODE WRITE-BLOCK2 4300 R/W-BLOCK  C; ( D - . )                
DECIMAL : TEST  0.                                             
  BEGIN  CR 2DUP D.                                            
         2000. D+ ( SKIP 1 MEG)                                
         2DUP READ-BLOCK2 1 AND UNTIL                          
DROP DROP ;                                                    
0 IVAR SYSTEM-OFFSET                                           
HEX : SAVE 140 * SYSTEM-OFFSET !                               
  140 0 DO I 0 READ-BLOCK2 .                                   
         SYSTEM-OFFSET @ I + 0 WRITE-BLOCK2 .                  
  LOOP ;                                                       
: .ELECTIVE 140 UM* 48. D+ READ-BLOCK2 . RW-BUFFER C/L TYPE ;  
DECIMAL                                                        
                                                               
( TEST OF HARD DISK )                                          
HEX  CODE READ-BLOCK2 4200 R/W-BLOCK  C;  ( D - . )            
 CODE WRITE-BLOCK2 4300 R/W-BLOCK  C; ( D - . )                
DECIMAL : TEST  0.                                             
  BEGIN  CR 2DUP D.                                            
         2000. D+ ( SKIP 1 MEG)                                
         2DUP READ-BLOCK2 1 AND UNTIL                          
DROP DROP ;                                                    
0 IVAR SYSTEM-OFFSET                                           
HEX : SAVE 140 * SYSTEM-OFFSET !                               
  140 0 DO I 0 READ-BLOCK2 .                                   
         SYSTEM-OFFSET @ I + 0 WRITE-BLOCK2 .                  
  LOOP ;                                                       
: .ELECTIVE 140 UM* 48. D+ READ-BLOCK2 . RW-BUFFER C/L TYPE ;  
DECIMAL                                                        
DECIMAL                                                        
( 16 BITS: Experimenting with drive parameters ) HEX           
ALIGN 0 IVAR RW-BUFFER B/BUF ALLOT                             
0 IVAR PARAM-BLOCK -2 ALLOT 10 C, 0 C,                         
2 , ( 2 sectors/block) RW-BUFFER , 7C0 ,                       
HERE 2 ALLOT  0 , 0 , 0 , CONSTANT BL#                         
 : R/W-BLOCK  ASSEMBLER  ( MACRO: OPCODE -- . )                
  POPX, BX|    POPX, AX|                                       
  ADD, W| AX'| R| AX|  MOVFA, W1| BL# W, XCHGX, BX|            
  ADC, W| AX'| R| AX|  MOVFA, W1| BL# 2 + W,                   
  PUSHX, SI|  MOVXI, BX| W,  MOVXI, DX| 0080 W,                
  MOVXI, SI| PARAM-BLOCK W,  TO-REAL,                          
  MOVI, W| AX| 7C0 MEM,  MOVSW, T| DS| AX|                     
  XCHGX, BX|                                                   
  INT, 13 B, PUSHF, POPX, BX| TO-PROT,                         
  POPX, SI|   PUSHX, BX|  NEXT ;                               
DECIMAL                                                        
 ( 01-APR-83 LADEN VAN CP/M FILE  #1 )                         
 ( EXAMPLE: .OPENR TEMP" 25 26 .LOAD .CLOSER )                 
 <HEX  0 IVAR LBUF 3E ALLOT 0 C,                               
 : I-F-A ( ADRES -- . ,INTERPRET FROM ADDRESS )                
     TIB @ >R  IN @ >R  ( SAVE CURRENT INTERPRET POSITION)     
     TIB !     0 IN !   ( NEW POSITION)                        
     0 INTERPRET                                               
     >R IN !   >R TIB ! ( RESTORE)  ;                          
                                                               
 : .LOAD ( LOAD THE CPM FILE SPECIFIED IN FCB2 )               
         BEGIN   LBUF DUP GET-LINE I-F-A                       
         EOF @ UNTIL ;                                         
                                                               
                                                               
    HEX>                                                       
                                                               
 CR ." TAARTEN AUTOMATISERING DOOR DRS HENK" CR                
 ." EEN VOORBEELD UIT BRODIE"     CR                           
 ." TYPE HELP VOOR DE GLOSSARY"  CR                            
 0 IVAR TAARTEN     0 IVAR DIEP-VRIES                          
 : HELP CR ." GLOSSARY:" CR ." BAK-TAART"                      
        CR ." EET-TAART" CR ." VRIES-IN" CR ." ONTDOOI"        
        CR ." START" CR ." STATUS" CR ;                        
 : START 0 TAARTEN ! 0 DIEP-VRIES ! ;                          
 : BAK-TAART 1 TAARTEN +! ;                                    
 : EET-TAART TAARTEN @ DUP                                     
       IF -1 TAARTEN +! CR ." DANKJEWEL !" CR ELSE             
         CR ." WELKE TAART ?" CR DROP THEN ;                   
 : VRIES-IN TAARTEN @ DIEP-VRIES +! 0 TAARTEN ! ;              
 : ONTDOOI DIEP-VRIES @ TAARTEN +! 0 DIEP-VRIES ! ;            
 : STATUS CR ." AANTAL AANWEZIGE TAARTEN: " TAARTEN ?          
   CR ." EN NOG " DIEP-VRIES ? ." IN DE DIEP VRIES " ;         
  ( EXTENDING THE FORTH SYSTEM #1 84/4/12 A.H.)                
(  <HEX                                                        
(  : NEW-SYSTEM   ( Generates a new FORTH system, )            
(                 ( using the CP/M SAVE command)               
(       LATEST N>P NFAO 10C ! ( Define new topmost word)       
(       ( Initial value for VOC-LINK and FENCE:)               
(       HERE DUP 11C ! 11E !                                   
(       HERE 100 / DECIMAL CR                                  
(       CR ." TYPE: SAVE" . ." NEWFORTH.COM"                   
(       BYE                                                    
(  ;     HEX>                                                  
( old)                                                         
                                                               
                                                               
                                                               
                                                               
 ." GILBREATH's benchmark - BYTE jan 83 "  ( VERSIE #1)        
 8190 CONSTANT SIZE                                            
 0 IVAR FLAGS      SIZE ALLOT                                  
                                                               
 : DO-PRIME                                                    
     FLAGS SIZE 1 FILL                                         
     0 ( 0 COUNT ) SIZE 0                                      
     DO FLAGS I + C@                                           
        IF I DUP + 3 +  ( DUP . )                              
           DUP I +                                             
           BEGIN DUP SIZE <                                    
           WHILE 0 OVER FLAGS +  C!  OVER + REPEAT             
           DROP DROP 1+                                        
        THEN                                                   
     LOOP                                                      
     . ." PRIMES" ;                                            
 ." ERATOSTHENES >1< Variables - A. van der Horst"  CR         
 ( User specified variables:)                                  
 52 IVAR CH/L  ( Characters per line)                          
 22 IVAR LN/P  ( Lines per page)                               
  1 IVAR PAUSE ( Boolean: pause between pages)                 
                                                               
 ( Other:)                                                     
 6250 CONSTANT SIZE ( 16 numbers pro byte)                     
 0 IVAR FLAGS      SIZE ALLOT                                  
 FLAGS SIZE + CONSTANT END-FLAGS                               
 0 IVAR LIM     ( part of FLAGS considered)                    
 0 IVAR C#      0 IVAR L#  ( char and line counter)            
 0 IVAR THOUSANDS ( #  thousand to be sieved)                  
 0 IVAR MILS      ( Contains current thousand)                 
 0 IVAR MANTISSA  ( The current thousands is to be printed)    
                                                               
 ." ERATOSTHENES >2< Pretty printing - A. van der Horst"  CR   
 : FFEED  PAUSE @ IF CR ." KEY FOR NEXT SCREEN" KEY DROP THEN  
     12 EMIT CR ." ERATOSTHENES SIEVE -- PRIMES LESS THAN"     
     THOUSANDS @ 5 .R ."  000" CR 2 L# ! 1 MANTISSA ! ;        
 : ?P ( LENGTH -- . , give FF if LENGTH lines don't fat)       
      DUP L# +! L# @ LN/P @ > IF FFEED L# +! ELSE DROP THEN ;  
 : NEWLINE  ( Start at a new line, maybe with a mantissa)      
     1 ?P CR ( Checks first)                                   
     MANTISSA @ IF MILS @ 6 .R ELSE 6 SPACES THEN              
     6 C# !   0 MANTISSA ! ;                                   
 : ?L ( LENGTH -- . , give LF if LENGTH char's don't fit)      
      DUP C# +! C# @ CH/L @ >                                  
      IF NEWLINE C# +! ELSE DROP THEN ;                        
 : .P   4 ?L SPACE 0 <# # # # #> TYPE ;                        
 : INIT-P  FFEED NEWLINE  ;                                    
                                                               
 ." ERATOSTHENES >3< Bit manipulation - A. van der Horst " CR  
   HEX                                                         
 : NOT   0FF XOR ( N -- N  FLIP ALL BITS OF N) ;               
 0 IVAR S-MASK -2 ALLOT 01 C, 02 C, 04 C, 08 C,                
                            10 C, 20 C, 40 C, 80 C,            
 0 IVAR C-MASK -2 ALLOT                                        
             01 NOT C, 02 NOT C, 04 NOT C, 08 NOT C,           
             10 NOT C, 20 NOT C, 40 NOT C, 80 NOT C,           
 : INIT-T   FLAGS SIZE 0FF FILL ; ( Preset to 'prime')         
 DECIMAL                                                       
 : 8/MOD   0 8 UM/MOD ; ( May be redefined in assembler )      
 : CLEAR-B ( BIT# --  clears the specified bit)                
           8/MOD FLAGS + SWAP  ( Address in flags table)       
           C-MASK + C@         ( Get mask)                     
           OVER C@ AND SWAP C! ( Clear the bit)  ;             
                                                               
 ." ERATOSTHENES >4< Bit manipulation - A. van der Horst " CR  
 : SET-B ( BIT# --  sets the specified bit)                    
           8/MOD FLAGS + SWAP  ( Address in flags table)       
           S-MASK + C@         ( Get mask)                     
           OVER C@ OR SWAP C!  ( Store with bit set)  ;        
 : TEST-B ( BIT# -- FLAG  Gets a FLAG testable by IF)          
           8/MOD FLAGS + C@ SWAP  ( Get flag)                  
           S-MASK + C@ AND        ( Result: =0 or #0)     ;    
 : FLIP ( PRIME,START -- .  , marks multiples of PRIME as      
        (  non prime starting with START)                      
           BEGIN  DUP LIM @ U<  WHILE                          
                  DUP CLEAR-B  OVER +                          
           REPEAT   DROP DROP ;                                
                                                               
                                                               
                                                               
 ." ERATOSTHENES >5< Main program - A. van der Horst " CR      
 : BATCH1 ( First batch of 500 numbers)                        
      500 1 ( Only odd numbers)                                
     DO I TEST-B                                               
        IF I DUP + 1 + DUP .P ( get prime number)              
           I FLIP THEN ( Mark multiple as non-prime)           
     LOOP ;                                                    
 : BATCH ( OFFSET --  every  following batch )                 
      500 0                                                    
      DO DUP I + TEST-B IF I DUP + 1 + .P THEN                 
      LOOP DROP ;                                              
 : SIEVE  ( N --  Makes list of primes <N*1000 )               
     DUP THOUSANDS !   500 * LIM !  0 MILS !                   
     INIT-T INIT-P 2 .P BATCH1                                 
     THOUSANDS @ 1                                             
     DO I MILS !  1 MANTISSA !  NEWLINE I 500 * BATCH LOOP ;   
 ." DEFINIEER ^. EN  ''.  85JAN06 ALBERT VAN DER HORST"        
 <HEX                                                          
  : TOH 30 - DUP 9 > IF 7 - THEN ;                             
     1 WIDTH !                                                 
  : ". ( 0/1 Leaves ASCII character at .  f.i. 'A leaves 41H)  
    HERE 2 + C@ [COMPILE] LITERAL ; IMMEDIATE                  
  : ^. ( 0/1 leaves control character at . f.i. ^A leaves 01H) 
    HERE 2 + C@ 1F AND [COMPILE] LITERAL ; IMMEDIATE           
  : $.. ( 0/1 leaves hex number f.i. $0A leaves 0AH)           
    HERE 2 + C@ TOH 10 * HERE 3 + C@ TOH + [COMPILE] LITERAL ; 
  IMMEDIATE                                                    
  : $.... ( 0/1 leaves hex number 16 bits)                     
     0 HERE 6 + HERE 2 + DO 10 * I C@ TOH + LOOP               
     [COMPILE] LITERAL ; IMMEDIATE                             
     1F WIDTH ! HEX>                                           
  $1B CONSTANT ESC    $0F CONSTANT SI   $0E CONSTANT SO        
 ." SYSTEM ELECTIVE CP/M FIGFORTH EXTENSIONS 3.43    AH"       
    1 LOAD  ( 16/32 BIT DEPENDANCIES)                          
 ( MAINTENANCE )  100 LOAD 32 LOAD                             
( HEX CHAR DUMP)  6 LOAD 30 LOAD 7 LOAD 39 LOAD ( i.a. editor) 
 ( STRING         36 LOAD 37 LOAD )                            
 ( EDITOR )       101 105 THRU 107 LOAD 106 LOAD               
 ( CP/M READ WRITE LOAD    15 LOAD 18 LOAD 21 LOAD 21: BUGS)   
 ( KRAKER )        10 LOAD                                     
 ( NEW SYSTEM      23 LOAD   )                                 
 ( CRC             71 LOAD   )                                 
 ( ASSEMBLER 8080  74 LOAD   )                                 
 ( ASSEMBLER 80x86) 120 LOAD                                   
 ( STAR PRINTER    31 LOAD   )                                 
 ( CP/M CONVERT    80 LOAD   )                                 
 WARNING 1 TOGGLE                                              
 2 LIST    : TASK ;                                            
: LOAD DSP@ >R LOAD R> DSP@ ?PAIRS ;                           
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
 CR ." KRAAKER"                                                
 : NEXTD ( CFA--DEA Get the DEA of the word defined)           
   CFA> LATEST             ( after the CFAO one)               
   2DUP = IF                                                   
     DROP DROP HERE  ( No following word)                      
   ELSE                                                        
     BEGIN                                                     
        2DUP >LFA @ <> WHILE                                   
        >LFA @                                                 
     REPEAT                                                    
     SWAP DROP ( The CFA)                                      
   THEN                                                        
 ;                                                             
 : NEXTC ( CFA--CFA Like previous definition, giving CFA)      
   NEXTD >CFA ;                                                
                                                               
( SAVE-SYSTEM )  HEX                                           
 CREATE MAGIC 7F C, &E C, &L C, &F C,                          
 : FIND-ELF BM BEGIN DUP @ MAGIC @ <> WHILE                    
 1 CELLS - REPEAT ; FIND-ELF CONSTANT SM                       
 : HERE-AT-STARTUP  ' DP >DFA @ +ORIGIN @ ;                    
 : SAVE-SYSTEM ( sc -- )                                       
  HERE HERE-AT-STARTUP - DUP                                   
  SM 20 + +!      SM 44 + +! ( File&Dict size)                 
   U0 @   0 +ORIGIN   100   MOVE ( Save user variables)        
   SM    HERE  SM - 2SWAP ( name) PUT-FILE ;                   
: TURNKEY  ( dea sc -- ) ROT                                   
>DFA @  ' ABORT >DFA !  SAVE-SYSTEM BYE ; DECIMAL              
: ARGC ARGS @ @ ;  : ARGV ARGS @ CELL+ ;                       
: ENV ARGS @ $@ 1+ CELLS + ;                                   
: Z$@ DUP BEGIN COUNT 0= UNTIL 1- OVER - ;  : CTYPE Z$@ $. ;   
: .C$ BEGIN $@ DUP WHILE CR CTYPE REPEAT DROP DROP ;           
5 CONSTANT OPEN                                                
3 CONSTANT READ                                                
6 CONSTANT CLOSE                                               
0 CONSTANT O_RDONLY                                            
: GET-FILE              ( <a><u> --- i*x )                     
   HERE >R   10000000 ALLOT                                    
   R@ $! 0 R@ $C+  "FiLeBuF" R@ $+!                            
   R> $@ OVER + >R                                             
   O_RDONLY 0 OPEN LINOS DUP ?LINUX-ERROR                      
   DUP R@ 9999000 READ LINOS DUP ?LINUX-ERROR >R               
   0 0 CLOSE LINOS ?LINUX-ERROR                                
   R> R> SWAP 2DUP + DP !;                                     
: INCLUDED                                                     
  GET-FILE                                                     
  SAVE (EVAL) INTERPRET RESTORE ;                              
                                                               
 CR ." #36 FROBOZZ AMATEUR ADVENTURER >1< 84/4/5 "             
 ( DIRECTIONS )                                                
 0 CONSTANT N  1 CONSTANT NE                                   
 2 CONSTANT E  3 CONSTANT SE   8 CONSTANT U                    
 4 CONSTANT S  5 CONSTANT SW   9 CONSTANT D                    
 6 CONSTANT W  7 CONSTANT NW                                   
 14 CONSTANT LEN ( gth of 1 entry in map)                      
 0 IVAR MAP LEN 256 * ALLOT                                    
 ( Offsets from entry in map :)                                
 : [inuse] 0 + ; ( boolean : valid entry )                     
 : [dist] 1+ ; ( 0=unknown,1..FD=distance+1)                   
   0 CONSTANT UNK ( nown)   255 CONSTANT TRUE                  
 254 CONSTANT AMB ( iguous)   255 CONSTANT BLO ( cked)         
 : [dir] + 2 + ;  ( M D -- A address A from MAP entry M dir D) 
 : [char] 12 + ; ( characterisation : 2 bytes)                 
 : ZAP MAP LEN 256 * ERASE ;  149 169 THRU                     
 CR  ." #37 FROBOZZ AMATEUR ADVENTURER >2< 84/5/27"            
 0 IVAR STATUS 10 ALLOT   ( last 10 moves)                     
 0 IVAR CURPOS ( This position in map)                         
 0 IVAR OLDPOS ( Previous position  in map)                    
 : SHIFT STATUS 1+ DUP 1 - 10 CMOVE ;                          
 STATUS 10 + CONSTANT LASTMOVE                                 
 : MAP[]   ( N -- M Converts position N to address M)          
      LEN * MAP + ;                                            
 : CN   ( defines a characterization )                         
         0 IVAR ; ( later misschien net vocabje)               
  CN AMBIGUOUS  CN BLOCKED  CN UNKNOWN                         
 : ID..   ID. ;                                                
 : .CHAR  DUP MAP[] [char] @ ( N-- Print char'n)               
   DUP IF                                                      
     DUP ID.. @ AMB = IF ." #" . ELSE DROP THEN                
   ELSE    DROP ." PLACE:#" . THEN ;                           
 CR ." #38 FROBOZZ AMATEUR ADVENTURER >3< 84/5/27"             
 : WHERE? CURPOS @ .CHAR ;                                     
 : C-CONNECT ( C P -- Cross connect links between an )         
    ( unambigous characterization and a position )             
     2DUP SWAP !  ( Point from characterization to map address)
     MAP[] [char] ! ( Store char'n in map)                     
 ;                                                             
 : P-CONNECT ( P1 P2 D -- Connect from P1 to P2 in direction D)
     ROT MAP[] [dir] C!                                        
 ;                                                             
 : NEW-ENTRY ( -- n Find first free position n in MAP)         
     MAP 0  BEGIN 1+ SWAP LEN + SWAP OVER [inuse]  C@ 0= UNTIL 
     SWAP TRUE SWAP [inuse] !                                  
 ;                                                             
                                                               
                                                               
 CR  ." #39 FROBOZZ AMATEUR ADVENTURER >4< 84/4/8 "            
 : FILL-IN  ( Fills in the information from last move)         
    OLDPOS @ CURPOS @ LASTMOVE C@ P-CONNECT                    
 ;                                                             
 : DIRECTION ( N --  moves the status in direction N)          
     SHIFT DUP LASTMOVE C!                                     
     CURPOS C@ MAP[] [dir] C@ ( get # of next place in map)    
     DUP CR ." OSB: exit is : " .CHAR CR                       
     ( Is it an unknown exit?)                                 
     DUP 0= IF DROP NEW-ENTRY THEN                             
     DUP BLO - IF ( Only change position if not blocked!)      
       CURPOS @ OLDPOS !  CURPOS !                             
       FILL-IN ( Only effective by a new entry )               
     ELSE                                                      
       DROP                                                    
     THEN  ;                                                   
CR  ." #40 FROBOZZ AMATEUR ADVENTURER >5< 84/6/27"             
 : HERE: ( Defining word:makes a name for the current place)   
   CURPOS @ MAP[] [char] @ 0= IF                               
      0 IVAR LATEST N>P CURPOS @ C-CONNECT                     
      FILL-IN ( The last position connection)                  
   ELSE                                                        
      CR ." This place already been characterized" QUIT        
   THEN ;                                                      
 : TEXT ." N  NE E  SE S  SW W  NW U  D  " ;                   
 : .DIR 1+ 3 * ' TEXT >DFA + 3 TYPE ;                          
 : SEND-DIR 1+ 3 * ' TEXT >DFA + 3 PTYPE ;                     
 : EXIT?  ( Print current exits)                               
   CURPOS @ MAP[] 10 0 DO                                      
     DUP I [dir] C@                                            
     DUP BLO - IF CR I .DIR ." : " .CHAR ELSE DROP THEN        
   LOOP DROP ;                                                 
CR  ." #41 FROBOZZ AMATEUR ADVENTURER >6< 84/4/9 "             
 : CLEAR! ( N -- Clears a place)                               
   MAP[] LEN ERASE ;                                           
 : AUX! ( P -- Characterizes with P the current place)         
    CURPOS @ MAP[] [char] @ IF                                 
      ." This place already characterized"                     
    ELSE                                                       
      CURPOS @ CLEAR! ( Throw away)                            
      OLDPOS @ SWAP LASTMOVE C@ P-CONNECT                      
    THEN ;                                                     
 : HERE! @ DUP AUX! DUP CURPOS ! ; ( Use: TROLL HERE! )        
 : ALSO! AMB OVER ! CURPOS @ MAP[] [char] ! ; ( Use:MAZE ALSO!)
 : BACKK    OLDPOS @ CURPOS !  ;                               
 : BLOCKED! BLO AUX! BACKK ;                                   
 : AMB!     AMB AUX! ;                                         
 : KILL! DUP @ MAP[] [char] 0 SWAP !  0 SWAP ! ;               
 CR  ." #42 FROBOZZ AMATEUR ADVENTURER >7< 84/6/28"            
 : P?    ( P -- Dumps a place)                                 
   CR ." PLACE# FL AU " TEXT ." CHAR"  CR                      
   DUP 3 .R 4 SPACES                                           
   DUP MAP[] LEN 2 - 0 DO DUP I + C@ B. SPACE LOOP DROP        
   .CHAR                                                       
 ;                                                             
 : PATCH ( Use: WHOUSE SHOUSE S PATCH )                        
       ROT @ ROT @ ROT P-CONNECT ;                             
 : ?FIX  MAP[] [char] @ DUP 0= 0= SWAP @ AMB = 0= AND ;        
 : ?SWAP ( P1 P2 -- {P1,P2} Change order such that )           
         ( P2 has no fixed characterization)                   
   DUP ?FIX IF SWAP THEN                                       
   DUP ?FIX IF                                                 
        CR ." Places differently characterized"  QUIT          
   THEN  ;                                                     
 CR  ." #43 FROBOZZ AMATEUR ADVENTURER >8< 84/4/19"            
 : REPLACE ( P1 P2--. Replace all references to P2 by P1)      
   ?SWAP   ( or the other way around if P2 characterized)      
   CURPOS @ OVER = IF                                          
       OVER ( P1) CURPOS !                                     
   THEN                                                        
   256  0 DO ( For all place do)                               
      I MAP[] 0 [dir] 10 OVER + SWAP DO ( For 10 dir's do)     
         I C@ OVER ( P2) = IF                                  
            OVER ( P1) I C!                                    
         THEN                                                  
      LOOP                                                     
   LOOP  DROP DROP                                             
 ;                                                             
                                                               
                                                               
 CR  ." #44 FROBOZZ AMATEUR ADVENTURER >9< 84/5/27"            
 : MYSELF LATEST >CFA , ; IMMEDIATE                            
 : (IDENT)  ( n1 n1 -- Identify locations n1 and n2)           
   ?SWAP 2DUP REPLACE SWAP ( Destination location on top)      
   10 0 DO ( For all directions do)                            
      OVER MAP[] I [dir] C@                                    
      OVER MAP[] I [dir] C@                                    
      2DUP = >R 2DUP UNK = SWAP UNK =  OR R> OR IF             
         MAX OVER MAP[] I [dir] C!                             
      ELSE                                                     
         CR MYSELF                                             
      THEN                                                     
   LOOP                                                        
   DROP ( Destination location) CLEAR! ( Other)                
 ;                                                             
 : FOUND! @ CURPOS @ (IDENT) ;                                 
 CR ." #45 FROBOZZ MAGIC COMMUNICATION >9A< 84/6/28 "          
  <HEX 54 3 C! HEX>                                            
 ( 30 LOAD 31 LOAD ( Get standard communication)               
 0 IVAR REMEMBER 254 ALLOT ( keeps last message from host)     
 0 IVAR L-W-L ( Boolean: last character was line-feed)         
 : STORE ( C--. Store char in REMEMBER)                        
      127 AND ( strip parity)                                  
      DUP "> - IF ( unless prompt)                             
         L-W-L @ IF 0 REMEMBER C! THEN                         
         DUP REMEMBER COUNT + C!      ( Store char an place)   
         REMEMBER C@ 1 + REMEMBER C!  ( Bump count)            
         ^J = L-W-L ! ( Keep flag)                             
      ELSE DROP THEN  ;                                        
 : L-STORE ( A L -. Store A in list L )                        
  2 OVER +! DUP @ + ! ;                                        
                                                               
CR  ." #46 FROBOZZ MAGIC COMMUNICATION >10< 84/6/27"           
                                                               
: ECR 13 4 BDOS DROP ;                                         
: WAIT BEGIN 0 3 BDOS DUP EMIT DUP STORE                       
     127 AND   "> = KEY? OR UNTIL  ;                           
 : \ ( Sends a line and wait for a prompt)                     
     P." ECR ( Send the remainder of the line)                 
     WAIT ;                                                    
 : COM-DIR DUP SEND-DIR ECR WAIT DIRECTION ;                   
 : n N COM-DIR ;  : ne NE COM-DIR ;                            
 : e E COM-DIR ;  : se SE COM-DIR ;                            
 : s S COM-DIR ;  : sw SW COM-DIR ;                            
 : w W COM-DIR ;  : nw NW COM-DIR ;                            
 : u U COM-DIR ;  : d  D  COM-DIR ;                            
 : bl BLOCKED! ; : ex EXIT? ; : wh WHERE? ;                    
                                                               
 CR  ." #47 FROBOZZ AMATEUR ADVENTURER >11< 84/6/12 "          
 : SEND-NAME  >NFA @ $@ 127 AND PTYPE ;                        
 : DROP! DUP @ IF DUP KILL! THEN                               
     DUP CURPOS @ C-CONNECT   ' DROP >DFA SEND-NAME 32 PEMIT   
     SEND-NAME ECR WAIT ;                                      
 : MARK-AUX ( N-- Marks the exits from N)                      
   MAP[] DUP [dist] C@ 1+ ( Get the marking on top)            
   SWAP 0 [dir] 10 OVER + SWAP DO                              
      I C@ MAP[] [dist] 2DUP C@ < IF                           
        OVER SWAP C! ( I C@ MYSELF) ELSE                       
        DROP THEN                                              
   LOOP  DROP  ;                                               
 : M-D  MAP[] [dist] C! ; ( Mark distance)                     
 : INIT-AUX  ( Zeros all normal places)                        
   255 UNK M-D 255 AMB M-D 255 BLO M-D                         
   254 MAP[] [dist] 1 MAP[] [dist] DO 0 I C! LEN +LOOP ;       
 CR  ." #48 FROBOZZ AMATEUR ADVENTURER >12< 84/5/22 "          
 ( The [dist]'s are in a circular list for each distance)      
 ( The DIST-LIST contains the start entries in these lists)    
 0 IVAR DIST-LIST 50 ALLOT                                     
                                                               
 : APPEND ( I,J --. Append place #J to Ith circular last)      
   OVER DIST-LIST + C@ 0= IF  ( so list was empty)             
     DUP DUP MAP[] [dist] C! ( Make J refer to itself)         
     SWAP DIST-LIST + C! ( Make list I refer to place #J)      
   ELSE                                                        
      SWAP DIST-LIST + C@   ( Get start of circular last)      
      2DUP MAP[] [dist] C@ SWAP MAP[] [dist] C! ( Redarect 2nd)
      MAP[] [dist] C! ( Make 1th of list point to new entry)   
   THEN ;                                                      
                                                               
                                                               
 CR  ." #49 FROBOZZ AMATEUR ADVENTURER >13< 84/5/22 "          
                                                               
 : MARK-EX ( I,N-- Appends all exits from N to the Ith list)   
   MAP[] 0 [dir] 10 OVER + SWAP DO                             
      I C@ MAP[] [dist] C@ 0= IF ( This exit not yet marked)   
           DUP I C@ APPEND                                     
      THEN                                                     
   LOOP  DROP ;                                                
 0 IVAR START                                                  
 : MARK-NEW-LIST ( N -- Generate Nth circular list)            
     0 OVER DIST-LIST + C! ( initialize to empty)              
     DUP 1 - DIST-LIST + C@ DUP START C! ( Keep end of list)   
     BEGIN 2DUP MARK-EX MAP[] [dist] C@ DUP START C@ = UNTIL   
     DROP DROP ;                                               
                                                               
                                                               
 CR  ." #50 FROBOZZ AMATEUR ADVENTURER >14< 84/5/27 "          
 : (M-U-F) ( N --M Generates circular lists )                  
        ( until place #N in last one or last list empty)       
        ( returns M: moves needed)                             
   0 DIST-LIST !                                               
   0 CURPOS @ APPEND ( Create 0th circular list )              
   0 BEGIN                                                     
      1+ DUP MARK-NEW-LIST                                     
   OVER MAP[] [dist] C@ ( place N marked?)                     
   OVER DIST-LIST + C@ 0= ( dead end?) OR UNTIL                
   SWAP DROP ;                                                 
                                                               
  : M-U-F  INIT-AUX (M-U-F) ; ( N--M As M-U-F but initialized) 
  : L-U-U  ( --M Lists incorrect,find length to first UNKNOWN) 
     INIT-AUX   0 UNK M-D  UNK (M-U-F) ;                       
                                                               
 CR ." #51 FROBOZZ AMATEUR ADVENTURER >15< 01/2/20 "           
: FIND-something ( CFA -- )                                    
   CFA> LATEST             ( after the CFAO one)               
   2DUP = IF                                                   
              ( from place #K to #J in direction D)            
              ( Leaves -1 if nothing found)                    
   -1 ROT ROT SWAP ( Initiate not found flag)                  
   MAP[] 0 [dir] ( get first exit from #K)                     
   10 0 DO                                                     
      2DUP I + C@ = IF                                         
        DROP I ROT ROT LEAVE ( the direction as a flag)        
      THEN                                                     
   LOOP  DROP DROP ;                                           
                                                               
                                                               
                                                               
 CR ." #52 FROBOZZ AMATEUR ADVENTURER >16< 84/6/12 "           
 : FIND-FROM ( M,J--D,N Find out place #N in sublist #J)       
             ( from where to go in direction D to arrive at M) 
  DIST-LIST + C@                                               
  BEGIN                                                        
     MAP[] [dist] C@ 2DUP ( get next place from list)          
     SWAP CONNECTED? -1 -                                      
  UNTIL                                                        
  DUP ROT CONNECTED? ( Repeat last,sucessful query)            
 ;                                                             
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
 CR ." #53 FROBOZZ AMATEUR ADVENTURER >17< 84/6/28 "           
                                                               
 : F-P-B-N ( N--{0,DN..D2,D1} Find place by its number N)      
   ( N is reachable by going direction D1,D2..Dn)              
   -1 SWAP ( leave stack bottom) DUP M-U-F                     
   DUP DIST-LIST + C@ IF                                       
      1 - -1 SWAP DO                                           
         I FIND-FROM SWAP                                      
      -1 +LOOP DROP                                            
   ELSE                                                        
      CR ." CAN'T REACH THAT ROOM " CR                         
   THEN ;                                                      
                                                               
 : GOTO!# ( Use: KITCHEN @ GOTO!# )                            
    F-P-B-N CR BEGIN DUP 1+ WHILE COM-DIR CR REPEAT DROP ;     
 : GOTO!  @ GOTO!# ; ( Use: KITCHEN @ GOTO!# )                 
 CR  ." #54 FROBOZZ INTELLIGENT ENQUIRER >18< 84/6/12 "        
                                                               
 : KEEP: ( Get the info from REMEMBER into a named buffer)     
   0 IVAR HERE 2 -   REMEMBER C@ ALLOT                         
   REMEMBER SWAP OVER C@ 1+ CMOVE ;                            
                                                               
 : =$ ( S1,S2--F Leaves flag indication equality of strings)   
      1 ROT ROT ( Start with equal flag)                       
      DUP C@ 1+ 0 DO                                           
         OVER I + C@ OVER I + C@ - IF                          
            ROT DROP 0 ROT ROT ( Replace flag with 0) LEAVE    
         THEN                                                  
      LOOP DROP DROP  ;                                        
                                                               
                                                               
                                                               
 CR  ." #55 FROBOZZ INTELLIGENT ENQUIRER >19< 84/5/27 "        
                                                               
 : COMPARE-TEXTS ( T--F Leaves flag indicating whether one )   
       ( from the array of pointers T has arrived)             
   0 SWAP ( Start with no flag)                                
   DUP @ ?DUP IF                                               
     0 DO                                                      
        DUP I 1+ 2 * + @ REMEMBER =$ IF                        
           SWAP DROP 1 SWAP LEAVE                              
        THEN                                                   
     LOOP                                                      
   THEN  DROP ;                                                
                                                               
                                                               
                                                               
                                                               
 CR  ." #56 FROBOZZ INTELLIGENT ENQUIRER >20< 84/6/27"         
 0 IVAR BLK-TEXTS 10 ALLOT                                     
 : BLOCKED? BLK-TEXTS COMPARE-TEXTS IF                         
           BLOCKED!                                            
        ELSE                                                   
           CR ." Please do by hand! " CR QUIT                  
        THEN ;                                                 
 : TRY-HERE ( investigates all exits of the current position)  
 ( If it discovers blocked exits, it will take care of that,)  
 ( Special cases are left to the adventurer himself)           
   CURPOS @ MAP[] 0 [dir] 10 0 DO                              
     DUP I + C@ UNK = IF                                       
        I SEND-DIR ECR WAIT ( try this direction)              
        I DIRECTION    BLOCKED?                                
     THEN                                                      
   LOOP ;                                                      
 CR  ." #57 FROBOZZ INTELLIGENT ENQUIRER >21< 84/6/27 "        
                                                               
 : TRY-EVERY-WHERE ( As TRY-HERE but in the whole map)         
 TRY-HERE                                                      
 BEGIN  KEY? 0= WHILE                                          
   L-U-U 1 - DIST-LIST + C@ ( Get some place from prev. list)  
   M-U-F UNK SWAP FIND-FROM                                    
   DROP ( direction found)                                     
   GOTO!# ( the place with an UNknown exit)                    
   TRY-HERE                                                    
   BLOCKED?                                                    
 REPEAT                                                        
 ;                                                             
                                                               
                                                               
                                                               
( A0apr12 www STRING AND PARSING words , OLD WORD BU!)         
FORGET TASK      : TASK ;  CHAR " CONSTANT DLM   36 38 THRU    
: CREATE 0 IVAR 0 CELL+ NEGATE ALLOT ; ( Like ANSI)            
( All strings are copied to here, increase if needed)          
CREATE POOL 1000 ALLOT                                         
POOL IVAR PP ( LIKE DP)  : pd POOL PP @ OVER - DUMP ;          
: POOL-HERE PP @ ;                                             
: POOL-ALLOT PP +! ;                                           
: $SAVE DUP >R POOL-HERE $!   POOL-HERE R> 1+ POOL-ALLOT ;     
( WORD HAS SCANNED NOTHING )                                   
: EMPTY?  HERE 1 + C@  0= ;                                    
( AS WORD BUT WITH AUTOMATIC REFILL )                          
: WORD+ BEGIN DUP WORD  EMPTY?  WHILE QUERY REPEAT DROP ;      
: PARSE WORD+ HERE COUNT $SAVE ;                               
0 0 $SAVE CONSTANT NILL ( EMPTY STRING)                        
BL PARSE ##  CONSTANT ##   ( END-SENTINEL)                     
( A0apr12 www SET words )                                      
: SET+! DUP >R @ ! 0 CELL+ R> +! ;                             
: SET-FOR-ALL POSTPONE DUP POSTPONE @ POSTPONE SWAP            
COMPILE CELL+ POSTPONE (DO) HERE POSTPONE I POSTPONE @ ;       
IMMEDIATE                                                      
: END-S-F-A  POSTPONE 0 POSTPONE CELL+                         
POSTPONE (+LOOP) BACK ; IMMEDIATE                              
: duse SET-FOR-ALL CR . END-S-F-A ;                            
: SET CREATE HERE CELL+ , 100 CELLS ALLOT DOES> ;              
: #IN-SET -1 ROT ROT SET-FOR-ALL                               
  OVER = IF SWAP DROP I SWAP THEN END-S-F-A DROP ;             
: ?IN-SET #IN-SET -1 <> ;                                      
: #IN-SET-$ -1 ROT ROT SET-FOR-ALL OVER                        
$@= IF SWAP DROP I SWAP THEN END-S-F-A DROP ;                  
: ?IN-SET-$ #IN-SET-$ -1 <> ;                                  
                                                               
( A0apr12 www SET words )                                      
SET META-HTML        ( ALL SETS OFHTML FILES )                 
: REGISTER-HTML META-HTML SET+! ;                              
SET META-RELATION         ( ALL RELATIONS  )                   
: REGISTER-RELATION META-RELATION SET+! ;                      
SET FILES                                                      
: REGISTER-FILE DUP FILES ?IN-SET-$ 0=                         
IF FILES SET+! ELSE DROP THEN ;                                
: ?HTML-SET META-HTML ?IN-SET ;                                
( CREATE NAME-BUFFER 256 ALLOT  )                              
: PAR-TO-DATA CELL+ ; ( GO FROM N>P TO WHERE DOES> IS)         
: IS-SET-NAME ( SS -- 1)                                       
 LATEST (FIND) IF                                              
   DROP PAR-TO-DATA DUP ?HTML-SET IF ELSE DROP 0 THEN          
ELSE 0 THEN ;                                                  
: du$ SET-FOR-ALL CR $@ TYPE END-S-F-A ;                       
( A0apr12 www SET words ) HEX                                  
: COLLECT-REST ( OF LINE, MAY CONTAIN SPACES ## MEANS EMPTY)   
DLM WORD HERE ## $@= IF NILL ELSE HERE COUNT $SAVE THEN , ;    
: FILE/SET DUP IS-SET-NAME  DUP IF CELL+ ( SKIP TEXT)          
 SET-FOR-ALL ( DUP COUNT TYPE KEY DROP ) , END-S-F-A DROP      
ELSE DROP DUP REGISTER-FILE , THEN ;                           
: COLLECT   BEGIN BL PARSE DUP ## $@= 0=                       
WHILE  FILE/SET REPEAT DROP ;                                  
: $LATEST LATEST COUNT 1F AND $SAVE ; ( -- N)                  
( A set of html files with reference)                          
: SET-HTML CREATE HERE REGISTER-HTML COLLECT-REST              
HERE 0 , COLLECT HERE SWAP ! ( make it a set) DOES> ;          
: #SET CELL+ DUP @ SWAP CELL+ - 0 CELL+ / ;                    
: RELATION-HTML CREATE HERE REGISTER-RELATION COLLECT-REST     
BL PARSE DUP IS-SET-NAME 0= 18 ?ERROR , BL PARSE ,  DOES> ;    
DECIMAL                                                        
( Randomize a set)                                             
89 LOAD 60 LOAD                                                
: CHOOSE-FROM-SET DUP #SET CHOOSE 2 + CELLS + ;                
: RANDOM-SET DUP #SET SWAP CELL+                               
 DUP @ SWAP CELL+ DO 1 -                                       
DUP 1 > IF I OVER RANDOM-SWAP THEN                             
0 CELL+ +LOOP DROP ;                                           
: RANDOMIZE-SET DUP                                            
CELL+ DUP @ SWAP CELL+ DO                                      
DUP CHOOSE-FROM-SET I .S @SWAP                                 
0 CELL+ +LOOP DROP ;                                           
                                                               
                                                               
                                                               
                                                               
                                                               
: $? $@ $. ;                                                   
: WRITE TYPE ;                                                 
: PEEK DUP @ $? CELL+  ;                                       
: .set CR PEEK SET-FOR-ALL CR $? END-S-F-A ;                   
: .rel CR PEEK CR PEEK CR PEEK DROP ;                          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Define the texts used in html)                               
STRING A1 <A HREF="                                            
STRING A2 >"                                                   
STRING A3 <A/>"                                                
: WRITE TYPE ;                                                 
: NEXT-IN-SET SWAP DROP ;                                      
: REF-SET ( SET FILE -- )                                      
 A1 WRITE OVER OVER NEXT-IN-SET @ $@ WRITE  A2 WRITE DROP      
 @ $@ WRITE ;                                                  
: DO-FILE META-HTML SET-FOR-ALL                                
OVER OVER .S ?IN-SET-$ .S IF OVER REF-SET THEN                 
END-S-F-A DROP ;                                               
: DO-ALL FILES SET-FOR-ALL  DO-FILE END-S-F-A ;                
                                                               
                                                               
                                                               
SET-HTML JONGENS VOOR EEN ANDERE FORTH FILE"                   
JAN   PIET  KLAAS   ##                                         
SET-HTML MEISJES DOM BLONDE"                                   
MARIEKE HELMA DORIENTJE ##                                     
SET-HTML OPIE A SET CONTAINING OTHER SETS"                     
JONGENS  MEISJES ##    OPIE .set                               
RELATION-HTML O-SEX OF HEB JE LIEVER EEN MEISJE"               
JONGENS MEISJES                                                
RELATION-HTML A-SEX OF HEB JE LIEVER EEN JONGEN"               
MEISJES JONGENS                                                
RELATION-HTML NO-SEX OF HEB JE LIEVER EEN JOPIE"               
MEISJES JOPIE     NO-SEX .rel                                  
EXIT NOT YET                                                   
1-1-RELATION-HTML VR OF WIL JE HAAR VRIENTJE" MEISJES JOMGENS  
1-1-RELATION-HTML RV OF WIL JE ZIJN VRIENDIN" JOMGENS MEISJES  
                                                               
 ( STREAM READ ROUTINES CP/M 85/012/08  AH )                   
 : F_READ ( B,N-N2 Tries to read N char's to buffer B)         
          ( N2 is number actually read, 0 for EOF)             
      ( NOT  YET: NOW IT IS FILLED WITH ^Z, NOTHING RETURNED ) 
  BEGIN                                                        
     SWAP GET-CHAR                                             
     OVER C! 1+ SWAP 1 -                                       
     DUP 0=                                                    
  UNTIL                                                        
 ;                                                             
 : F_WRITE ( B,N-N2 Tries to write N char's from buffer B)     
      ( N2 is the number actually written to disk )            
      ( NOT  YET: NOW IT IS UNCLEAR, NOTHING RETURNED )        
   TO-DISK                                                     
 ;                                                             
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Main screen for parser AH&CH                        A0oct03 )
( Create a forward definition, one that patches its own        
  call with a cfa that is in its data field. Then goes on      
  with that definition. )                                      
: FORWARD CREATE 0 , DOES> @ DUP 0= 9 ?ERROR                   
   R> 1 CELLS - DUP >R  ! ;                                    
( : DOIT HERE IN @ POSTPONE ' POSTPONE >DFA ! IN !             
POSTPONE : ; )                                                 
: :R  IN @ >R [COMPILE] : R> IN !                              
HERE >CFA (WORD) FOUND IF CELL+ ! THEN ; IMMEDIATE             
FORWARD FAC                                                    
:R FAC   DUP 0= IF DROP 1 ELSE DUP 1 - FAC * THEN ;            
.S 4 FAC .S ." 4! IS " .                                       
                                                               
 38 LOAD         181 188 THRU                                  
                                                               
( BNF PARSER NON-IM COMPILING WORDS CALLED BY IM WORDS AH&CH)  
0 IVAR SUCCESS                                                 
: POP POSTPONE R> ;    : PUSH POSTPONE >R ;                    
: SUC@ POSTPONE  SUCCESS POSTPONE @ ;                          
: SUC! POSTPONE 1 POSTPONE SUCCESS POSTPONE ! ;                
: EXIT R> DROP ;     : % DSP@ H. TIB @ IN @ TYPE               
SPACE SUCCESS ? CR ; : % POSTPONE  % ;                         
 CODE  POPSP MOV, W| T| SP'| DB| [BP] 0 B,                     
 LEA, BP'| DB| [BP] 0 CELL+ B, NEXT C;                         
: <PTS POSTPONE DSP@ PUSH                                      
       POSTPONE DP POSTPONE @ PUSH                             
       POSTPONE IN POSTPONE @ PUSH ;                           
: BT> POP POSTPONE IN POSTPONE !                               
      POP POSTPONE DP POSTPONE !                               
      POSTPONE POPSP ;                                         
: PTS> POP POSTPONE DROP  POP POSTPONE DROP  POP               
POSTPONE DROP ;                                                
( compile-only words called by immediate words   )             
( Fake an embedded colon definition, i.e. an `EXIT' between    
  `<FAKE' and `FAKE>' must return after `FAKE>' )              
: <FAKE POSTPONE LIT HERE 0 , POSTPONE >R ;                    
: FAKE>  POSTPONE R> POSTPONE DROP  HERE SWAP ! ;              
                                                               
( Bracket an optional part, i.e. its success depends on what is
  before it. The part must balance the return stack. )         
: <OPT  SUC@ PUSH ;                                            
: OPT>  POP POSTPONE SUCCESS POSTPONE ! ;                      
                                                               
                                                               
                                                               
                                                               
                                                               
( hANDLING OF SINGLE CHARACTERS )                              
                                                               
: @TOKEN  IN @ TIB @ + C@ ;                                    
: +TOKEN  ( f ) IF 1 IN +! THEN ;                              
: =TOKEN ( n) SUCCESS @ IF @TOKEN = DUP SUCCESS ! +TOKEN       
ELSE       DROP THEN ;                                         
: TOKEN CREATE ( c) C, DOES> ( a) C@ =TOKEN ;                  
                                                               
1 WIDTH !                                                      
: '__  HERE 2 + C@ [COMPILE] LITERAL POSTPONE =TOKEN ;         
IMMEDIATE                                                      
31 WIDTH !                                                     
0 TOKEN <EOL>    $0A TOKEN 'CR'    BL TOKEN 'BL'               
                                                               
                                                               
                                                               
( bnf PARSER TOKENS                                     )      
( Skip blanks and handle comment )                             
: SKIP-BLANK TIB @ IN @ + BEGIN DUP C@ BL = WHILE              
  1+ 1 IN +! REPEAT DROP ;                                     
FORWARD skip  ( SKIP-BLANK is possible)                        
: +KEYWORD ( len f -) IF IN +! ELSE DROP THEN ;                
: =KEYWORD  ( VS -) SUCCESS @ IF $@ >R R                       
IN @ TIB @ + R@ $= DUP SUCCESS ! R> SWAP +KEYWORD              
ELSE DROP THEN ;                                               
: KEYWORD CREATE BL WORD HERE C@ 1+ ALLOT                      
DOES> skip =KEYWORD ;                                          
                                                               
                                                               
                                                               
                                                               
                                                               
( BNF PARSER                                   ch&ch )         
: `IF [COMPILE] IF ;            : `BEGIN [COMPILE] BEGIN ;     
: `ELSE [COMPILE] ELSE ;        : `WHILE [COMPILE] WHILE ;     
: `THEN [COMPILE] THEN ;        : `REPEAT [COMPILE] REPEAT ;   
: `EXIT POSTPONE EXIT ;                                        
                                                               
: <BNF  SUC@ `IF <PTS `ELSE `EXIT `THEN ;                      
: BNF>  SUC@ `IF PTS> `ELSE BT> `THEN ;                        
                                                               
( Embed a BNF definition in the current one, i.e.              
<<BNF ... BNF>>  is equivalent to xxx with xxx defined         
by BNF: xxx ... ;BNF )                                         
: <<BNF <FAKE <BNF ;                                           
: BNF>> BNF> FAKE> ;                                           
                                                               
                                                               
( THE { } round_bracket_pair and [ ] definitions )             
( Start a BNF definition, must have been declared by FORWARD ) 
: BNF:   [COMPILE] :R   <BNF   SUC!   ;                        
: ;BNF   BNF>   [COMPILE] ;   ; IMMEDIATE                      
                                                               
: | SUC@ `IF PTS> `EXIT `ELSE BT> <PTS SUC! `THEN ;            
IMMEDIATE                                                      
: (( <<BNF ;  IMMEDIATE                                        
: )) BNF>> ;  IMMEDIATE                                        
: [ <OPT <<BNF ;  IMMEDIATE                                    
: ] BNF>> OPT> ;  IMMEDIATE                                    
: {  <OPT `BEGIN SUC@ `WHILE <<BNF ; IMMEDIATE                 
: } BNF>> `REPEAT OPT> ; IMMEDIATE                             
                                                               
                                                               
                                                               
( Examples and tests )                                         
 : COMMENT  SKIP-BLANK  ;                                      
FORWARD AUX1 BNF: AUX1 'A' [ 'B' | 'C' ] ;BNF                  
: RUN[ 1 SUCCESS ! AUX1 SUCCESS ? ;                            
: RUN{ 1 SUCCESS ! 'A' { 'B' 'C' } SUCCESS ? ;                 
FORWARD <CHAR>                                                 
BNF: <CHAR> @TOKEN DUP [CHAR] ) = >R DUP [CHAR] ( = >R 0=      
   R> R> OR OR   0= DUP SUCCESS !      +TOKEN ;BNF             
( This requires an enormous return stack ! )                   
FORWARD <S>   BNF: <S>  '(' <S> ')' <S> | <CHAR> <S> | ;BNF    
FORWARD AUX3 BNF: AUX3 <S>  <EOL>  ;BNF                        
: RUN() 1 SUCCESS ! AUX3 SUCCESS @                             
IF -1 IN +! ." ok" ELSE ." NOK"  THEN ;                        
                                                               
FORWARD "KEY" BNF: "KEY" 'K' 'E' 'Y' ;BNF                      
: RUNKEY 1 SUCCESS ! "KEY" SUCCESS ? ;                         
                                                               
( Push the string matched by the current terminal symbol on    
 the data stack  bnf: iets  app nooot mies PUSH" ;bnf          
 pushes what is matched by iets )                              
: PUSH" R>   R@ TIB @ +   IN @ R@ -  ROT >R ;                  
( Add the string on the stack to the output )                  
: POP" >R HERE R@ CMOVE R> ALLOT BL C, ;                       
( Add the symbol matched to the output )                       
: .SYMBOL POSTPONE PUSH" POSTPONE POP" ; IMMEDIATE             
FORWARD STATEMENT    FORWARD IDENTIFIER                        
BNF: IDENTIFIER { 'A' | 'B' | 'C' } .SYMBOL ;BNF               
BNF: STATEMENT IDENTIFIER COMMENT '+' IDENTIFIER  " F+" POP"   
| '(' STATEMENT COMMENT ')'  ;BNF                              
: PS HERE 1 SUCCESS ! STATEMENT SUCCESS ?  HERE OVER - TYPE ;  
                                                               
                                                               
PS ABA + BABAA                                                 
." NOW COME THE FREAKS"                                        
( PS (A+B)     )                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Cludge to get things loadable on Linux )                     
: MODE ;    : BIOS ;   : BDOS ;                                
8 LOAD                                                         
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Up til LIMIT forget the vocabulary whose DEA is given. )     
(   Leave LIMIT    )                                           
: FORGET-VOC                                                   
  2DUP SWAP U< IF ( contains something)                        
      SWAP >R                                                  
      >DDEA  ( start with dictionary entry)                    
      DUP                                                      
 BEGIN CR DUP ID. >LFA  @  DUP  DUP H. R@  DUP H. U<  UNTIL    
        CR DUP H. DUP ID.                                      
          SWAP  >LFA !                                         
    R>                                                         
  ELSE       ( contains nothing)                               
     [COMPILE]  FORTH  DEFINITIONS                             
     >VFA @ VOC-LINK ! (  unlink)                              
  THEN ;                                                       
                                                               
( WORDS and VOCS )                                             
: WORDS ' ID. FOR-WORDS ;                                      
                                                               
: .VOC 2 CELLS - 2 - N>P ID. ;                                 
: VOCS ' .VOC >CFA FOR-VOCS ;                                  
                                                               
(   Up til LIMIT forget in all vocabularies.        )          
: FORGET [COMPILE] ' DUP H.                                    
    ' FORGET-VOC >CFA FOR-VOCS DROP ;                          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Test screens                                      )          
                                                               
VOCABULARY AAP                                                 
AAP                                                            
DEFINITIONS                                                    
: JAN ; : PIET ; : KLAAS ;                                     
                                                               
                                                               
AAP                                                            
 ' PIET   VOC-LINK @  FORGET-VOC                               
 ' TASK VOC-LINK @  FORGET-VOC                                 
                                                               
 ' .VOC >CFA FOR-VOCS                                          
FORTH                                                          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
             `                                                 
quityes899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES18
MES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 
1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRI
9 PRIMES1899                                                   
RIMES1899 PRI                                                  
ES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 PRIMES1899 P
899 PRIMES189                                                  
THE BYTE BENCHMARK LASTED  7.650mS OK                          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( tak )                                                        
VARIABLE X      VARIABLE Y      VARIABLE Z                     
                                                               
: tak                                                          
X @ Y @ Z @ >R >R >R                                           
Z ! Y ! X !                                                    
    X @ Y @ > 0= IF                                            
        Z @                                                    
    ELSE                                                       
        X @ 1 - Y @ Z @ RECURSE                                
        Y @ 1 - Z @ X @ RECURSE                                
        Z @ 1 - X @ Y @ RECURSE                                
        RECURSE                                                
    THEN                                                       
R> R> R> Z ! Y ! X !                                           
;                                                              
( tak , Using poor man's locals on the data stack)             
: Z "R@ @" EVALUATE ; IMMEDIATE                                
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE                          
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE                    
: tak                                                          
     DSP@  >R  \ X . Y . Z . CR                                
     X Y > 0= IF                                               
         Z                                                     
     ELSE                                                      
         X 1 - Y Z RECURSE                                     
         Y 1 - Z X RECURSE                                     
         Z 1 - X Y RECURSE                                     
         RECURSE                                               
     THEN                                                      
     RDROP \ Drop frame pointer.                               
     >R DROP DROP DROP R> ; \ Discard input, leave result      
( tak )                                                        
: Z "R@ @" EVALUATE ; IMMEDIATE                                
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE                          
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE                    
: tak SWAP ROT                                                 
     DSP@  >R  \ X . Y . Z . CR                                
     X Y > 0= IF                                               
         Z                                                     
     ELSE                                                      
         X 1 - Y Z SWAP ROT  RECURSE                           
         Y 1 - Z X SWAP ROT  RECURSE                           
         Z 1 - X Y SWAP ROT  RECURSE                           
         SWAP ROT  RECURSE                                     
     THEN   RDROP \ Drop frame pointer.                        
     >R DROP DROP DROP R> ; \ Discard input, leave result      
: tak SWAP ROT tak ;                                           
( tak )                                                        
: Z "R@ @" EVALUATE ; IMMEDIATE                                
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE                          
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE                    
: tak SWAP ROT                                                 
     DSP@  >R  \ X . Y . Z . CR                                
     X Y > 0= IF                                               
         Z                                                     
     ELSE                                                      
         Y X Z 1 - RECURSE                                     
         X Z Y 1 - RECURSE                                     
         Z Y X 1 - RECURSE                                     
         RECURSE                                               
     THEN   RDROP \ Drop frame pointer.                        
     >R DROP DROP DROP R> ; \ Discard input, leave result      
: tak SWAP ROT tak ;                                           
( tak )                                                        
: Z "R@ @" EVALUATE ; IMMEDIATE                                
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE                          
: X "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE                    
: tak SWAP ROT                                                 
     DSP@  >R  \ X . Y . Z . CR                                
     X Y > 0= IF                                               
         Z                                                     
     ELSE                                                      
         Y X Z 1 - RECURSE  >R DROP DROP DROP R>               
         X Z Y 1 - RECURSE  >R DROP DROP DROP R>               
         Z Y X 1 - RECURSE  >R DROP DROP DROP R>               
         RECURSE >R DROP DROP DROP R>                          
     THEN   RDROP \ Drop frame pointer.                        
; \ Discard input, leave result                                
: tak SWAP ROT tak >R DROP DROP DROP R> ;                      
( tak )                                                        
: X "R@ @" EVALUATE ; IMMEDIATE                                
: Y "R@ CELL+ @" EVALUATE ; IMMEDIATE                          
: Z "R@ CELL+ CELL+ @" EVALUATE ; IMMEDIATE                    
: tak                                                          
     2DUP < 0= IF                                              
         >R >R DUP R> SWAP R> SWAP                             
     ELSE                                                      
         DSP@  >R  \ X . Y . Z . CR                            
         Y X Z 1 - RECURSE  >R DROP DROP DROP R>               
         X Z Y 1 - RECURSE  >R DROP DROP DROP R>               
         Z Y X 1 - RECURSE  >R DROP DROP DROP R>               
         RECURSE >R DROP DROP DROP R>                          
         RDROP \ Drop frame pointer.                           
     THEN ;                                                    
: tak SWAP ROT tak >R DROP DROP DROP R> ;                      
( tak )                                                        
: kat                                                          
    2DUP < 0= IF                                               
         >R >R DUP R> SWAP R> SWAP                             
    ELSE                                                       
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         R> R> R> SWAP ROT                                     
         RECURSE                                               
        >R DROP DROP DROP R>                                   
    THEN ;                                                     
: tak SWAP ROT kat >R DROP DROP DROP R> ;                      
                                                               
                                                               
                                                               
( tak, Look mother! Only stacks and still comprehensible)      
: 2PICK  ">R >R DUP R> SWAP R> SWAP" EVALUATE ; IMMEDIATE      
: tak ; ( Forward)                                             
: kat                                                          
    2DUP < 0= IF 2PICK                                         
    ELSE                                                       
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         R> R> R> tak                                          
    THEN ;                                                     
: tak' SWAP ROT kat >R DROP DROP DROP R> ;                     
' tak' >DFA @ ' tak >DFA ! ( solve forward reference)          
                                                               
: q MARK-TIME 18 12 6 tak . ELAPSED ;                          
                                                               
( tak, Look mother! Only stacks and still comprehensible)      
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)  
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)          
: tak SWAP ROT kat' >R DROP DROP DROP R> ( 3 NIP's ) ;         
\ Like ``tak'' but arguments in reverse order and not consumed 
: kat                                                          
    2DUP < IF                                                  
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         ROT 1 - RECURSE  >R 1+                                
         R> R> R> tak                                          
    ELSE                                                       
        >R >R DUP R> SWAP R> SWAP \ 2 PICK                     
    THEN ;                                                     
' kat >DFA @ ' kat' >DFA ! ( solve forward reference)          
: q MARK-TIME 18 12 6 tak . ELAPSED ;                          
( tak, Look mother! Only stacks, already less comprehensible)  
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)  
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)          
: tak SWAP ROT 1+ kat' >R DROP DROP DROP R> ( 3 NIP's ) ;      
\ For Z Y X   --- return Z Y X tak(X-1,Y,Z)                    
: kat                                                          
   1-                                                          
   2DUP < IF                                                   
       ROT RECURSE >R   ROT RECURSE >R   ROT RECURSE >R        
       1+                                                      
       R> R> R> tak                                            
   ELSE  1+                                                    
       >R >R DUP R> SWAP R> SWAP \ 2 PICK                      
   THEN ;                                                      
' kat >DFA @ ' kat' >DFA ! ( solve forward reference)          
: q MARK-TIME 18 12 6 tak . ELAPSED ;                          
( tak, Look mother! Only stacks, less and less comprehensible) 
: kat' ;  ( Forward definition of ``kat''. Mutual recursion!)  
\ For X Y Z return the VALUE of take-uchi: tak(X,Y,Z)          
: tak SWAP ROT 1+ kat' >R DROP DROP DROP R> ( 3 NIP's ) ;      
\ For Z Y X   --- return Z Y X tak(X-1,Y,Z)                    
: kat                                                          
   2DUP 1- < IF                                                
       1-                                                      
       ROT RECURSE >R   ROT RECURSE >R   ROT RECURSE >R        
       1+                                                      
       R> R> R> tak                                            
   ELSE                                                        
       >R >R DUP R> SWAP R> SWAP \ 2 PICK                      
   THEN ;                                                      
' kat >DFA @ ' kat' >DFA ! ( solve forward reference)          
: q MARK-TIME 18 12 6 tak . ELAPSED ;                          
( tak, using 3SWAP NIP and PICK doesn't run on ciforth)        
: PICK 1+ CELLS DSP@ + @ ;                                     
: NIP SWAP DROP ;                                              
: 3SWAP SWAP ROT ; \ Reverse top 3 elements.                   
: tak' ;  ( Forward reference)                                 
\ Auxiliary: For Z Y X   --- return Z Y X tak(X-1,Y,Z)         
: kat                                                          
   2DUP 1- < IF                                                
       1-  ROT RECURSE >R  ROT RECURSE >R  ROT RECURSE >R  1+  
       R> R> R> tak'                                           
   ELSE                                                        
       2 PICK                                                  
   THEN ;                                                      
: tak 3SWAP 1+ kat NIP NIP NIP ;                               
' tak >DFA @ ' tak' >DFA ! ( Solve forward reference)          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Mini editor by retyping, Usage ME) <HEX                      
: EL LINE C/L 1 - BLANK ;                                      
: GL PAD C/L  ACCEPT C/L 1- MIN >R LINE PAD SWAP R> MOVE ;     
: OEPS SCR @ LIST "PROCEED?" $. KEY 20 OR                      
  &y <> 2000 ?ERROR  "GO" $. CR ;                              
: ME SCR ! OEPS 10 0 DO I EL I GL LOOP ;                       
                                                               
HEX>                                                           
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Binary search, comment ) EXIT                                
( BIN-SEARCH    : n IMIN, n IMAX, xt COMP -- n IRES )          
Uses a comparison routine with execution token `COMP' `COMP'   
must have the stack diagram ( IT -- flag) , where flag         
typically means that IT compares lower or equal to some fixed  
value. It should be TRUE for `IMIN' and decreasing in between  
`IMIN' and `IMAX' . Finds the last index `IT' between `IMIN'   
and `IMAX' (inclusive) for which `COMP' returns true.          
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( BIN-SEARCH    : n IMIN, n IMAX, xt COMP -- n IRES )          
VARIABLE IMIN  \ IMIN ' COMP EXECUTE is always TRUE            
VARIABLE IMAX  \ IX ' COMP EXECUTE is always FALSE for IX>IMAX 
VARIABLE COMP \ Execution token of comparison word.            
: BIN-SEARCH    COMP !  IMAX ! IMIN !                          
    BEGIN       \ Loop variant IMAX - IMIN                     
        IMIN @ IMAX @ .S <> WHILE                              
        IMAX @ IMIN @ + 1+ 2 /   ( -- ihalf )                  
        DUP COMP @ EXECUTE IF                                  
           ( ihalf) IMIN !                                     
        ELSE                                                   
           ( ihalf) 1- IMAX !                                  
        THEN                                                   
    REPEAT                                                     
IMIN @ ;                                                       
\  HIDE IMIN   HIDE IMAX   HIDE COMP                           
( Binary search, comment, Test )                               
: <100 100 < ;  -1000 +1000 ' <100 BIN-SEARCH                  
." EXPECT 99:" .                                               
CREATE XXX 123 , 64 , 32 , 12                                  
\ Find first number < 40                                       
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( Solution)                                                    
: CC CELLS XXX + @ 40 < 0= ;                                   
0 3 ' CC BIN-SEARCH 1+ CELLS XXX + @                           
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( BIN-SEARCH    : n IMIN, n IMAX, xt COMP -- n IRES )          
VARIABLE IMIN  \ IMIN ' COMP EXECUTE is always TRUE            
VARIABLE IMAX  \ IX ' COMP EXECUTE is always FALSE for IX>IMAX 
VARIABLE COMP \ Execution token of comparison word.            
: BIN-SEARCH    COMP !  IMAX ! IMIN !                          
    BEGIN       \ Loop variant IMAX - IMIN                     
        IMIN @ IMAX @ <> WHILE                                 
        IMAX @ IMIN @ + 1+ 2 /   ( -- ihalf )                  
        DUP COMP @ EXECUTE IF                                  
           ( ihalf) IMIN !                                     
        ELSE                                                   
           ( ihalf) 1- IMAX !                                  
        THEN                                                   
    REPEAT                                                     
IMIN @ ;                                                       
\  HIDE IMIN   HIDE IMAX   HIDE COMP                           
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
123 THROW                                                      
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( This has the effect as ?ERROR )                              
( But counting back from 100 )                                 
: LINUX-ERROR 100 OVER - ?ERROR ;                              
: IOCTL 54 LINOS LINUX-ERROR ;                                 
0 IVAR TERMIO 60 ALLOT                                         
HEX 5401 CONSTANT TCGETS                                       
HEX 5402 CONSTANT TCSETS                                       
8 2 OR CONSTANT RAWIO                                          
: getit 0 TCGETS TERMIO IOCTL ;                                
: setit 0 TCSETS TERMIO IOCTL ;                                
( Set the terminal length to len and toggle the )              
( raw byte with b )  ( len b -- )                              
: tc TERMIO 3 CELLS + SWAP TOGGLE                              
    TERMIO 4 CELLS + 1 + 6 + C!                                
     setit ;                                                   
DECIMAL  getit                                                 
3 CONSTANT read                                                
( expect one key and retain it.)                               
: KEY2 1 RAWIO tc                                              
     0 DSP@                                                    
    0 SWAP 1 read LINOS DROP                                   
     1 RAWIO tc                                                
;                                                              
( expect zero keys and retain the count.)                      
: KEY?2                                                        
    0 RAWIO tc                                                 
    0 DSP@                                                     
    0 SWAP 1 read LINOS SWAP DROP                              
    1 RAWIO tc                                                 
;                                                              
                                                               
                                                               
( Trying to implement SYSTEM )                                 
: CV OVER + 1 - 0 SWAP C! ;                                    
  CREATE COMMAND 256 ALLOT                                     
  "-c " CV "sh " CV CREATE ARG[] , , COMMAND CELL+ , 0 ,       
"/bin/sh " CV CONSTANT SHELL                                   
: SYSTEM COMMAND $! 0 COMMAND $C+                              
     0 0 0 2 LINOS DUP ?LINUX-ERROR                            
     0= IF SHELL ARG[] ENV 11 LINOS ?LINUX-ERROR THEN          
;                                                              
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
                                                               
( CATCH and THROW)                                             
 VARIABLE HANDLER                                              
: CATCH                                                        
    DSP@ CELL+ >R                                              
\    SAVE                                                      
    HANDLER @ >R                                               
    RSP@ HANDLER !                                             
    EXECUTE                                                    
    R> HANDLER !                                               
\    RDROP RDROP RDROP                                         
    RDROP 0 ;                                                  
                                                               
                                                               
                                                               
                                                               
                                                               
( CATCH and THROW)                                             
: THROW                                                        
?DUP IF                                                        
   HANDLER @ 0= IF ERROR THEN                                  
   HANDLER @ RSP!                                              
   R> HANDLER !                                                
\   RESTORE                                                    
   R> SWAP >R ( Keep throw code)                               
   DSP!                                                        
   R>                                                          
   THEN ;                                                      
: T12 12 THROW ;                                               
                                                               
                                                               
                                                               
                                                               
( A reverse engineering BLK and SOURCE-ID )                    
: BLK'                                                         
    IN @ FIRST LIMIT WITHIN                                    
    SRC 2@ - 1024 = AND                                        
    IF SRC @ 2 CELLS - @ ELSE 0 THEN                           
    BLK !                                                      
    BLK                                                        
;   BLK' ?   " BLK' ?" EVALUATE                                
: SOURCE-ID                                                    
   SRC @                                                       
   DUP TIB @ = IF DROP 0 ELSE                                  
   DUP 7 - "FiLeBuF" CORA IF ( Leave it) ELSE                  
   DROP -1 THEN THEN ;                                         
SOURCE-ID ? "SOURCE-ID ?" EVALUATE                             
                                                               
( Last line, preserve !! Must be line 4096)                    
