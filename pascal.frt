

FORWARD array-type 
FORWARD atom
FORWARD block 
FORWARD case-statement   
FORWARD case-list-element
FORWARD compound-statement 
FORWARD constant-definition
FORWARD constant-definition-part
FORWARD constituent 
FORWARD dyadic
FORWARD expression
FORWARD expression-list
FORWARD file-type 
FORWARD identifier-list
FORWARD if-statement
FORWARD for-statement 
FORWARD formal-parameters 
FORWARD formal-parameter-list 
FORWARD function-declaration-part  
FORWARD function-heading   
FORWARD label-declaration-part
FORWARD list
FORWARD main-program-declaration
FORWARD monadic
FORWARD number
FORWARD operator-adding   
FORWARD operator-exponentiating   
FORWARD operator-multiplying   
FORWARD operator-relational   
FORWARD ordinal-type 
FORWARD pointer-type 
FORWARD program
FORWARD program-component
FORWARD program-heading
FORWARD record-type
FORWARD repeat-statement
FORWARD set-type 
FORWARD simple-statement 
FORWARD skip-keyword 
FORWARD statement   
FORWARD statement-sequence   
FORWARD structured-statement 
FORWARD structured-type 
FORWARD type-definition
FORWARD type-definition-part
FORWARD type-denoter   
FORWARD variable-declaration
FORWARD variable-declaration-part  
FORWARD while-statement 

\ In this bnf the order where in alternatives are states is crucial for the
\ automatic translation. 

\ An atom is a constant, a variable (accessed) or a function (called)
\ The identifier determines which of the three. list only with function.
BNF: atom  
    identifier [ `(' list `)' ]    
    { `[' expression { `,' expression } `]' 
       | `.' identifier 
       | `[' expression `..' expression `]' 
       | `^' 
     } ;BNF
BNF: array-type `array' `[' ordinal-type { `;' ordinal-type } `]' `of' type-denoter ;BNF

BNF: block { label-declaration-part | constant-definition-part | type-definition-part | variable-declaration-part | function-declaration-part } compound-statement ;BNF
BNF: case-statement   `case' expression `of' case-list-element { `;' case-list-element } `end' ;BNF
BNF: case-list-element   expression-list `:' statement | ;BNF
BNF: compound-statement   `begin' statement-sequence `end' ;BNF      
BNF: constant-definition   identifier `=' expression ;BNF
BNF: constant-definition-part   `const' constant-definition `;' { constant-definition `;' } ;BNF
BNF: dyadic operator-adding | operator-exponentiating | operator-multiplying | operator-relational ;BNF
BNF: constituent character-string | `(' expression `)' | atom | number ;BNF
BNF: expression   [ monadic ] constituent { dyadic expression } ;BNF
BNF: expression-list   expression { `,' expression } ;BNF
BNF: file-type     0 SUCCESS ! ;BNF    \ CAN'T DEAL BEHIND THAT
BNF: for-statement   `for' identifier ( `:=' expression `downto' | `:=' expression `to' | `in' ) expression `do' statement ;BNF
BNF: formal-parameter-list [ `var' | `function' | `procedure' | ] identifier-list [ `:' identifier ] ;BNF  
BNF: formal-parameters `(' formal-parameter-list { `;' formal-parameter-list } `)' ;BNF
BNF: function-declaration-part   function-heading block `;' ;BNF
BNF: function-heading   ( `function' | `procedure' ) identifier [ formal-parameters ] [ `:' identifier ] `;' ;BNF
BNF: identifier-list   identifier { `,' identifier } ;BNF
BNF: if-statement   `if' expression `then' statement [ `else' statement ] ;BNF
BNF: label   skip digit-sequence ;BNF
BNF: label-declaration-part   `label' label { `,' label } `;' ;BNF
BNF: list   expression { `,' expression } ;BNF
BNF: monadic `+' | `-' | `not' ;BNF
BNF: number skip [ `+' | `-' ] digit-sequence [ `.' digit-sequence ] [ `e' digit-sequence ] ;BNF
BNF: operator-adding   `+' | `-' | `or' ;BNF
BNF: operator-exponentiating   `**' | `pow' ;BNF
BNF: operator-multiplying   `*' | `/' | `div' | `mod' | `and' ;BNF
BNF: operator-relational   `<>' | `<=' | `>=' | `<' | `>' | `=' | `in' ;BNF
BNF: ordinal-type identifier | `(' identifier-list `)' | expression `..' expression ;BNF
BNF: program-heading `program' identifier [ `(' identifier-list `)' ] ;BNF
BNF: program program-heading `;' block `.' ;BNF
\ Here an atom must be a function call. For an assigment types must be compatible.
BNF: repeat-statement   `repeat' statement-sequence `until' expression ;BNF
BNF: while-statement   `while' expression `do' statement ;BNF
BNF: simple-statement    atom [ `:=' expression ] | `goto' label ;BNF
\  "no-keyword" matches everything except keywords that can start a statement, 
\ but doesn't advance the parse pointer. 
\ This is the only way to convince "atom" that "else" is not a call to the procedure "else" 
BNF: no-keyword ( `begin' | `end' | `if' | `then' | `else' | `while' | `repeat' | `do' | `until' | `to' | `downto' ) SUCCESS @ 0= SUCCESS ! ;BNF
BNF: statement   [ label `:' ] ( structured-statement | no-keyword simple-statement | ) ;BNF
BNF: statement-sequence   statement { `;' statement } ;BNF
BNF: structured-statement compound-statement | if-statement | case-statement | repeat-statement | while-statement | for-statement ;BNF    
BNF: type-definition   identifier `=' type-denoter ;BNF
BNF: type-definition-part   `type' type-definition { `;' type-definition } `;' ;BNF
BNF: type-denoter   structured-type | pointer-type | ordinal-type ;BNF
BNF: structured-type [ `packed' ] array-type | record-type | set-type | file-type ;BNF
BNF: pointer-type  0 SUCCESS ! ;BNF    \ CAN'T DEAL BEHIND THAT
BNF: record-type   0 SUCCESS ! ;BNF    \ CAN'T DEAL BEHIND THAT
BNF: set-type      0 SUCCESS ! ;BNF    \ CAN'T DEAL BEHIND THAT
BNF: variable-declaration   identifier-list `:' type-denoter ;BNF
BNF: variable-declaration-part   `var' variable-declaration `;' { variable-declaration `;' } ;BNF





0 VARIABLE SAVE-TIB
0 VARIABLE SAVE-IN
: PASCAL   
    TIB @ SAVE-TIB !   
    IN @ SAVE-IN !   
    GET-FILE 0 IN !
    FILE-BUFFER TIB ! 0 IN ! 
    1 SUCCESS ! program ." AFTER PARSING :" SUCCESS ?
    SAVE-IN @ IN ! 
    SAVE-TIB @ TIB ! 
;


