

FORWARD array-type 
FORWARD atom
FORWARD block 
FORWARD compound-statement 
FORWARD constant-definition
FORWARD constant-definition-part
FORWARD expression
FORWARD file-type 
FORWARD identifier-list
FORWARD formal-parameters 
FORWARD formal-parameter-list 
FORWARD function-declaration-part  
FORWARD function-heading   
FORWARD label-declaration-part
FORWARD list
FORWARD main-program-declaration
FORWARD number
FORWARD operator
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
FORWARD set-type 
FORWARD structured-type 
FORWARD type-definition
FORWARD type-definition-part
FORWARD type-denoter   
FORWARD variable-declaration
FORWARD variable-declaration-part  



\ An atom is a constant, a variable (accessed) or a function (called)
\ The dot_name determines which of the three. list only with function.
BNF: atom 
    identifier [ '(' list ')' ]    
    { `[' expression { `,' expression } `]' 
       | `.' identifier 
       | `[' expression `..' expression `]' 
       | `^' 
     } ;BNF
BNF: array-type `array' `[' ordinal-type { `;' ordinal-type } `]' `of' type-denoter ;BNF

BNF: block { label-declaration-part | constant-definition-part | type-definition-part | variable-declaration-part 
        | function-declaration-part } 
        compound-statement ;BNF

BNF: constant-definition   identifier `=' expression ;BNF
BNF: constant-definition-part   `const' constant-definition `;' { constant-definition `;' } ;BNF
BNF: expression   `(' expression   `)' | atom { operator atom } | number ;BNF
BNF: file-type     0 SUCCESS ! ;BNF    \ CAN'T DEAL BEHIND THAT
BNF: formal-parameter-list [ `var' | `function' | `procedure' | ] identifier-list [ `:' identifier ] ;BNF  
BNF: formal-parameters `(' formal-parameter-list { `;' formal-parameter-list } `)' ;BNF
BNF: function-declaration-part   function-heading block `;' ;BNF
BNF: function-heading   [ `function' | `procedure' ] identifier [ formal-parameters ] [ `:' identifier ] `;' ;BNF
BNF: identifier-list   identifier { `,' identifier } ;BNF
BNF: label-declaration-part   `label' digit-sequence { `,' digit-sequence } `;' ;BNF
BNF: list   expression { `,' expression } ;BNF
BNF: number [ `+' | `-' ] digit-sequence [ `.' digit-sequence ] [ `e' digit-sequence ] ;BNF
BNF: operator operator-adding | operator-exponentiating | operator-multiplying | operator-relational ;BNF
BNF: operator-adding   `+' | `-' ;BNF
BNF: operator-exponentiating   `**' | `pow' ;BNF
BNF: operator-multiplying   `*' | `/' | `div' | `mod' | `and' ;BNF
BNF: operator-relational   `=' | `<>' | `<' | `<=' | `>=' | `>' | `in' ;BNF
BNF: ordinal-type identifier | `(' identifier-list `)' | expression `..' expression ;BNF
BNF: program-heading `program' identifier [ `(' identifier-list `)' ] ;BNF
BNF: program program-heading `;' block ;BNF
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


