

FORWARD atom
FORWARD block 
FORWARD compound-statement 
FORWARD constant-definition
FORWARD constant-definition-part
FORWARD expression
FORWARD identifier-list
FORWARD function-declaration-part  
FORWARD label-declaration-part
FORWARD list
FORWARD main-program-declaration
FORWARD number
FORWARD operator
FORWARD operator-adding   
FORWARD operator-exponentiating   
FORWARD operator-multiplying   
FORWARD operator-relational   
FORWARD program
FORWARD program-component
FORWARD program-heading
FORWARD type-definition-part
FORWARD variable-declaration-part  



\ An atom is a constant, a variable (accessed) or a function (called)
\ The dot_name determines which of the three. list only with function.
BNF: atom 
    identifier [ list ] 
    { `[' expression { `,' expression } `]' 
       | `.' identifier 
       | `[' expression `..' expression `]' 
       | `^' 
     } ;BNF

BNF: block { label-declaration-part 
| constant-definition-part 
| type-definition-part 
        | variable-declaration-part 
        | function-declaration-part } 
        compound-statement ;BNF

BNF: constant-definition   identifier `=' expression ;BNF
BNF: constant-definition-part   `const' constant-definition `;' { constant-definition `;' } ;BNF
BNF: expression   `(' expression   `)' | atom { operator atom } | number ;BNF
BNF: identifier-list   identifier { `,' identifier } ;BNF
BNF: label-declaration-part   `label' digit-sequence { `,' digit-sequence } `;' ;BNF
BNF: number [ `+' | `-' ] digit-sequence [ `.' digit-sequence ] [ `e' digit-sequence ] ;BNF
BNF: operator operator-adding | operator-exponentiating | operator-multiplying | operator-relational ;BNF
BNF: operator-adding   `+' | `-' ;BNF
BNF: operator-exponentiating   `**' | `pow' ;BNF
BNF: operator-multiplying   `*' | `/' | `div' | `mod' | `and' ;BNF
BNF: operator-relational   `=' | `<>' | `<' | `<=' | `>=' | `>' | `in' ;BNF
BNF: program-heading `program' identifier [ `(' identifier-list `)' ] ;BNF
BNF: program program-heading `;' block ;BNF


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


