\  takeuchi benchmark in Forth (ciforth)
\  see <url:http://www.lib.uchicago.edu/keith/crisis/benchmarks/tak/>
\  Tod Olson <ta-olson@uchicago.edu>
\
REQUIRE ARGC
REQUIRE ARG[]

\ Print an error message and return to the OS
\ In good Unix style, the name fo the program and
\ usage is printed.
: MY-ERROR
    " : FATAL! " TYPE CR
    "Usage: " TYPE CR
    0 ARG[] TYPE " <integer1> <integer2> <integer3>" TYPE CR
    BYE ;

\ Install it as the handler for uncaught exceptions.
0 WARNING !     'MY-ERROR 'ERROR 3 CELLS MOVE

\ Wel here you have it.
\ The example program with poor mans recursion
VARIABLE X      VARIABLE Y      VARIABLE Z
: <= > 0= ;
: tak
    >R 2DUP <= IF
        2DROP R>
    ELSE R>
        X @ Y @ Z @ >R >R >R
            Z ! Y ! X !
            X @ 1 - Y @ Z @ RECURSE
            Y @ 1 - Z @ X @ RECURSE
            Z @ 1 - X @ Y @ RECURSE
            RECURSE
        R> X ! R> Y ! R> Z !
    THEN
;

\ Return command line argument number INDEX as a DOUBLE
: GET-ARGUMENT-NUMBER
    >R 0. R@ ARG[] DROP 23    \ Prepare arg for >NUMBER
    >NUMBER  DROP
    \ If the inconvertable digit was not the end of the
    \ c-string, we have a trouble spot.
    C@ IF
        "ARG " TYPE R@ . "NOT A CORRECT NUMBER" TYPE MY-ERROR
    THEN
    RDROP
;

\ Get PAR1 PAR2 PAR3 :
\    the arguments for uchi from the command line
: get-args
    ARGC 4 <> IF "INCORRECT NUMBER OF ARGUMENTS" TYPE MY-ERROR THEN
    4 1 DO
        I GET-ARGUMENT-NUMBER
        DROP \ single number
    LOOP
;

\ Guess what? The main program.
: main get-args tak . CR BYE ;
