COPYRIGHT (c) 2000-2016 Albert van der Horst, THE NETHERLANDS
                   LICENSE
This program is free software; you can redistribute it and/or
modify it under the terms of version 2 of the GNU General
Public License as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with this program; if not, write to the
            Free Software Foundation, Inc.,
   59 Temple Place, Suite 330, Boston, MA 02111, USA.

( -a :_Make_want_available_silently )           \ AvdH B0nov25
( WANT WANTED CF: ) \ AvdH A7feb24
CREATE _pad 80 ALLOT \ Word surrounded by spaces
: FILL-pad   ( sc --) " " _pad $!   _pad $+!   " " _pad $+! ;
\  For LINE : return the POSITION of the word at ``_pad''.
: POSITION   _pad @ - 0 MAX OVER + SWAP
  DO   I _pad $@ CORA   0= IF I UNLOOP EXIT THEN   LOOP   0 ;
\ Find WORD in blocks N and up. Leave the BLOCK or throw.
: #LOCATED  >R FILL-pad R> BEGIN 0 OVER (LINE) -TRAILING
    DUP 0= 24 AND THROW POSITION 0= WHILE 1+ REPEAT   ;
: (WANTED)   ( sc -- sc ) ERRSCR @ 4 + >R   BEGIN 2DUP PRESENT
  0= WHILE 2DUP R> #LOCATED DUP 1+ >R  LOAD REPEAT RDROP ;
: WANTED   ( Make WORD available. ) '(WANTED) CATCH DUP 24 = IF
    >R ETYPE R> MESSAGE ELSE THROW 2DROP THEN ;
: WANT  ^J PARSE SAVE SET-SRC BEGIN NAME DUP WHILE WANTED
    REPEAT 2DROP RESTORE ;      : CF: "CONFIG" WANTED ;
( -b :_This_option_is_available )














\
( -c PROGRAM :_compile_PROGRAM_to_binary ) \ AvdH B7mar28
1 LOAD   WANT OLD:   TURNKEY   SWAP-DP
WANT ARG[]   INCLUDE   SRC>EXEC
: MY-ERROR  DUP EXIT-CODE ! OLD: ERROR BYE ;
'MY-ERROR DUP 'ERROR 3 CELLS MOVE  HIDDEN
\ Don't include source in executable.
: INCD'   SWAP-DP GET-FILE SWAP-DP EVALUATE ;
'INCD' DUP 'INCLUDED 3 CELLS MOVE   HIDDEN

'TASK 'OPTIONS 3 CELLS MOVE \ No options.
'TASK '.SIGNON 3 CELLS MOVE \ No sign on.
: REGRESS  POSTPONE \ ; IMMEDIATE \ Turn off regression test
ARGC 3 < 13 ?ERROR
2 ARG[] INCLUDED
LATEST   2 ARG[] SRC>EXEC   TURNKEY

( -d :_This_option_is_available )














\
( -e :_Load_system_electives ) \ AvdH A3sep01
.SIGNON CR 0 MESSAGE CR 0 BLOCK  B/BUF TYPE
1 LOAD
\  "-legacy-" WANTED  \ If you want this, it must be up front
WANT CONFIG HELP ORDER
WANT L-S DO-DEBUG H. DUMP
WANT  $.
WANT INCLUDE SEE  LOCATE
 "CASE-INSENSITIVE" WANTED            CASE-INSENSITIVE
\ WANT EDITOR      \ WANT OOPS     \ WANT FARDUMP
\  "ASSEMBLERi86" WANTED \  "DEADBEEF" WANTED
\ Select os-interface DOS/Unix
 "OS-IMPORT" WANTED  (  "DIR" WANTED )      "ls" WANTED
 "-scripting-" WANTED
 "AUTOLOAD" WANTED
: TASK ; OK
( -f :_Forth_words_to_be_executed_80_chars) \ AvdH A1oct05
1 LOAD
 "ARG[]" WANTED    "Z$@" WANTED
CREATE COMMAND-BUFFER 0 , 1000 ALLOT
: DOIT
    BEGIN SHIFT-ARGS ARGC 1 > WHILE
    1 ARG[] COMMAND-BUFFER $+!   BL COMMAND-BUFFER $C+
    REPEAT ;
DOIT    COMMAND-BUFFER $@
\ 'DOIT HIDDEN   COMMAND-BUFFER HIDDEN
2DUP TYPE EVALUATE




\
( -g GROW :_grow_by_megabytes ) \ AvdH B4may28
'FORTH >WID >LFA @    HERE CONSTANT HERE-NOW
1 LOAD       "ARG[]" WANTED  CF: ?LI
ARGC 4 <> 13 ?ERROR     "SAVE-SYSTEM" WANTED
SM HERE-NOW OVER - $, CONSTANT thisforth$
 2 ARG[] EVALUATE 20 LSHIFT CONSTANT INCREASE
 thisforth$ CELL+ SM - CONSTANT >TARGET
: PATCH   >TARGET + INCREASE SWAP +! ;
: PATCH-CONSTANT   >DFA PATCH ;
: PATCH-ALL   5 0 DO I CELLS +ORIGIN PATCH LOOP
  _PREV PATCH   G-SIZE PATCH
  'EM PATCH-CONSTANT   '_FIRST PATCH-CONSTANT
  '_LIMIT PATCH-CONSTANT  ;
: GROW   'FORTH >WID >LFA >TARGET + ! PATCH-ALL
thisforth$ $@ 3 ARG[]   PUT-FILE ;
GROW BYE
( -h :_help,_show_options ) \ AvdH B6feb26
.SIGNON CR   0 MESSAGE
0 BLOCK  B/BUF TYPE
: _indices   DO 0 I (LINE)   BL $/ 2DROP   &) $/ 2SWAP 2DROP
  BEGIN 2DUP &_ $^ DUP WHILE BL SWAP C! REPEAT   DROP
  TYPE CR LOOP ;
^Z 1+ 1 _indices CR
.( See also the pdf documentation ) CR
.( or print the PostScript documentation ) CR

OK BYE




\
( -i BINARY-PATH LIBRARY-PATH SHELL-PATH )\ A3aug30
CREATE task     1 LOAD
    "ARG[]" WANTED    "SAVE-SYSTEM" WANTED
: INSTALL-LIB BLOCK-FILE $@ GET-FILE   3 ARG[] PUT-FILE
    3 ARG[] BLOCK-FILE $! ;
\ Trim back to before ``task''. Save system at binary path.
\ Must be done all at once, because of forgetting.
: INSTALL-BIN 'task    DUP 'FORTH FORGET-VOC   >NFA @ DP !
    2 ARG[] SAVE-SYSTEM  BYE ;
\ Specify shell name.
: INSTALL-SHELL 4 ARG[] SHELL $! ;
: DOIT   ARGC 4 6 WITHIN 0=
    IF ." -i requires 2 or 3 arguments" CR BYE THEN
    ARGC 5 = IF INSTALL-SHELL THEN   INSTALL-LIB INSTALL-BIN ;
DOIT
\
( -j :_This_option_is_available )














\
( -k :_This_option_is_available )














\
( -l LIBRARY :_LIBRARY_to_be_used_for_blocks ) \ AvdH A1oct05
CREATE task
1 LOAD    "SHIFT-ARGS" WANTED    "ARG[]" WANTED
\ Install other library
: SWITCH-LIBS   BLOCK-EXIT
    2 ARG[] BLOCK-FILE $!
    2 BLOCK-INIT
    SHIFT-ARGS   SHIFT-ARGS
    'task 'FORTH FORGET-VOC COLD ;

\ Must all be done in one go!
SWITCH-LIBS



\
( -m/--/--help/--version :_Help_and_version_information )
1 31 INDEX   OK         \ Help
22 LOAD












\
( -n :_This_option_is_available )














\
( -o :_This_option_is_available )














\
( -p :_Load_system_preferences ) \ AvdH A1oct02
1 LOAD
 "-legacy-" WANTED     \ Must be first to WANT












\
( -q :_This_option_is_available )














\
( -r :_This_option_is_available )














\
( -s SCRIPT-FILE :_Interpret_SCRIPT-FILE ) \ AvdH A1oct02
1 LOAD    "OLD:" WANTED    "ARG[]" WANTED
 "CTYPE" WANTED   2 ARG[] $, CONSTANT SCRIPT-NAME

: BY-WHO   "XOS" PRESENT IF " run by " TYPE
0 ARG[] TYPE THEN ;
\ This error handler may be overwritten by the script.
: MY-ERROR    OLD: ERROR
      "Fatal error in file :" TYPE SCRIPT-NAME $@ TYPE CR
    BY-WHO CR BYE ;
-1 WARNING !     'MY-ERROR >DFA @     'ERROR >DFA !
SHIFT-ARGS  SHIFT-ARGS
SCRIPT-NAME $@ GET-FILE   "-scripting-" WANTED
^J $/ 2DROP     \ Line with #!lina
EVALUATE
BYE
( -t FILE :_Try_to_compile_FILE_by_all_means ) \ AvdH A1oct26
.SIGNON 1 LOAD
\ Reload  "with" WANTED new ``CORA'' but hide it direct after.
 "CORA-IGNORE" WANTED
: CORA CORA-IGNORE ;   1 LOAD   'CORA HIDDEN

 "[IF]" WANTED    "ARG[]" WANTED    "PREFIX" WANTED

 "CASE-INSENSITIVE" WANTED   CASE-INSENSITIVE
 "NO-SECURITY" WANTED        NO-SECURITY
 "AUTOLOAD" WANTED           AUTOLOAD
ARGC 3 < 13 ?ERROR
2 ARG[] INCLUDED
SECOND-PASS @ 0= ?LEAVE-BLOCK
2 ARG[] INCLUDED
\
( -u :_This_option_is_available )














\
( -v :_Version_and_copyright_information_)
"CPU  NAME  VERSION" TYPE .SIGNON CR
"LIBRARY FILE: " TYPE
0 MESSAGE
"$RCSfile$ $Revision$" TYPE CR
CR
0 BLOCK  B/BUF TYPE
BYE







\
( -w :_Make_want_available                     ) \ AvdH B2sep22
.SIGNON   1 LOAD   OK













\
( -x :_This_option_is_available )














\
( -y :_This_option_is_available )














\
( -z :_This_option_is_available )














\
(    :_This_option_is_available )














\
(    :_This_option_is_available )














\
(    :_This_option_is_available )














\
( -  :_This_option_is_available )














\
( -? :_This_option_is_available )

8 LOAD












\
