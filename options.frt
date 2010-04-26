COPYRIGHT (c) 2000-2009 Albert van der Horst, THE NETHERLANDS
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

( -a :_Make_require_available_silently ) \ AvdH B0jan12
( WANT WANTED CF: ) \ AvdH A7feb24
CREATE _pad 80 ALLOT \ Word surrounded by spaces
: FILL-pad   ( sc --) " " _pad $!   _pad $+!   " " _pad $+! ;
\  For LINE : return the POSITION of the word at ``_pad''.
: POSITION   _pad @ - 0 MAX OVER + SWAP
  DO   I _pad $@ CORA   0= IF I UNLOOP EXIT THEN   LOOP   0 ;
\ Find WORD in blocks N and up. Leave the BLOCK or throw.
: #LOCATED  >R FILL-pad R> BEGIN 0 OVER (LINE) -TRAILING
    DUP 0= 24 AND THROW POSITION 0= WHILE 1+ REPEAT   ;
: (WANTED)   ( sc -- ) ERRSCR @ 4 + >R   BEGIN 2DUP PRESENT 0=
  WHILE 2DUP R> #LOCATED DUP 1+ >R  LOAD REPEAT RDROP ;
\ Make sure WORD is present in the current namespace
: WANTED   '(WANTED) CATCH DUP 24 = IF >R ETYPE R> MESSAGE
    ELSE THROW 2DROP THEN ;   : WANT   ( "name" ) NAME WANTED ;
: CF: "CONFIG" WANTED ;   WANT -legacy-
( -b :_This_option_is_available )














\
( -c PROGRAM :_compile_PROGRAM_to_binary ) \ AvdH A1oct02
1 LOAD   WANT OLD:   WANT TURNKEY   WANT SWAP-DP
WANT ARG[]   WANT INCLUDE   WANT SRC>EXEC
: MY-ERROR  DUP EXIT-CODE ! OLD: ERROR BYE ;
'MY-ERROR DUP 'ERROR 3 CELLS MOVE  HIDDEN
\ Don't include source in executable.
: INCD'   SWAP-DP GET-FILE SWAP-DP EVALUATE ;
'INCD' DUP 'INCLUDED 3 CELLS MOVE   HIDDEN

'TASK 'OPTIONS 3 CELLS MOVE \ No options.
'TASK '.SIGNON 3 CELLS MOVE \ No sign on.

ARGC 3 < 13 ?ERROR
2 ARG[] INCLUDED
LATEST   2 ARG[] SRC>EXEC   TURNKEY

( -d :_This_option_is_available )














\
( -e :_Load_system_electives ) \ AvdH A3sep01
.SIGNON CR 0 BLOCK  B/BUF TYPE 1 LOAD

WANT CONFIG      WANT HELP       WANT ORDER
WANT L-S         WANT DO-DEBUG   WANT H.         WANT DUMP
WANT SUPER-QUAD  WANT FARDUMP    WANT $.         WANT ^
WANT INCLUDE     WANT CRACK      WANT LOCATE     WANT OS-IMPORT
WANT CASE-INSENSITIVE            CASE-INSENSITIVE
\ WANT EDITOR      WANT OOPS
\ WANT ASSEMBLERi86 WANT DEADBEEF

( WANT DIR )     WANT ls    \ Select os-interface DOS/Unix

: TASK ;
 ( BACKUP        250 LOAD   77 81 THRU )
OK
( -f :_Forth_words_to_be_executed_80_chars) \ AvdH A1oct05
1 LOAD
WANT ARG[]   WANT Z$@
CREATE COMMAND-BUFFER 0 , 1000 ALLOT
: DOIT
    BEGIN SHIFT-ARGS ARGC 1 > WHILE
    1 ARG[] COMMAND-BUFFER $+!   BL COMMAND-BUFFER $C+
    REPEAT ;
DOIT    COMMAND-BUFFER $@
\ 'DOIT HIDDEN   COMMAND-BUFFER HIDDEN
2DUP TYPE EVALUATE




\
( -g GROW :_grow_by_megabytes ) \ AvdH A4oct26
'FORTH >WID >LFA @    HERE CONSTANT HERE-NOW
1 LOAD      WANT ARG[]  CF: ?LI
ARGC 4 <> 13 ?ERROR    WANT SAVE-SYSTEM
SM HERE-NOW OVER - $, CONSTANT thisforth$
 2 ARG[] EVALUATE 20 LSHIFT CONSTANT INCREASE
 thisforth$ CELL+ SM - CONSTANT >TARGET
: PATCH   >TARGET + INCREASE SWAP +! ;
: PATCH-CONSTANT   >DFA PATCH ;
: PATCH-ALL   5 0 DO I CELLS +ORIGIN PATCH LOOP
  PREV PATCH   G-SIZE PATCH
  'EM PATCH-CONSTANT   'FIRST PATCH-CONSTANT
  'LIMIT PATCH-CONSTANT  ;
: GROW   'FORTH >WID >LFA >TARGET + ! PATCH-ALL
thisforth$ $@ 3 ARG[]   PUT-FILE ;
GROW BYE
( -h :_help,_show_options ) \ AvdH A1oct04
.SIGNON   1 20 INDEX CR
.( See also the pdf documentation ) CR
.( or print the PostScript documentation ) CR

OK BYE









\
( -i BINARY_PATH LIBRARY_PATH SHELL_PATH )\ A3aug30
CREATE task     1 LOAD
   WANT ARG[]   WANT SAVE-SYSTEM
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
1 LOAD   WANT SHIFT-ARGS   WANT ARG[]
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
5 LOAD














( -q :_This_option_is_available )














\
( -r :_Make_require_available ) \ AvdH A1oct04
.SIGNON   1 LOAD   OK













\
( -s SCRIPT-FILE :_Interpret_SCRIPT-FILE ) \ AvdH A1oct02
1 LOAD   WANT OLD:   WANT ARG[]
WANT CTYPE   2 ARG[] $, CONSTANT SCRIPT-NAME
: BY-WHO   "LINOS" PRESENT? IF " run by " TYPE
0 ARG[] TYPE THEN ;
\ This error handler may be overwritten by the script.
: MY-ERROR    DECIMAL
    "In file " TYPE SCRIPT-NAME $@ TYPE BY-WHO CR
    IN @ 20 - 40 TYPE CR
    "Fatal error at : " TYPE   OLD: ERROR CR CR    ;
-1 WARNING !     'MY-ERROR >DFA @     'ERROR >DFA !
SHIFT-ARGS  SHIFT-ARGS
SCRIPT-NAME $@ GET-FILE
^J $/ 2DROP     \ Line with #!lina
EVALUATE
BYE
( -t FILE :_Try_to_compile_FILE_by_all_means ) \ AvdH A1oct26
.SIGNON 1 LOAD
\ Reload WANT with new ``CORA'' but hide it direct after.
WANT CORA-IGNORE
: CORA CORA-IGNORE ;   1 LOAD   'CORA HIDDEN

WANT [IF]   WANT ARG[]   WANT PREFIX

WANT CASE-INSENSITIVE    CASE-INSENSITIVE
WANT AUTOLOAD            AUTOLOAD

2 ARG[] INCLUDED
SECOND-PASS @ 0= ?LEAVE-BLOCK
2 ARG[] INCLUDED

\
( -u :_This_option_is_available )














\
( -v :_Version_and_copyright_information_)
"CPU  NAME  VERSION" TYPE .SIGNON CR
"LIBRARY FILE: " TYPE
"$RCSfile$ $Revision$" TYPE CR
CR
0 BLOCK  B/BUF TYPE
BYE








\
( -w :_This_option_is_available )














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
