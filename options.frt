COPYRIGHT (c) 2000-2002 Albert van der Horst, THE NETHERLANDS
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

( -a :_Make_require_available_silently ) \ AvdH A2jan20
( PRESENT? REQUIRE REQUIRED ) \ AvdH A1oct04
\ This screen must be at a fixed location. To find REQUIRED.
\ For LINE and WORD sc's : line CONTAINS word.
CREATE pad 80 ALLOT    : CONTAINS   0 pad !   BL pad $C+
pad $+!   BL pad $C+   pad @ - OVER + SWAP
    DO   I pad $@ CORA   0= IF -1 UNLOOP EXIT THEN   LOOP   0 ;
\ Find WORD in the block library and load it.
: FIND&LOAD     256 ERRSCR @ 4 +
DO 0 I (LINE) 2OVER CONTAINS IF I LOAD LEAVE THEN LOOP
2DROP ;
\ For WORD sc: it IS found unabbreviated.
: PRESENT? PRESENT 0= 0= ;
\ Make sure WORD is present in the ``FORTH'' vocabulary.
: REQUIRED 2DUP PRESENT? IF 2DROP ELSE FIND&LOAD THEN ;
: REQUIRE (WORD) REQUIRED ;    : CF "CONFIG" REQUIRED ;
( -b :_This_option_is_available )















( -c PROGRAM :_compile_PROGRAM_to_binary ) \ AvdH A1oct02
1 LOAD   REQUIRE OLD:   REQUIRE TURNKEY   REQUIRE SWAP-DP
REQUIRE ARG[]   REQUIRE INCLUDE   REQUIRE SRC>EXEC
: MY-ERROR  DUP EXIT-CODE ! OLD: ERROR BYE ;
\ Be economic with disk space
: INCD'   SWAP-DP GET-FILE SWAP-DP EVALUATE ;
: INC' (WORD) INCD' ;
: MY-OPTIONS DROP 0 ;  \ No options, no sign on.
'MY-ERROR DUP 'ERROR 3 CELLS CMOVE  HIDDEN
'INCD' DUP 'INCLUDED 3 CELLS MOVE   HIDDEN
'INC'  DUP 'INCLUDE 3 CELLS MOVE    HIDDEN
'MY-OPTIONS DUP 'OPTIONS 3 CELLS MOVE  HIDDEN
ARGC 3 < 13 ?ERROR  0 HANDLER ! ( We are being caught!)
2 ARG[] INCLUDED
LATEST   2 ARG[] SRC>EXEC   TURNKEY

( -d :_This_option_is_available )














\
( -e :_Load_system_electives ) \ AvdH A1oct19
.SIGNON CR 0 LIST  1 LOAD    : REQ REQUIRE ;

REQ CONFIG
( MAINTENANCE ) REQ L-S  REQ DO-DEBUG
REQ H.   REQ DUMP   REQ SUPER-QUAD   REQ FARDUMP
REQ $.   REQ ^      REQ INCLUDE
\ REQ REFRESH ( temporaryly)  \ CREATE I-LIKE-DOS
REQ CRACK    REQ LOCATE   REQ OS-IMPORT
REQ EDITOR   REQ OOPS                         OK  EXIT
 ( BACKUP        250 LOAD   77 81 THRU )
( REQ ASSEMBLERi86 )
( REQ DEADBEEF )


: TASK ;   ( 'REQ HIDDEN)     OK
( -f :_Forth_words_to_be_executed_80_chars) \ AvdH A1oct05
1 LOAD  REQUIRE CONFIG   ?LI
REQUIRE ARGV   REQUIRE Z$@
CREATE COMMAND-BUFFER 0 , 1000 ALLOT
: DOIT   ARGV CELL+ CELL+
    BEGIN $@   DUP WHILE
    Z$@ COMMAND-BUFFER $+!   BL COMMAND-BUFFER $C+
    REPEAT 2DROP ;
DOIT    COMMAND-BUFFER $@
\ 'DOIT HIDDEN   COMMAND-BUFFER HIDDEN
2DUP TYPE EVALUATE




\
( -g :_This_option_is_available )














\
( -h :_help,_show_options ) \ AvdH A1oct04
.SIGNON   1 31 INDEX   OK BYE













\
( -i BIN_INSTALL_PATH LIB_INSTALL_PATH :_Install) \ A1jan20
CREATE task
1 LOAD
REQUIRE SAVE-SYSTEM   REQUIRE ARG[]
BLOCK-FILE $@ GET-FILE   3 ARG[] PUT-FILE
3 ARG[] BLOCK-FILE $!
\ Trim back to before ``task''. Save system at binary path.
: DOIT   'task    DUP 'FORTH FORGET-VOC   >NFA @ DP !
    2 ARG[] SAVE-SYSTEM   BYE ;

\ Must all be done in one go!
DOIT



\
( -j :_This_option_is_available )














\
( -k :_This_option_is_available )














\
( -l LIBRARY :_LIBRARY_to_be_used_for_blocks ) \ AvdH A1oct05
CREATE task
1 LOAD   REQUIRE SHIFT-ARGS   REQUIRE ARG[]
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
DROP  1 LOAD    REQUIRE OLD:   REQUIRE ARGV   REQUIRE CTYPE
ARGV CELL+ CELL+ @ Z$@ $, CONSTANT SCRIPT-NAME
\ This error handler may be overwritten by the script.
: MY-ERROR    DECIMAL
    "In file " TYPE SCRIPT-NAME $@ TYPE " run by "
    TYPE ARGV @ CTYPE CR  IN @ 20 - 40 TYPE CR
    "Fatal error at : " TYPE
    OLD: ERROR CR CR CR CR ( BYE)
    ;
-1 WARNING !     'MY-ERROR >DFA @     'ERROR >DFA !

SCRIPT-NAME $@ GET-FILE
^J $S 2DROP     \ Line with #!lina
EVALUATE
BYE
( -t FILE :_Try_to_compile_FILE_by_all_means ) \ AvdH A1oct26
.SIGNON 1 LOAD
\ Reload REQUIRE with new ``CORA'' but hide it direct after.
REQUIRE CORA-IGNORE
: CORA CORA-IGNORE ;   1 LOAD   'CORA HIDDEN

REQUIRE [IF]   REQUIRE ARG[]   REQUIRE ARGC   REQUIRE $

REQUIRE CASE-INSENSITIVE    CASE-INSENSITIVE
REQUIRE AUTOLOAD            AUTOLOAD

ARGC 3 < ?LEAVE-BLOCK
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
0 LIST
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
