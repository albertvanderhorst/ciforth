\ INCLUDE regexp.frt
VARIABLE OLDIN : !OLDIN IN @ OLDIN ! ; !OLDIN
: test IF CR "The following failed: " TYPE OLDIN @ IN @ OVER - TYPE CR THEN !OLDIN ;
\ Verbose
\     : test CR IF "Test failed: " ELSE "Test suceeded: " THEN TYPE
\     OLDIN @ IN @ OVER - TYPE CR !OLDIN ;
: \ POSTPONE \ !OLDIN ; IMMEDIATE
: match 0= test ;
: nomatch test ;

\ No syncs  --------------------------
\ Simple matches
"" "^" RE-MATCH match
"A" "^" RE-MATCH match
"" "^A" RE-MATCH nomatch
"A" "^A" RE-MATCH match
"AB" "^A" RE-MATCH match
"AB" "^AB" RE-MATCH match
"AB" "^ABC" RE-MATCH nomatch


\ Quantifiers
"" "^A*" RE-MATCH match
"A" "^A*" RE-MATCH match
"AAAA" "^A*" RE-MATCH match
"B" "^A*" RE-MATCH match
"AB" "^A*" RE-MATCH match
"AAAB" "^A*" RE-MATCH match
"B" "^A*B" RE-MATCH match
"AB" "^A*B" RE-MATCH match
"AC" "^A*B" RE-MATCH    nomatch
"ABAC" "^A*BA*D" RE-MATCH    nomatch
"ABACAD" "^A*BA*CA*D" RE-MATCH match
"ABACAD" "^A*BA*DA*DA*" RE-MATCH    nomatch

"" "^A+" RE-MATCH    nomatch
"A" "^A+" RE-MATCH match
"AB" "^A+" RE-MATCH match
"AB" "^A+C" RE-MATCH    nomatch
"AAAAB" "^A+B" RE-MATCH match
"ABACAD" "^A+BA+CA+D" RE-MATCH match
"ABACAD" "^A+BA+DA+DA+" RE-MATCH    nomatch

"AB" "^A?" RE-MATCH match
"AB" "^B?" RE-MATCH match

\ Backtracking
"AAB" "^A*AB" RE-MATCH match
"AAB" "^A+AB" RE-MATCH match
"AB" "^A+AB" RE-MATCH  nomatch
"AB" "^A?AB" RE-MATCH match
"B" "^A?AB" RE-MATCH  nomatch

\ Up front sync --------------------------
\ Simple matches
"" "" RE-MATCH match
"A" "" RE-MATCH match
"" "A" RE-MATCH nomatch
"A" "A" RE-MATCH match
"AB" "A" RE-MATCH match
"AB" "AB" RE-MATCH match
"AB" "ABC" RE-MATCH nomatch


\ Quantifiers
"" "A*" RE-MATCH match
"A" "A*" RE-MATCH match
"AAAA" "A*" RE-MATCH match
"B" "A*" RE-MATCH match
"AB" "A*" RE-MATCH match
"AAAB" "A*" RE-MATCH match
"B" "A*B" RE-MATCH match
"AB" "A*B" RE-MATCH match
"AC" "A*B" RE-MATCH    nomatch
"ABAC" "A*BA*D" RE-MATCH    nomatch
"ABACAD" "A*BA*CA*D" RE-MATCH match
"ABACAD" "A*BA*DA*DA*" RE-MATCH    nomatch

"" "A+" RE-MATCH    nomatch
"A" "A+" RE-MATCH match
"AB" "A+" RE-MATCH match
"AB" "A+C" RE-MATCH    nomatch
"AAAAB" "A+B" RE-MATCH match
"ABACAD" "A+BA+CA+D" RE-MATCH match
"ABACAD" "A+BA+DA+DA+" RE-MATCH    nomatch

"AB" "A?" RE-MATCH match
"AB" "B?" RE-MATCH match

\ Backtracking
"AAB" "A*AB" RE-MATCH match
"AAB" "A+AB" RE-MATCH match
"AB" "A+AB" RE-MATCH  nomatch
"AB" "A?AB" RE-MATCH match
"B" "A?AB" RE-MATCH  nomatch

\ BACK sync also --------------------------

\ Simple matches
"" "$" RE-MATCH match
"A" "$" RE-MATCH match
"" "A$" RE-MATCH nomatch
"A" "A$" RE-MATCH match
"AB" "A$" RE-MATCH nomatch
"AB" "AB$" RE-MATCH match
"AB" "ABC$" RE-MATCH nomatch


\ Quantifiers
"" "A*$" RE-MATCH match
"A" "A*$" RE-MATCH match
"AAAA" "A*$" RE-MATCH match
"B" "A*$" RE-MATCH match
"AB" "A*$" RE-MATCH match
"AAAB" "A*$" RE-MATCH match
"B" "A*B$" RE-MATCH match
"AB" "A*B$" RE-MATCH match
"AC" "A*B$" RE-MATCH    nomatch
"ABAC" "A*BA*D$" RE-MATCH    nomatch
"ABACAD" "A*BA*CA*D$" RE-MATCH match
"ABACAD" "A*BA*DA*DA*$" RE-MATCH    nomatch

"" "A+$" RE-MATCH    nomatch
"A" "A+$" RE-MATCH match
"AB" "A+$" RE-MATCH nomatch
"AB" "A+C$" RE-MATCH    nomatch
"AAAAB" "A+B$" RE-MATCH match
"ABACAD" "A+BA+CA+D$" RE-MATCH match
"ABACAD" "A+BA+DA+DA+$" RE-MATCH    nomatch

"AB" "A?$" RE-MATCH match
"AB" "B?$" RE-MATCH match

\ Backtracking
"AAB" "A*AB$" RE-MATCH match
"AAB" "A+AB$" RE-MATCH match
"AB" "A+AB$" RE-MATCH  nomatch
"AB" "A?AB$" RE-MATCH match
"B" "A?AB$" RE-MATCH  nomatch

\ ---- escape ---------------


HERE ^J C,  ^M C, ^H C, ^I C, ^Z 1+ C, HERE OVER -
CONSTANT ESCAPE-LEN      CONSTANT ESCAPE
: ESCAPE$ ESCAPE  ESCAPE-LEN ;
!OLDIN

ESCAPE$ "^\n\r\b\t\e$" RE-MATCH match

".\^$+?*[]<>()" "^\.\\\^\$\+\?\*\[\]\<\>\(\)$" RE-MATCH match

"***" "^\**$" RE-MATCH match
"***" "^\*+$" RE-MATCH match
"*" "^\*?$" RE-MATCH match
"**" "^\*?$" RE-MATCH nomatch


\ ----- sets -------------------------------

"A" "[A]" RE-MATCH match
"B" "[A]" RE-MATCH nomatch
"A" "[AB]" RE-MATCH match
"B" "[AB]" RE-MATCH match
"C" "[AB]" RE-MATCH nomatch
"AB" "A[B]" RE-MATCH match
"A" "C[B]" RE-MATCH nomatch
"CA" "C[B]" RE-MATCH nomatch

"A" "[AD-H]" RE-MATCH match
"C" "[AD-H]" RE-MATCH nomatch
"D" "[AD-H]" RE-MATCH match
"E" "[AD-H]" RE-MATCH match
"H" "[AD-H]" RE-MATCH match
"I" "[AD-H]" RE-MATCH nomatch
"AD" "A[D-H]" RE-MATCH match
"A" "C[D-H]" RE-MATCH nomatch
"CA" "C[D-H]" RE-MATCH nomatch

"A" "[^AD-H]" RE-MATCH nomatch
"C" "[^AD-H]" RE-MATCH match
"D" "[^AD-H]" RE-MATCH nomatch
"E" "[^AD-H]" RE-MATCH nomatch
"H" "[^AD-H]" RE-MATCH nomatch
"I" "[^AD-H]" RE-MATCH match
"AD" "A[^D-H]" RE-MATCH nomatch
"A" "C[^D-H]" RE-MATCH nomatch
"CA" "C[^D-H]" RE-MATCH match

ESCAPE$ "^[\n\r\b\t\e]*$" RE-MATCH match
ESCAPE$ "^[\n\r\b\e]*$" RE-MATCH nomatch

"nrbte" "^[\n\r\b\t\e]*$" RE-MATCH nomatch
"n" "^[\n\r\b\t\e]" RE-MATCH nomatch
"r" "^[\n\r\b\t\e]" RE-MATCH nomatch
"b" "^[\n\r\b\t\e]" RE-MATCH nomatch
"t" "^[\n\r\b\t\e]" RE-MATCH nomatch
"e" "^[\n\r\b\t\e]" RE-MATCH nomatch

"nrbte" "^[^\n\r\b\t\e]*$" RE-MATCH match

"|" "." RE-MATCH match
"ABCDE" "...." RE-MATCH match
"ABCDE" "....." RE-MATCH match
"ABCDE" "......" RE-MATCH nomatch
ESCAPE$ "....." RE-MATCH nomatch

ESCAPE$ "^[^A]*$" RE-MATCH match

"ABCDE" "[^.]" RE-MATCH nomatch
ESCAPE$ "[^.]" RE-MATCH match

"A" "\w" RE-MATCH match
"ABCDE" "^\w*$" RE-MATCH match
" " "\w" RE-MATCH nomatch
" " "[\w]" RE-MATCH nomatch
" " "[^\w]" RE-MATCH match

\ More difficult backtracking with sets
"A B C" "A.*B.*C" RE-MATCH match
"A B C" "A.*C.*C" RE-MATCH nomatch

\ Start and end of strings.  -----------------------------------
" AAP " "<AAP" RE-MATCH match
"QAAP " "<AAP" RE-MATCH nomatch
" AAP " "AAP>" RE-MATCH match
" AAPQ" "AAP>" RE-MATCH nomatch
" AAP " "<AAP>" RE-MATCH match
"QAAPQ" "<AAP>" RE-MATCH nomatch
" A " "<.>" RE-MATCH match
" AAP " "<.>" RE-MATCH nomatch
" AAP " "<...>" RE-MATCH match
" AAP " "<.*>" RE-MATCH match
" AAP " "<[A-Z]*>" RE-MATCH match
" AAP " "<[^P]*>" RE-MATCH nomatch

\ Grouping   -------------------------

\ "AP"
"AAPE" "AP" RE-MATCH match  CR \0 TYPE

\ "AP"
"AAPE" "(AP)" RE-MATCH match  CR \1 TYPE

\ "NOOT" "NOOT"
"   AAP  NOOT " "<(....)>"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AP" "AQ"
"AAPEAQE" "(A[PQ]).*(A[PQ])" RE-MATCH match CR \1 TYPE \2 TYPE

\ "NOOT" "NOOT"
"   AAP  NOOT " "(<....>)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "NOOT" "NOOT"
"   AAP  NOOT " "(\w\w\w\w)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "(\w+)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "(<\w+>)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "<(\w+)>"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "([A-Z]+)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "(<[A-Z]+>)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "AAP" "AAP"
"   AAP  NOOT " "<([A-Z]+)>"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "P" "P"
"   AAP  NOOT " "([B-Z]+)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "NOOT" "NOOT"
"   AAP  NOOT " "(<[B-Z]+>)"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "NOOT" "NOOT"
"   AAP  NOOT " "<([B-Z]+)>"   RE-MATCH match CR \0 TYPE \1 TYPE

\ "A" "B" "C"
"A B C" "(A).*(B).*(C)" RE-MATCH match CR \1 TYPE \2 TYPE \3 TYPE

\ "ABCDEFGHI"
"ABCDEFGHI" "(A)(B)(C)(D)(E)(F)(G)(H)(I)" RE-MATCH match
CR \1 TYPE \2 TYPE \3 TYPE \4 TYPE \5 TYPE \6 TYPE \7 TYPE \8 TYPE \9 TYPE

\ Substitution -------------------------------------------
\ "AQC"
"ABC" "B" RE-MATCH match
"Q" RE-REPLACE CR TYPE

\ "AQC"
"ARC" "[D-Z]" RE-MATCH match
"Q" RE-REPLACE CR TYPE

\ "AQC"
"ARC" "[D-Z]+" RE-MATCH match
"Q" RE-REPLACE CR TYPE

\ "AQRQC"
"ARC" "[D-Z]+" RE-MATCH match
"Q\0Q" RE-REPLACE CR TYPE

\ "ABQBC"
"ABC" "(B)" RE-MATCH match
"\1Q\1" RE-REPLACE CR TYPE

\ "ABQB"
"ABC" "(B)C" RE-MATCH match
"\1Q\1" RE-REPLACE CR TYPE

\ "ABCABC"
"ABC" ".*" RE-MATCH match
"\0\0" RE-REPLACE CR TYPE

\  "IHGFEDCBA"
"ABCDEFGHI" "(A)(B)(C)(D)(E)(F)(G)(H)(I)" RE-MATCH match
"\9\8\7\6\5\4\3\2\1" RE-REPLACE CR TYPE

\ Precompiled expressions --------------------------------------
\ "A" "B" "C"
: test1 RE-MATCH" (A).*(B).*(C)" ;
"A B C" test1 match CR \1 TYPE \2 TYPE \3 TYPE ;

\  "QIHGFEDCBAQ"
: test2 RE-MATCH" (A)(B)(C)(D)(E)(F)(G)(H)(I)" ;
"QABCDEFGHIQ" test2 match
"\9\8\7\6\5\4\3\2\1" RE-REPLACE CR TYPE
CR
