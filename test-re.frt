\ INCLUDE regexp.frt
VARIABLE OLDIN : !OLDIN IN @ OLDIN ! ; !OLDIN
: test IF CR "The following failed: " TYPE OLDIN @ IN @ OVER - TYPE CR THEN !OLDIN ;
\ Verbose
\     : test CR IF "The following failed: " ELSE "The following suceeded: " THEN TYPE
\     OLDIN @ IN @ OVER - TYPE CR !OLDIN ;

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
