\ INCLUDE regexp.frt
VARIABLE OLDIN : !OLDIN IN @ OLDIN ! ; !OLDIN
 : test IF CR "The following failed: " TYPE OLDIN @ IN @ OVER - TYPE CR THEN !OLDIN ;
\ Verbose
\ : test CR IF "The following failed: " ELSE "The following suceeded: " THEN TYPE
\    OLDIN @ IN @ OVER - TYPE CR !OLDIN ;

: match 0= test ;
: nomatch test ;

\ No syncs  --------------------------
\ Simple matches
"" "^" RE-MATCH match
\ "A" "^" RE-MATCH match
\ "" "^A" RE-MATCH match
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
\ "" "" RE-MATCH match
\ "A" "" RE-MATCH match
\ "" "A" RE-MATCH match
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

\ "" "A+" RE-MATCH    nomatch
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
