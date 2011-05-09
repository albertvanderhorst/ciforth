dnl $Id$
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Macro's to adapt the source to Microsoft Macro Assembler 7.00.9466
divert(-1)dnl
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in _HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
define({_dbquoted},"{{$1}}")dnl
define({_sgquoted},'{{$1}}')dnl
define({_quoted},{ifelse( -1, index({$1},{"}),{_dbquoted},{_sgquoted})}({{$1}}))
dnl
define({_C},{{;}})
define({_HEADER_ASM},{;
; MASM/TASM version of ciforth created by ``m4'' from generic listing.
; This source can be assembled using the microsoft assembler ML.EXE ,
;  nowadays deeply hidden in a Visual Studio bin directory.
;})dnl
define({SET_32_BIT_MODE},{ .686p })dnl
define({_SECTION_},{$1    SEGMENT})
define({_SECTION_NOBITS_},{$1   SEGMENT READ WRITE EXECUTE STACK})
define({_SECTION_END_},{$1   ENDS})    
dnl Get rid of unknown MASM specifier.
define({_BYTE},)dnl
dnl
define({_ENDP},)dnl Each ENDP is started with _ENDP in generic listing.
define({_EXTRANOP},)dnl where MASM introduces a superfluous NOP
define({RELATIVE_WRT_ORIG}, {$1 - ORIG})
dnl
dnl MASM has an unreasonable quirk to reserve memory
define({_RESB}, {DB      $1 DUP (0)})dnl
dnl
dnl Assembly Pointer
define({_AP_}, {$})dnl
dnl
dnl Second and later uses of ORG shift the program counter
define({_NEW_ORG},
        ORG    $1)dnl
dnl
dnl MASM uses operators as keywords
define({_OR_},{OR})
dnl
dnl Pointer handling
define({_BYTE_PTR},{BYTE PTR $1})dnl
define({_CALL_FAR_INDIRECT},{CALL DWORD PTR [$1]})dnl Perfectly unreasonable!
define({_CELL_PTR},{WORD PTR})dnl Sometimes really needed even after introducing [].
define({_OFFSET},OFFSET)dnl
define({LONG},{DWORD})dnl
dnl
dnl Handling large blocks of comment
define({_COMMENT},COMMENT ~
)dnl
define({_ENDCOMMENT},~
)dnl
define({_ALIGN},{ALIGN    $1})dnl
define({DSS},{DB})dnl
define({_DX16},{{DX}})
dnl Work around because of poor performance of .NET assembler.
define({A32},{DB 0x67
        })
divert{}dnl
