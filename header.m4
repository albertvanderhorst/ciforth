dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl
dnl _STRING : Lay down a string in memory.
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
dnl The digression using _squote is needed because the single quote is used in m4.
define(_squote,')
define({_dbquoted},"{{$1}}")dnl
define({_sgquoted},'{{$1}}')dnl
define({_quoted},{ifelse( -1, index({$1},{"}),{_dbquoted},{_sgquoted})}({{$1}}))
define({_STRING},{
        DB      len({$1})
        DSS      _quoted}({{$1}}))dnl
dnl             
dnl _LINKOLD is a m4 variable that generates numbers in sequence.
dnl We lay down a nice square around the definition as a tribute to Thomas Newman
dnl _star(x) generates x stars
define({_star},{ifelse}(0,$1,,{*{_star}({decr}($1))}))
define({_LINKOLD},0)dnl
dnl Lay down a header with forth name $1, assembler name $2 and code field $3
dnl If $4 is defined, make the word immediate.
define(HEADER, {dnl
define({_x},len({$1}))dnl
;  ********_star(len({$1})) 
;  *   {$1}   *
;  ********_star(len({$1})) 
;  
N_$2:     DB   80H+len({$1})ifelse(1,$4,+40H,)
ifelse(1,_x, , 
{         DSS      }_quoted({substr({$1},0,eval(_x-1))})
)dnl    
dnl Rather mysterious: without the dummy ifelse it just doesn;t work
ifelse(1,2, , 
{         DB       }_quoted({substr({$1},eval(_x-1),1)})+80H
)dnl
         DC    N_$2
         DC    _LINKOLD
$2:      DC     $3
         undefine({_x})dnl
define({_LINKOLD},{$2-2*CW})dnl
})dnl
define({HEADER_NULL},{
N_$2:            DB      0C1H,80H
         DC    N_$2
         DC    _LINKOLD
$2:             DC      $3
define({_LINKOLD},{$2-2*CW})dnl
})
dnl
dnl
dnl ------------------ to get dictionaries better under control -------------------------------------
dnl The link etc. field of the word with assembler name $1
define({_DEA},$1-CW*2)dnl
define({_LINK_FIELD},($1-CW))dnl
define({_CODE_FIELD},$1)dnl
define({_PARAMETER_FIELD},($1+CW))dnl
dnl     Handle Branching
define({_0BRANCH},dnl
{DC      ZBRAN
        DC      $1-$})dnl
define({_BRANCH},dnl
{DC      BRAN
        DC      $1-$})dnl
define({_LOOP},dnl
{DC      XLOOP
        DC      $1-$})dnl
dnl The field where a pointer to the latest entry of a vocabulary resides.
define({CODE_HEADER},{HEADER({$1},{$2},{$+CW})})dnl
define({JMPHERE_FROM_PROT},{})dnl
define({JMPHERE_FROM_REAL},{})dnl
define({JMPFAR},{DB    0EAH})dnl
define({CELLS},(CW*$1))dnl
#
# Start of Intel dependant code part
# The 32 bit version may be used in the postlude to redefine
# _NEXT etc. to generate faster code.
#
# See definition of NEXT in glossary.
define({_NEXT},{JMP     NEXT})   
define({_NEXT32},
        {LODSW                 ; NEXT
        MOV     BX,AX                  
        JMP     _CELL_PTR[BX]  } )
# See definition of PUSH in glossary.
define({_PUSH},{JMP     APUSH})   
define({_PUSH32},
        {PUSH    AX
        _NEXT32})
# Like PUSH but for two numbers.      
define({_2PUSH},{JMP     DPUSH})   
define({_2PUSH32},
        {PUSH    DX
        _PUSH32})

