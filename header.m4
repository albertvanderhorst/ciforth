dnl  $Id$  M4 file to handle the develish FIG headers.
dnl Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License
dnl Once and for all. 
changequote({,})dnl
changecom{}dnl
dnl
dnl
dnl _STRING : Lay down a string in memory.
dnl Take care of embedded double quotes by using single quotes.
dnl Note: this cannot be used in HEADER, because index must look in the real string,
dnl not on some variable that contains the string.
dnl The digression using _squote is needed because the single quote is used in m4.
define(_squote,')
define({_dbquoted},"{$1}")dnl
define({_sgquoted},'{$1}')dnl
define({_quoted},{ifelse( -1, index({$1},"),{_dbquoted},{_sgquoted})}({{$1}}))
define({_STRING},{
        DB      _quoted}({{$1}}))dnl
dnl             
dnl _LINKOLD is a m4 variable that generates numbers in sequence.
dnl We lay down a nice square around the definition as a tribute to Thomas Newman
dnl _star(x) generates x stars
dnl May later be simplified by having only x-1,
dnl May be later simplified by splitting off the quoting from string.
dnl Maybe that isn}t necessary because we will have the { } quotes anyway.
define({_star},{ifelse}(0,$1,,{*{_star}({decr}($1))}))
define({_LINKOLD},0)dnl
define(HEADER,{
define({_x},len({$1}))dnl
define({_y}, {substr({$1},eval(_x-1),1)})dnl
define({_z}, {substr({$1},0,eval(_x-1))})dnl
;  ********_star(len({$1})) 
;  *   $1   *
;  ********_star(len({$1})) 
;  
_$2:     DB   80H+len({$1})ifelse(1,$4,+40H,)
         DB       ifelse(1,_x, , "{_z}"{,})"_y"+80H
         DW    _LINKOLD
$2:      DW     $3
         undefine({_x})dnl
         undefine({_y})dnl
         undefine({_z})dnl
define({_LINKOLD},{_$2})dnl
})
dnl
dnl Similar to HEADER but uses single quotes. instead of doubles. 
dnl Useful if a name has double quotes like PDOT. (Cannot handle a combination of the two)
define(HEADER_SGQ,{
define({_x},len({$1}))dnl
define({_y}, {substr({$1},eval(_x-1),1)})dnl
define({_z}, {substr({$1},0,eval(_x-1))})dnl
;  ********_star(len({$1})) 
;  *   $1   *
;  ********_star(len({$1})) 
;  
_$2:    DB   80H+len({$1})ifelse(1,$4,+40H,)
        DB       ifelse(1,_x, , _squote{_z}_squote{,})_squote{}_y{}_squote+80H
        DW    _LINKOLD
$2:     DW     $3
        undefine({_x})dnl
        undefine({_y})dnl
        undefine({_z})dnl
define({_LINKOLD},{_$2})dnl
})
dnl
dnl Similar to HEADER but can handle a name that ends in tick.
dnl Do not put in the tick (}), use a space as a place holder.
define(HEADER_TICK,{

;  ********_star(len({$1})) 
;  *   $1   *
;  ********_star(len({$1})) 
;  
        EVEN 
	_LINK	= $				;;link points to a name string
        DB   80H+len({$1})ifelse(1,$4,+40H,)
        define({_x},len({$1}))dnl
        define({_y}, {substr({$1},eval(_x-1),1)})dnl
        define({_z}, {substr({$1},0,eval(_x-1))})dnl
        define({_w}, {_STRING({_z}),})dnl
ifelse(1,_x,{
        DB    },
_STRING({{_z}}){,})dnl
"_squote"+80H
        EVEN 
        DW    _LINKOLD
        _LINKOLD=_LINK
$2:     DW     $3
        undefine({_x})dnl
        undefine({_y})dnl
        undefine({_z})dnl
        undefine({_w})dnl
})
define({HEADER_NULL},{
_$2:            DB      0C1H,80H
                DW      _LINKOLD
$2:             DW      $3
define({_LINKOLD},{_$2})dnl
})
dnl

