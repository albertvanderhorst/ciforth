include(prelude.m4)dnl
include(postlude.m4)dnl 
define({quote},{{{$1}}})
define({forthvar},{@var{}quote($1)})
define({forthsamp},{@samp{}quote($1)})
define({forthcode},{@code{}quote($1)})
