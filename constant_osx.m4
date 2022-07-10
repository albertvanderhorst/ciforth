_C{   System calls for OSX were kindly supplied by Robert Spykerman          }
_C{   who is the admin of https://sourceforge.net/projects/xina/  }
_C{ ------------------------------------------------------------      }

_C{ FIXME! The relation between these numbers and others Unices }
_C{  is still to be documented                                  }
_C{ you may benefit from the program stealconstant }
_C{ https://github.com/albertvanderhorst/stealconstant }

SEEK_SET        EQU     0x0
TCGETS  EQU     0x5401
TCSETS  EQU     0x5402
ECHO    EQU     0x8
EAGAIN  EQU     0xb
EINTR   EQU     0x4
EPIPE   EQU     0x20
VMIN    EQU     0x6
VTIME   EQU     0x5
ICANON  EQU     0x2
O_RDWR  EQU     0x2
O_RDONLY        EQU     0x0
O_WRONLY        EQU     0x1
SIZE_TERMIO     EQU     0x3c

_C{ From apple's file:///Developer/SDKs/MacOSX10.5.sdk/usr/include/sys/fcntl.h}
O_CREAT EQU     0x0200  _C{ create if nonexistant }
O_TRUNC EQU     0x0400  _C{ truncate to zero length }
_C{O_ACCMODE     EQU       0x0003  ;       ; mask for above modes }
_C{O_EXCL  EQU     0x0800  ; error if already exists }
_C{S_IRUSR EQU     0000400 ;       ; [XSI] R for owner }
_C{S_IWUSR EQU     0000200 ;       ; [XSI] W for owner }
_C{S_IXUSR EQU     0000100 ;       ; [XSI] X for owner }
_C{S_IRWXU EQU     0000700 ;       ; [XSI] RWX mask for owner }

_C{ Numbers of system calls. See "Linux kernel Internals" Appendix A. }
_C{ By M.Beck, H. Boehme e.a. Addison Wesley.                         }
_C{ The system calls themselves are extensively documented in chapter }
_C{ 2 of the man pages, e.g. "man 2 exit"}
exit    EQU     0x1
open    EQU     0x5
close   EQU     0x6
creat   EQU     0x8
unlink  EQU     0xa
chdir   EQU     0xc
read    EQU     0x3
access  EQU     0x21
select  EQU     0x52
_newselect      EQU     0x5d
_C{ _newselect      EQU     0x8e  }
write   EQU     0x4
ioctl   EQU     0x36
_C{ ioperm  EQU     0x65   ; Spykerman non-existant? }
_C{ iopl    EQU     0x6e   ; Spykerman non-existant?   }
lseek   EQU     0xc7
_C{_osx_lseek   EQU   0xc7}
execve  EQU     0x3B
_C{_osx_execve  EQU     59}
fork    EQU     0x2
waitpid EQU     0x7
pipe    EQU     0x2a
_C{ ------------------------------------------------------------      }
_C{   End of constants stolen from C.                                 }
_C{ ------------------------------------------------------------      }
