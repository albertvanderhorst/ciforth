_C{ ------------------------------------------------------------      }
_C{   Start of constants stolen from C.                                 }
_C{ ------------------------------------------------------------      }

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
O_CREAT EQU     0x40
O_NONBLOCK      EQU     0x800
SIZE_TERMIO     EQU     0x3c

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
select  EQU     0x52
_newselect      EQU     0x8e
write   EQU     0x4
ioctl   EQU     0x36
ioperm  EQU     0x65
iopl    EQU     0x6e
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
