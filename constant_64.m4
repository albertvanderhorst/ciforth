
_C{ ------------------------------------------------------------ }
_C{   Constants stolen from C in linux 64 bit environment.        }
_C{ ------------------------------------------------------------ }

SEEK_SET        EQU       0x0
TCGETS  EQU       0x5401
TCSETS  EQU       0x5402
ECHO    EQU       0x8
EAGAIN  EQU       0xb
EINTR   EQU       0x4
EPIPE   EQU       0x20
VMIN    EQU       0x6
VTIME   EQU       0x5
ICANON  EQU       0x2
O_RDWR  EQU       0x2
O_RDONLY        EQU       0x0
O_WRONLY        EQU       0x1
O_CREAT EQU       0x40
O_NONBLOCK      EQU       0x800
SIZE_TERMIO     EQU       0x3c

_C{ Numbers of system calls. See "Linux kernel Internals" Appendix A. }
_C{ By M.Beck, H. Boehme e.a. Addison Wesley.                         }
_C{ The system calls themselves are extensively documented in chapter }
_C{ 2 of the man pages, e.g. "man 2 exit"}

exit    EQU       0x3c
open    EQU       0x2
close   EQU       0x3
creat   EQU       0x55
unlink  EQU       0x57
chdir   EQU       0x50
read    EQU       0x0
select  EQU       0x17
write   EQU       0x1
ioctl   EQU       0x10
ioperm  EQU       0xad
iopl    EQU       0xac
lseek   EQU       0x8
execve  EQU       0x3b
fork    EQU       0x39
waitpid EQU       0x3d
pipe    EQU       0x16

_newselect EQU select

wait4   EQU     waitpid
_C{RAWIO           EQU     (ECHO _OR_ ICANON)}

_C{ ------------------------------------------------------------  }
_C{   End of constants stolen from C in linux 64 bit environment.        }
_C{ ------------------------------------------------------------  }
