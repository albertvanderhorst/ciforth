; ------------------------------------------------------------
;   Constants stolen from C in OSX environment.
; ------------------------------------------------------------

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


; ------------------------------
; syscall constant declarations:
; ------------------------------

exit    EQU     0x1
open    EQU     0x5
close   EQU     0x6
creat   EQU     0x8
unlink  EQU     0xa
chdir   EQU     0xc
read    EQU     0x3
_newselect  EQU     0x5D
write   EQU     0x4
ioctl   EQU     0x36
; ioperm  EQU     0x65 ; non-existent?
; iopl    EQU     0x6e ; non-existent?
_osx_lseek   EQU     0xC7
_osx_execve  EQU     59
fork    EQU 0x2
waitpid EQU     0x7

RAWIO           EQU     (ECHO _OR_ ICANON)

; ------------------------------------------------------------
;   End of constants stolen from C in OSX environment.
; ------------------------------------------------------------
