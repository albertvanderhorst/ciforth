divert(-1)dnl
#include <stdio.h>
#include <stdlib.h>
#include <sys/times.h>
#include <unistd.h>
#include <linux/unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <termios.h> 
#include <signal.h>
#include <setjmp.h>
#include <fcntl.h>
#include <errno.h>
#include <ioctls.h>

/*****************************************************************************/
/*                                                                           */
/*  This is really a weird c-source. It is only intended to be pushed        */
/*  through the c-preprocessor, then through M4 that does the actually       */
/*  stealing. The result is a list of EQU's to be included in the main       */
/*  assembly source.                                                         */
/*                                                                           */
/*****************************************************************************/

/* Templates for editing: 
steal({%}{@},%@)
steal({%}{@},__NR_%@)
 */

define({steal}, {{$1}      EQU     $2})
divert{}dnl
; ------------------------------------------------------------
;   Start of constants stolen from C.
; ------------------------------------------------------------
steal({S}{EEK_SET},SEEK_SET)
steal({T}{CGETS},TCGETS)
steal({T}{CSETS},TCSETS)
steal({E}{CHO},ECHO)
steal({V}{MIN},VMIN)
steal({V}{TIME},VTIME)
steal({I}{CANON},ICANON)
steal({O}{_RDWR},O_RDWR)
steal({O}{_RDONLY},O_RDONLY)

; Numbers of system calls. See "Linux kernel Internals" Appendix A.
; By M.Beck, H. Boehme e.a. Addison Wesley.
; The system calls themselves are extensively documented in chapter
; 2 of the man pages, e.g. "man 2 exit"
steal({e}{xit},__NR_exit)
steal({o}{pen},__NR_open)
steal({c}{lose},__NR_close)
steal({r}{ead},__NR_read)
steal({w}{rite},__NR_write)
steal({i}{octl},__NR_ioctl)
steal({l}{seek},__NR_lseek)

; ------------------------------------------------------------
;   End of constants stolen from C.
; ------------------------------------------------------------
