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
/*  This c-source is only intended to be print a number of EUQATES           */
/*  in form that is palatable for the assemblers used in generating          */
/*  ciforth.                                                                 */
/*                                                                           */
/*****************************************************************************/

#define STEAL(A,B)   printf("%s   EQU   0x%x\n", A, B );

int main()
{
        STEAL("SEEK_SET",SEEK_SET)
        STEAL("TCGETS",TCGETS)
        STEAL("TCSETS",TCSETS)
        STEAL("ECHO",ECHO)
        STEAL("EAGAIN",EAGAIN)
        STEAL("EINTR",EINTR)
        STEAL("EPIPE",EPIPE)
        STEAL("VMIN",VMIN)
        STEAL("VTIME",VTIME)
        STEAL("ICANON",ICANON)
        STEAL("O_RDWR",O_RDWR)
        STEAL("O_RDONLY",O_RDONLY)
        STEAL("O_WRONLY",O_WRONLY)
        STEAL("O_CREAT",O_CREAT)
        STEAL("O_NONBLOCK",O_NONBLOCK)
        STEAL("SIZE_TERMIO",sizeof(struct termios))

printf(";{ Numbers of system calls. See \"Linux kernel Internals\" Appendix A. }\n");
printf(";{ By M.Beck, H. Boehme e.a. Addison Wesley.                         }\n");
printf(";{ The system calls themselves are extensively documented in chapter }\n");
printf(";{ 2 of the man pages, e.g. \"man 2 exit\"}\n");
        STEAL("exit",__NR_exit)
        STEAL("open",__NR_open)
        STEAL("close",__NR_close)
        STEAL("creat",__NR_creat)
        STEAL("unlink",__NR_unlink)
        STEAL("chdir",__NR_chdir)
        STEAL("read",__NR_read)
        STEAL("select",__NR_select)
        STEAL("_newselect",__NR__newselect)
        STEAL("write",__NR_write)
        STEAL("ioctl",__NR_ioctl)
        STEAL("ioperm",__NR_ioperm)
        STEAL("lseek",__NR_lseek)
        STEAL("execve",__NR_execve)
        STEAL("fork",__NR_fork)
        STEAL("waitpid",__NR_waitpid)
printf(";{ ------------------------------------------------------------      }\n");
printf(";{   End of constants stolen from C.                                 }\n");
printf(";{ ------------------------------------------------------------      }\n");
        exit(0);
}
