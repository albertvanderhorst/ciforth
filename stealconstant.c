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

/*****************************************************************************/
/*                                                                           */
/*  This is really a weird c-source. It is only intended to be pushed        */
/*  through the c-preprocessor. The resulting define's are used by M4.       */
/*  To be safe, only the define lines are retained, by using grep.           */
/*                                                                           */
/*****************************************************************************/

define({M4_SEEK_SET},SEEK_SET)
define({M4_O_RDWR},O_RDWR)

define({M4_@},__NR_@)
define({M4_open},__NR_open)
define({M4_close},__NR_close)
define({M4_read},__NR_read)
define({M4_write},__NR_write)
define({M4_lseek},__NR_lseek)

