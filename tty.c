/* tty.c */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include "tty.h"

extern int errno;
/*  extern char     *sys_errlist[];                                          */
int nodevice;

/* ****************************************************************************
 * tty_error(),tty_warning():
 *     error handling and cleanup
 * ***************************************************************************/
void 
tty_error (char *s)
{
  fprintf (stderr, "\n\nerror: %s, (%s)\n", s, sys_errlist[errno]);
  fflush (stderr);
}

void 
tty_warning (char *s)
{
  fprintf (stderr, "\n\nwarning: %s, (%s)\n", s, sys_errlist[errno]);
  fflush (stderr);
}

/* ****************************************************************************
 * tty_init(), tty_set(), tty_restore()  : 
 *     primitive functions to encapsulate terminal state changes. Any
 *     terminal state modification is done through this two functions.
 * ***************************************************************************/
void 
tty_set (struct tty *ptty)
{
  if (nodevice)
    return;
  if (tcsetattr (ptty->fd, TCSADRAIN, &ptty->now.tio) < 0)
    tty_error ("unable to set tty state");
}

void 
tty_restore (struct tty *ptty)
{				/* write termio struct */
  if (nodevice)
    return;
  if (tcsetattr (ptty->fd, TCSADRAIN, &ptty->org.tio) < 0)
    tty_error ("unable to restore tty state");
}

void 
tty_init (int fd, struct tty *ptty)
{				/* fill-in tty struct */
  ptty->fd = fd;
  nodevice = 0;
  /* save original tty_state */
  if (tcgetattr (ptty->fd, &ptty->org.tio) < 0)
    nodevice = 1;
  if (nodevice)
    return;
  /* template for new tty_state */
  if (tcgetattr (ptty->fd, &ptty->now.tio) < 0)
    nodevice = 1;
  if (nodevice)
    return;
  /* disable some special characters */
  ptty->now.tio.c_cc[VINTR] = _POSIX_VDISABLE;
  ptty->now.tio.c_cc[VQUIT] = _POSIX_VDISABLE;
  ptty->now.tio.c_cc[VSUSP] = _POSIX_VDISABLE;
  tty_set (ptty);
}

/* ****************************************************************************
 * tty_echo(), tty_noecho() :
 *     change tty attributes
 * ***************************************************************************/
void 
tty_echo (struct tty *ptty)
{
  if (nodevice)
    return;
  ptty->now.tio.c_lflag |= ECHO;
  tty_set (ptty);
}

void 
tty_noecho (struct tty *ptty)
{
  if (nodevice)
    return;
  ptty->now.tio.c_lflag &= ~ECHO;
  tty_set (ptty);
}

/* ****************************************************************************
 * tty_keymode(), tty_linemode() :
 *    change main tty operating mode
 * ***************************************************************************/
void 
tty_keymode (struct tty *ptty)
{
  if (nodevice)
    return;
  ptty->now.tio.c_lflag &= ~ICANON;
  ptty->now.tio.c_cc[VMIN] = 1;
  ptty->now.tio.c_cc[VTIME] = 0;
  tty_set (ptty);
}

void 
tty_linemode (struct tty *ptty)
{
  if (nodevice)
    return;
  ptty->now.tio.c_lflag |= ICANON;
/* VMIN, VTIME are overloaded and should really be restored ... */
  ptty->now.tio.c_cc[VMIN] = ptty->org.tio.c_cc[VMIN];
  ptty->now.tio.c_cc[VTIME] = ptty->org.tio.c_cc[VTIME];
  tty_set (ptty);
}

/* ************************************************************************** */

int 
keyq (int fd)
{
  fd_set rfds;
  fd_set afds;
  struct timeval tv;
  int nfds = 1;
  if (nodevice)
    return 1;
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  FD_ZERO (&afds);
  FD_SET (fd, &afds);
  memmove (&rfds, &afds, sizeof (rfds));
  if (select (nfds, &rfds, NULL, NULL, &tv) < 0)	/* any input around? */
    tty_warning ("select failed");
  if (FD_ISSET (fd, &rfds))	/* input from fd? */
    return 1;
  return 0;
}				/* no input */

int 
tty_keyq (struct tty *ptty)
{
  return (keyq (ptty->fd));
}
