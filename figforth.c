/* Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License */
#include <stdio.h>
#include <stdlib.h>
#include <sys/times.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <termios.h> 
#include <signal.h>
#include <setjmp.h>
#include <fcntl.h>
#include <errno.h>
 

/* Entry point of figforth. */

/* The offset into the boot parameters. */
typedef enum { COLD = 0, WARM = 4 } BOOT_OFFSET;
extern void figforth( BOOT_OFFSET offset, int argc, char **argv ); 

/* The maximum size of a Forth command passed to the OS */
#define MAX_COMMAND 2000

/* The size of a Forth block aka screen */
#define KBBUF 1024

/* public declarations */ 
struct ttystate { struct termios tio; }; 
struct tty      { int fd; struct ttystate org; struct ttystate now; };  
 
extern int errno;
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
/*ptty->now.tio.c_cc[VINTR] = _POSIX_VDISABLE;                               */
/*ptty->now.tio.c_cc[VQUIT] = _POSIX_VDISABLE;                               */
/*ptty->now.tio.c_cc[VSUSP] = _POSIX_VDISABLE;                               */
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


/*****************************************************************************/
/*                      SIGNALS                                              */
/*****************************************************************************/


/* To be executed on SIGINT (Normally ^C) */
int break_pressed = 0;
void break_int( int ignore )
{
  signal(SIGINT, break_int);
  break_pressed = 1;
}

/* To be executed on SIGQUIT (Normally ^\) */
jmp_buf restart_forth;
void break_quit( int ignore )
{
  signal(SIGQUIT, break_quit);
  longjmp(restart_forth, WARM );
}

struct tty std_in;

/* Actions performed in behalf of Forth.                                     */
/*  The Forth names fully specifiy the action, once you know that the stack  */
/*  translates in variables, but reversed. Forth names are got by removing   */
/*  "c_", upercasing and replacing '_' by  '-' from the c-function name      */

/* ?TERMINAL */
/* ^C is the break key. */
int c_qterm(void)
{
   return break_pressed-- >0;
}

/* ?TERMINAL */
/* The "any key" is the break key. */
int c_qterm_old(void) 
{

  fd_set rfds;
  struct timeval tv; 

  /* Zero timeout, must be set each time! */
  /* A Linux peculiarity.                 */
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  FD_ZERO(&rfds);
  FD_SET(0, &rfds);    /* From standard input */
  select(1, &rfds, NULL, NULL, &tv); 

  return FD_ISSET(0, &rfds);     
}
 
/* KEY */
int c_key( void )
{
  char key;
  int i;
  tty_keymode (&std_in);                                                       
  printf("Watingin for a key \n");
  i=read (0, &key, 1);
  if ( 1!=i) {fprintf(stderr,"oei oei\n"); /* exit(1);*/}                           
  return key;                 
}

/* EMIT */
void c_emit(int ch)
{
  fputc (ch, stdout);
  fflush (stdout);
}

/* TYPE */
void c_type(int count, char buffer[])
{
  write (1, buffer, count);
  fflush (stdout);
}

/* EXPECT */
int c_expec(int count, char buffer[])
{
  int i;
  if ( count <= 0 ) return 0;

  tty_linemode (&std_in);                                                       
  i= read(0, buffer, count);
  buffer[--i]=0;  /* Eat the cr. */

  printf(" Har there, we have a total of %d chars\n", i);
  printf(" It looks like **%s**\n", buffer);
  return i;     /* Ignored by fig-Forth, use it and you have ANSI ACCEPT */
}

int block_fid = -1;

/* Open out block file with the Forth name `filename' */
/* `filename' is a stored Forth string.               */
int c_block_init( int count, char filename[] )
{
  /* turn into c-string */
  char zname[MAX_COMMAND];
  strncpy(zname, filename, count);
  zname[count] = 0;

  close(block_fid);    /* Silently. */
  block_fid = open( zname, O_RDWR );

  return block_fid > 0 ? 0 : errno;
}       

/* Close block file earlier opened with c_block_init */
int c_block_exit( void )
{
  return close( block_fid );
}

/* RSLW */
int c_rslw(int control, int block, void *pmem )
{
    /* Did you now the real signature of `read' ? */
    ssize_t (*rslw)(int, void *, size_t ) = control? read: write ;           
    off_t where = block * KBBUF;

    return 
         block_fid <= 0 ? -1 :
         where != lseek( block_fid, where, SEEK_SET ) ? errno :
         KBBUF != (*rslw)( block_fid, pmem, KBBUF )   ? errno :
         0 ;
}

/* Perform ANSI Forth 'SYSTEM' */
/* Interpret the Forth string (`command',`count') as linux                   */
/* command and execute it.                                                   */
int c_system(int count, char command[])
{
  int i;
  char buffer[MAX_COMMAND];

  if( MAX_COMMAND-1 > count )
      return -1;

  strncpy( buffer, command, count);
  buffer[count]=0;
  buffer[count-1]=0;
  return system (buffer);
}


int main (int argc, char *argv[])
{
  BOOT_OFFSET bootmode = COLD;
  signal(SIGINT, break_int);
  signal(SIGQUIT, break_quit);


  tty_init (0, &std_in);                                                       
  if ( nodevice ) {printf("oei oei nodevice\n");/*exit(1);*/}

  for(;;)
  if ( !setjmp(restart_forth) )
  {
       printf(bootmode==COLD?"icy":bootmode==WARM?"hot":"UNKNOWN");
      figforth ( bootmode, argc, argv);
      break;
  }
  else 
  {
        bootmode = WARM;
  }
  /*
  else
  {
        printf("hot");
      figforth ( WARM, argc, argv);
  } */
  tty_restore (&std_in);                                                       
}
