/* Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License */
#include <stdio.h>
#include <stdlib.h>
#include <sys/times.h>
#include "tty.c"

void (*figforth)() = 0x8050000;

/* This is absolutely necessary, but I have no clue. */
char reservespace[1000000];

struct tms tm;
struct tty std_in;
char str[256];

/* Assume the terminal understands TERM=Linux or TERM=X console codes */

enum
  {
    cursor_home,
    cursor_left,
    cursor_right,
    cursor_up,
    cursor_down,
    clear_screen,
    clr_eos,
    clr_eol,
    bell,
    delete_character,
    delete_line,
    scroll_forward,
    scroll_reverse,
    enter_standout_mode,
    exit_standout_mode,
    enter_underline_mode,
    exit_underline_mode,
    enter_bold_mode,
    enter_reverse_mode,
    enter_blink_mode,
    exit_attribute_mode
  };

static char *control_string[] =	/* Some hardcoded console cmds (Linux/XTerm) */
{"\033[H",			/* ho - home position */
 "\b",				/* le - cursor left */
 "\033[C",			/* nd - right one column */
 "\033[A",			/* up - up one column */
 "\n",				/* do - down one column */

 "\033[H\033[J",		/* cl - clear screen and home */
 "\033[J",			/* cd - clear down */
 "\033[K",			/* ce - clear to end of line */
 "\a",				/* bl - bell */

 "\033[P",			/* dc - delete character in line */
 "\033[M",			/* dl - delete line from screen */

 "\033D",			/* sf - scroll screen up (XTerm ??) */
 "\033M",			/* sr - scroll screen down */

 "\033[7m",			/* so - enter standout mode */
 "\033[27m",			/* se - leave standout mode */
 "\033[4m",			/* us - turn on underline mode */
 "\033[m",			/* ue - turn off underline mode */

 "\033[1m",			/* md - enter double bright mode */
 "\033[7m",			/* mr - enter reverse video mode */
 "\033[5m",			/* mb - enter blinking mode (XTerm??) */
 "\033[m"			/* me - turn off all appearance modes */
};

char **inkeys;

char *linux_rawkey_string[] =	/* Strings sent by function keys */
{"\033[[A",			/* k1 - function keys 1 - 4 */
 "\033[[B",			/* k2 */
 "\033[[C",			/* k3 */
 "\033[[D",			/* k4 */
 "\033[[E",			/* k5 */
 "\033[17~",			/* k6 */
 "\033[18~",			/* k7 */
 "\033[19~",			/* k8 */
 "\033[20~",			/* k9 */
 "\033[21~",			/* k0 */

 "\033[D",			/* kl - arrow left */
 "\033[C",			/* kr - arrow right */
 "\033[A",			/* ku - arrow up */
 "\033[B",			/* kd - arrow down */

 "\033[1~",			/* kh - home key */
 "\033[4~",			/* kH - home down key (end key) */
 "\033[6~",			/* kN - next page */
 "\033[5~",			/* kP - previous page */

 "\b",				/* kb - backspace key */
 "\033[3~",			/* kD - delete character key */
 "\033[2~"			/* kI - insert character key */
};				/* 21 strings */

char *xterm_rawkey_string[] =	/* Strings sent by function keys */
{
  "\033[11~",			/* k1 - function keys 1 - 4 */
  "\033[12~",			/* k2 */
  "\033[13~",			/* k3 */
  "\033[14~",			/* k4 */
  "\033[15~",			/* k5 */
  "\033[17~",			/* k6 */
  "\033[18~",			/* k7 */
  "\033[19~",			/* k8 */
  "\033[20~",			/* k9 */
  "\033[21~",			/* k0 */

  "\033OD",			/* kl - arrow left */
  "\033OC",			/* kr - arrow right */
  "\033OA",			/* ku - arrow up */
  "\033OB",			/* kd - arrow down */

  "\033[1~",			/* kh - home key ?? */
  "\033[4~",			/* kH - end key ?? */
  "\033[6~",			/* kN - next page */
  "\033[5~",			/* kP - previous page */

  "\b",				/* kb - backspace key */
  "\033[3~",			/* kD - delete character key ?? */
  "\033[2~"			/* kI - insert character key */
};				/* 21 strings */

char buf[257];

int 
identify (void)
{
  int i, index;			/* only called when nodevice=0 */
  buf[0] = 0x1B;
  i = 1;
  do
    {
      read (0, &buf[i], 1);
      buf[i + 1] = '\0';
      i++;
      for (index = 0; index < 21; index++)
	if (strcmp (inkeys[index], buf) == 0)
	  break;		/* recognized string */
    }
  while (keyq (0) && (i < 256));
  if (i == 256)
    return 0x1B;
  else
    return index;
}

int 
qkey (void)
{
  char buf;
  if (nodevice)
    {
      fread (&buf, 1, 1, stdin);
      return buf;
    }
  if (!keyq (0))
    return EOF;
  read (0, &buf, 1);
  if (buf == 8)
    return 127;			/* ^H -> delete */
  if (buf != 0x1B)
    return buf;			/* only one */
  return (0x100 | identify ());	/* ESC followed by something else */
}

static int row, col;		/* track cursor position */

void 
emit (int ch)
{
  fputc (ch, stdout);
  fflush (stdout);
}

void 
type (int count, char *addr)
{
  fwrite (addr, 1, count, stdout);
  col = col + count;
  fflush (stdout);
}

int 
shell (int count, char addr[])
{
  int i;
  str[0] = '\0';
  count &= 0xFF;
  if (count)
    for (i = 0; i < count; i++)
      str[i] = addr[i];
  str[i] = '\0';
  return system (str);
}

typedef int FUNC ();		/* array with I/O functions */
FUNC *call[256] =
{
  0,                      /* eForth itself */
  qkey,				/*  1   */
  emit,				/*  2   */
  0,                            /*  3   */
  type,				/*  4   */
  shell,			/*  5   */
/* REMAINDER NOT USED IN EFORTH */
};

int 
main (int argc, char *argv[])
{
  FILE *in;
  if ((in = fopen ("fig86.linux.bin", "rb")) == NULL)
    {
      printf ("Problem opening fig86.linux.bin\n");
      exit (1);
    }
  fread ((char *) figforth, 1, 1000000, in);
  fclose (in);
/*printf ("[0x%x] ", figforth);                                              */
  tty_init (0, &std_in);
  tty_keymode (&std_in);
  tty_noecho (&std_in);
  if (!nodevice)
    if (strcmp (getenv ("TERM"), "linux"))
      {
	inkeys = xterm_rawkey_string;
      }
    else
      {
	inkeys = linux_rawkey_string;
      }
  figforth (argc, argv, &call);
  tty_restore (&std_in);
}
