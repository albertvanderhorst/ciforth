/* Copyright (2000): Albert van der Horst, HCC FIG Holland by GNU Public License */
#include <stdio.h>
#include <stdlib.h>
#include <sys/times.h>
#include "tty.c"

#define EFORTH 0x8050000

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

void 
t_puts (int cap)
{
  fputs (control_string[cap], stdout);
}

int 
get_ticks (void)
{
  times (&tm);
  return tm.tms_utime + tm.tms_stime;
}				/* 10 ms ticks */

/******************************************
 * Output to screen, control with termcap *
 ******************************************/

static int rows = 25, cols = 80, row, col;

void 
gotoxy (int x, int y)
{
  col = x;
  row = y;
  printf ("\033[%d;%dH", x + 1, y + 1);
  fflush (stdout);
}

int 
wherexy (void)
{
  return ((col << 16) | (row & 0xFFFF));
}

void 
home (void)
{
  t_puts (cursor_home);
  row = col = 0;
}
void 
clrscr (void)
{
  t_puts (clear_screen);
  home ();
}
void 
clreol (void)
{
  t_puts (clr_eol);
}
void 
clrdown (void)
{
  t_puts (clr_eos);
}
void 
tbell (void)
{
  t_puts (bell);
}

void 
standout_on (void)
{
  t_puts (enter_standout_mode);
}
void 
standout_off (void)
{
  t_puts (exit_standout_mode);
}
void 
underline_on (void)
{
  t_puts (enter_underline_mode);
}
void 
underline_off (void)
{
  t_puts (exit_underline_mode);
}
void 
bright (void)
{
  t_puts (enter_bold_mode);
}
void 
reverse (void)
{
  t_puts (enter_reverse_mode);
}
void 
blinking (void)
{
  t_puts (enter_blink_mode);
}
void 
normal (void)
{
  t_puts (exit_attribute_mode);
}

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
  switch (ch)
    {
    case 7:
      fputc ('\a', stdout);
      break;			/* bell */
    case 127:			/* delete key */
    case 8:
      fputc ('\b', stdout);
      if (col > 0)
	col--;
      break;			/* backspace */
    case 9:
      fputc ('\t', stdout);
      col = ((col / 8) + 1) * 8;
      break;			/* tab */
    case 10:
      fputc ('\n', stdout);
      col = 0;
      if (row < rows - 1)
	row++;
      break;			/* line feed */
    case 12:
      clrscr ();
      break;			/* form feed */
    case 13:
      fputc ('\r', stdout);
      col = 0;
      break;			/* carriage return */
    default:
      fputc (ch, stdout);
      if (col < cols - 1)
	col++;
    }
  fflush (stdout);
}

void 
goaway (int retval)
{
  tty_restore (&std_in);
  exit (retval);
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

int 
qkb (void)
{
  return !nodevice;
}

typedef int FUNC ();		/* array with I/O functions */
FUNC *call[256] =
{
  0,				/* eForth itself */
  qkey,				/*  1   */
  emit,				/*  2   */
  goaway,			/*  3   */
  type,				/*  4   */
  shell,			/*  5   */
/* REMAINDER NOT USED IN EFORTH */
  qkb,				/*  6   */
  home,				/*  7   cursor_home */
  clrscr,			/*  8   clear_screen */
  clreol,			/*  9   clr_eol */
  clrdown,			/* 10   clr_eos */
  tbell,			/* 11   bell */
  standout_on,			/* 12   enter_standout_mode */
  standout_off,			/* 13   exit_standout_mode */
  underline_on,			/* 14   enter_underline_mode */
  underline_off,		/* 15   exit_underline_mode */
  bright,			/* 16   enter_bold_mode */
  reverse,			/* 17   enter_reverse_mode */
  blinking,			/* 18   enter_blink_mode */
  normal,			/* 19   exit_attribute_mode */
  gotoxy,			/* 20   */
  wherexy,			/* 21   */
  get_ticks			/* 22   */
};

char *eforth[1000000];		/* array with eForth binary */

int 
main (int argc, char *argv[])
{
  FILE *in;
  if ((in = fopen ("fig86.linux.bin", "rb")) == NULL)
    {
      printf ("Problem opening fig86.linux.bin\n");
      exit (1);
    }
  fread ((char *) EFORTH, 1, 1000000, in);
  fclose (in);
  printf ("[0x%x] ", eforth);
  call[0] = (FUNC *) EFORTH;
  tty_init (0, &std_in);
  tty_keymode (&std_in);
  tty_noecho (&std_in);
  if (!nodevice)
    if (strcmp (getenv ("TERM"), "linux"))
      {
	inkeys = xterm_rawkey_string;
	rows = 24;
      }
    else
      {
	inkeys = linux_rawkey_string;
	rows = 25;
      }
  call[0] (argc, argv, &call);
  goaway(0);
}
