/* tty.h */
 
#include <termios.h> 
 
/* public declarations */ 
struct ttystate { struct termios tio; }; 
struct tty      { int fd; struct ttystate org; struct ttystate now; };  
 
/* public functions */ 
void tty_init(int fd,struct tty *ptty);  
void tty_restore(struct tty *ptty);  
void tty_keymode(struct tty *ptty); 
void tty_linemode(struct tty *ptty); 
void tty_echo(struct tty *ptty); 
void tty_noecho(struct tty *ptty); 
int  tty_keyq(struct tty *ptty); 
int  keyq(int fd);
