/* Program to strip the 8th bits from a file */
#include <stdio.h>

int atoi();

main(argv, argc)
char *argv[];
int argc;
{
    int i;

    while(EOF!=(i=getchar()) )
        if ( EOF==putchar(i&0x7f) )
        {
           fprintf(stderr, "Write error in strip8\nProbably disk full\n");
           exit(1);
       }
   exit(0);
}
