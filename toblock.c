/* $Id$                   */
/* Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License */
#include <stdio.h>
#include <string.h>

#define CPL 64
void error(char *progname, char *message)
{
    printf("Error in program %s\n   %s\n",progname,message);
    exit(1);
}

int main( int argc , char **argv ) 
{
    int count = 0;
    int i;
     /* One extra for the '\n', one for the '\000', and one to detect
      * oversized lines  */
    char buffer[CPL+3];  
    int ch;

    while(NULL != fgets(buffer, CPL+3, stdin))
    {
        count = strlen(buffer);
        if ( CPL+1 < count ) error(argv[0],"line too long");
        buffer[--count] = '\000';  /* Remove '\n' */
        for(i=0; i<CPL; i++) 
            putchar(i<count?buffer[i]:' ');
    }
    return 0;
}
