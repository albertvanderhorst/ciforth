/* $Id$                   */
/* Copyright(2000): Albert van der Horst, HCC FIG Holland by GNU Public License */
#include <stdio.h>
#include <string.h>

main(char **argv, int argc)
{
    int count = 0;
    int ch;

    while(EOF !=(ch=getchar()))
    {
        putchar(isprint(ch)?ch:' ');
        if ( 64 == ++count)
        {
            putchar('\n');
            count = 0;
        }
    }
    return 0;
}
