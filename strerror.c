
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    printf("%s\n",strerror(atoi(argv[1])));
    return 0;
}


