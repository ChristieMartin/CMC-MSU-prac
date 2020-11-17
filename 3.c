#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>

int nint = 1;
int ntrap = 1;

void CountInt (int s){
    printf("\n SIGINT %d \n", nint++);
    if (nint == 7) signal(SIGINT, SIG_DFL);
    
}

void CountTrap (int s){
    if ((nint > 2) && (nint < 4)) printf("\n SIGTRAP %d \n", ntrap++);
}

int main(int argc, char **argv){
    signal(SIGINT, CountInt); 
    signal(SIGTRAP, CountTrap);

    while(1);   
    return 0;
}
