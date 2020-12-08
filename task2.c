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

int main(int argc, char *argv[])
{   
    if (argc < 1){
        perror("args");
        exit(3);
    }
    int n = atoi(argv[1]);
    char** args = (char**) malloc((n + 2) * sizeof(char *));
    args[0] = "echo";
    for (int i = 1; i <= n; i++){
        args[i] = (char *) malloc(64);
        sprintf(args[i], "%d", i);
    }
    args[n + 1] = NULL;
    if (fork()){
        wait(NULL);
    } else {
        execvp(args[0], args);
        perror("error");
        exit(4);
    }
    free(args);
    return 0;
}
