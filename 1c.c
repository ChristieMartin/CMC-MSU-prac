#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>

int main(int argc, char *argv[]){
    int i = 0, fd[2];
    pid_t pid = fork();
    switch (pid){
    case -1:
        perror("fork1");
        exit(2);
    case 0:
        while (argv[++i] != NULL) {
            pipe(fd);
            switch (pid = fork())
            {
            case -1:
                perror("fork");
                exit(3);
            case 0:
                if (i + 1 != argc) dup2(fd[1], 1);
                close(fd[0]); close(fd[1]);
                execlp(argv[i], argv[i], NULL);
                perror(argv[i]);
                exit(4);
            }
            dup2(fd[0], 0);
            close(fd[0]); close(fd[1]);
        }
    default:
        while(wait(NULL) != -1);
    }
    return 0;
}
