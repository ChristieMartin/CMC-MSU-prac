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

int main(int argc, char *argv[]){

    if (argc != 5) {
        perror("args, please enter pr1 pr2 pr3 fname");
        exit(5);
    }
    char* pr1 = argv[1];
    char* pr2 = argv[2];
    char* pr3 = argv[3];
    char* fname = argv[4];
    int f;
    int fd[2];
    int status;
    pipe(fd);
    pid_t p = fork();
    switch (p){
    case -1:
        perror("fork");
        exit(1);
    case 0:
        dup2(fd[1], 1);
        close(fd[0]); close(fd[1]);
        f = open(fname, O_RDONLY);
        if (f < 0){
            perror(fname);
            exit(9);
        }
        dup2(f, 0);
        close(f);
        execlp(pr1, pr1, NULL);
        perror(pr1);
        exit(2);
    default:
        break;
    }
    p = fork();
    switch (p)
    {
    case -1:
        perror("fork2");
        exit(2);
    case 0:
        dup2(fd[0], 0);
        close(fd[0]); close(fd[1]);
        p = fork();
        
        switch (p)
        {
        case -1:
            perror("fork3");
            exit(2);
            break;
        case 0:
            execlp(pr2, pr2, NULL);
            perror(pr2);
            exit(6);
        }
        wait(&status); 

        if (WIFEXITED(status)){
            p = fork();
            switch (p)
            {
            case -1:
                perror("fork3");
                exit(2);
                break;
            case 0:
                dup2(fd[0], 0);
                close(fd[0]); close(fd[1]);
                execlp(pr3, pr3, NULL);
                perror(pr3);
                exit(6);
            }
            //dup2(fd[0], 0);
            //close(fd[0]); close(fd[1]);
            //execlp(pr2, pr2, NULL);
            //perror(pr2);
            //exit(4);
        }
    default:
        close(fd[0]); close(fd[1]);
        wait(NULL); wait(NULL);
        break;
    }
    return 0;
}
