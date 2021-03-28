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
    if (argc != 7) {
        perror("args, please enter pr1 arg1 file pr2 pr3 pr4");
        exit(111);
    }
    char* pr1 = argv[1];
    char* arg1 = argv[2];
    char* fl = argv[3];
    char* pr2 = argv[4];
    char* pr3 = argv[5];
    char* pr4 = argv[6];
    int f;
    int status = 1;
    pid_t p = fork();
    switch (p)
    {
    case -1:
        perror("first fork");
        exit(2);
    case 0:
        f = open(fl, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (f < 0){
            perror(fl);
            exit(3);
        }
        dup2(f, 1);
        close(f);
        execlp(pr1, pr1, arg1, NULL);
        perror(pr1);
        exit(4);
    default:
        while(wait(&status)){
            if(WEXITSTATUS(status) == 0) break; else {
                perror("");
                exit(5);
            }
        }
    }

    int fd[2];
    pipe(fd);

    p = fork();
    switch (p)
    {
    case -1:
        perror("second fork");
        exit(6);
    case 0:
        dup2(fd[1], 1);
        close(fd[1]); close(fd[0]);
        execlp(pr2, pr2, NULL);
        perror(pr2);
        exit(7);
    default:
        dup2(fd[0], 0);
        close(fd[1]); close(fd[0]);
    }

    pipe(fd);
    switch (p = fork())
    {
    case -1:
        perror("third fork");
        exit(8);
    case 0:
        dup2(fd[1], 1); 
        close(fd[1]); close(fd[0]);
        execlp(pr3, pr3, NULL);
        perror(pr3);
        exit(9);
    default:
        dup2(fd[0], 0);
        close(fd[1]); close(fd[0]);
    }

    pipe(fd);
    switch (p = fork())
    {
    case -1:
        perror("forth fork");
        exit(10);
    case 0:
        close(fd[1]); close(fd[0]);
        f = open(fl, O_WRONLY | O_APPEND | O_CREAT, 0644);
        if (f < 0){
            perror(fl);
            exit(3);
        }
        dup2(f, 1);
        close(f);
        execlp(pr4, pr4, NULL);
        perror(pr4);
        exit(11);
    default:
        close(fd[1]); close(fd[0]);
        wait(NULL); wait(NULL); wait(NULL);
        break;
    }
    return 0;
}
