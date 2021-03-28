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
        perror("args, please enter pr1 arg1 arg2 pr2 pr3 file");
        exit(111);
    }
    char* pr1 = argv[1];
    char* arg1 = argv[2];
    char* arg2 = argv[3];
    char* pr2 = argv[4];
    char* pr3 = argv[5];
    char* fl = argv[6];
    int fd[2];
    int status1;
    pipe(fd);
    pid_t p = fork();
    switch (p)
    {
    case -1:
        perror("");
        exit(2);
    case 0: //child
        dup2(fd[1], 1);
        close(fd[0]); close(fd[1]);
        execlp(pr1, pr1, arg1, arg2, NULL);
        perror("child1");
        exit(3);
    default:
        break;
    }

    p = fork();
    switch (p)
    {
    case -1:
        perror("fork1");
        exit(4);
    case 0:
        dup2(fd[0], 0);
        close(fd[0]); close(fd[1]);
        execlp(pr2, pr2, NULL);
        perror("child1");
        exit(6);
    default:
        close(fd[0]); close(fd[1]);
        status1 = 1;
        while (wait(&status1) != -1){
            if (WEXITSTATUS(status1) == 0) break; else perror("");
        }
        while (wait(&status1) != -1){
            if (WEXITSTATUS(status1) == 0) break; else perror("");
        }
        break;
    }
    int f;
    p = fork();
    switch (p)
    {
    case -1:
        perror("fork2");
        exit(8);
    case 0:
        f = open(fl, O_WRONLY | O_APPEND | O_CREAT , 0644);
        if (f < 0){
            perror("file");
            exit(9);
        }
        dup2(f, 1);
        close(f);
        execlp(pr3, pr3, NULL);
        perror("exec");
        exit(10);
    default:
        wait(NULL);
    }

    return 0;
}
