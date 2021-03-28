#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>


int main(int argc, char const *argv[]){
    if (argc != 6) {
        perror("args, please enter pr1 file1 pr2 file2 pr3");
        exit(111);
    }
    char* pr1 = argv[1];
    char* fl1 = argv[2];
    char* pr2 = argv[3];
    char* fl2 = argv[4];
    char* pr3 = argv[5];
    int f;
    int fd[2];
    int status = 1;
    pipe(fd);
    pid_t p = fork();
    switch (p){
    case -1:
        perror("fork1");
        exit(2);
    case 0:
        dup2(fd[1], 1);
        close(fd[0]); close(fd[1]);
        f = open(fl1, O_RDONLY);
        dup2(f, 0);
        close(f);
        execlp(pr1, pr1, NULL);
        perror(pr1);
        return 2;
    default:
        break;
    }

    p = fork();
    switch (p){
    case -1:
        perror("fork2");
        return 5;
    case 0:
        dup2(fd[0], 0);
        close(fd[0]); close(fd[1]);
        f = open(fl2, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (f < 0){
            perror("file");
            return 19;
        }
        dup2(f, 1);
        close(f);
        execlp(pr2, pr2, NULL);
        perror(pr2);
        return 6;
    default:
        close(fd[0]); close(fd[1]);
        while (wait(&status) != -1){
            if (WEXITSTATUS(status) == 0) break; else {
                perror("status1");
                return 7;
            }
        }
        while (wait(&status) != -1){
            if (WEXITSTATUS(status) == 0) break; else {
                perror("status2");
                return 8;
            }
        }
        break;
    }

    p = fork();
    switch (p){
    case -1:
        perror("fork3");
        return 9;
    case 0:
        execlp(pr3, pr3, NULL);
        perror(pr3);
        return 10;
    default:
        wait(NULL);
    }
    return 0;
}
