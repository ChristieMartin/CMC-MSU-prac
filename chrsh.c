#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define HOMEDIR getenv("HOME")

#define BLUE "\x1b[34m"
#define GREEN "\x1b[32m"
#define PURPLE "\x1b[35m"
#define COLORENDS "\x1b[0m"


void runcommand(char** arg){
    int status = 0;
    if (fork()){
        wait(&status);
        if(status != 0) perror(arg[0]);
    } else{
        execvp(arg[0], arg);
        perror(arg[0]);
        exit(2);
    }
}

void freearr(char ** a, int i)
{
	int j;
	for (j = 0; j <= i; j++)
	{
		free(a[j]);
	}
	free(a);
}

void changedir(char* arg){
    if (arg == NULL){
        if(chdir(HOMEDIR) != 0){
            perror(HOMEDIR);
        }
    } else {
        if (strcmp(arg, "~") == 0){
            if(chdir(HOMEDIR) != 0){
                perror(arg);
            }
        } else if (chdir(arg) != 0){
                perror(arg);
            }
    }
}


char* w = NULL;



int main(int argc, char *argv[]){

    char** arg = NULL;

    //arg = malloc(sizeof(char*)*10);
    //arg = NULL;
    arg = (char**)malloc(sizeof(char*));
    int ch, size = 0, n = 0, i = 0, incommas = 0;
    int size2 = 0;
    char host[10];
    char login[10];
    char buf[1024];
    gethostname(host, 10);
    getlogin_r(login, 10);
    printf(BLUE "chrish:%s@%s " GREEN "%s " PURPLE "> " COLORENDS, login, host, getcwd(buf, sizeof(buf)));
    //printf("chrish %s> ", getcwd(NULL, 0));
    while ((ch = getchar()) != EOF){
        if (size2 <= i){
            size2 = size2 * 2 + 1;
            arg = realloc(arg, size2*sizeof(char*));
            if (arg == NULL){
                fprintf(stderr, "memory error1\n");
                return 3;
            } 
        }
        if (size <= n){
            size = 2 * size + 1;
            w = realloc(w, size);
            if (w == NULL) {
                fprintf(stderr, "memory error2\n");
                return 3;
            }
        }
        
        if (ch == '\n'){
            if (n > 0){
                if (size < n + 1){
                    w = realloc(w, n + 1);
                }
                w[n] = '\0';
                arg[i] = strdup(w);
                free(w);
            }
            if (strcmp(arg[0], "exit") == 0) {
                free(arg[0]);
                free(arg);
                return 0;
            } else
            if (strcmp(arg[0], "cd") == 0){
                changedir(arg[1]);
            } else {
                arg[i + 1] = NULL;
                if (incommas == 0) runcommand(arg); else fprintf(stderr,"comma missing\n");
            }
            
            printf(BLUE "chrish:%s@%s " GREEN "%s " PURPLE "> " COLORENDS, login, host, getcwd(buf, sizeof(buf)));

            freearr(arg, i + 1);

            arg = malloc(1);
            w = malloc(1);
            i = incommas = n = size = 0;
        } else
            if (ch == '"') {
                if (incommas == 0) incommas = 1; else incommas = 0;
            }   else
                    if (isspace(ch) && (incommas == 0)){
                        if(n > 0){
                            if (size < n + 1){
                               w = realloc(w, n + 1);
                               if (w == NULL){
                                    fprintf(stderr, "memory error3\n");
                                    return 3;
                               }
                            }
                            w[n] = '\0';
                            arg[i] = strdup(w);
                            i++;
                            n = size = 0;
                            free(w);
                            w = malloc(1);
                        }
                    } else {
                        w[n] = ch;
                        n++;
                    }
    }
    if (n > 0){
        if (size < n + 1){
            w = realloc(w, n+1);
        }
        w[n] = '\0';
        arg[i] = strdup(w);
        if (strcmp(arg[0], "exit") == 0) return 0;
        printf("\n");
        arg[i+1] = NULL;
        if (incommas == 0) runcommand(arg); else fprintf(stderr,"comma missing\n");
        
    }

    freearr(arg, i+1);
    free(w);
    printf("\n");
    return 0;
}
