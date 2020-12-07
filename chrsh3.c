#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>

#define HOMEDIR getenv("HOME")

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"

#define BALD           "\x1b[1m"

#define BACKGROUND_RED    "\x1b[48;2;110;20;12m"
#define BACKGROUND_PURPLE "\x1b[48;2;103;74;12m"

#define _OUT  1
#define _AOUT 2
#define _IN   3
#define _PIPE 4
#define _PIPE 4
#define _BACK 5
#define _STOP 6
#define _OR   7
#define _AND  8

struct shell{
    char ***arg; 
    int len;
    int *llen;
    char **metas;
    int lenm;
    int bgflag;
};

int* pids;
int pidl;

void changedir(char* arg){
    if (arg == NULL){
        if(chdir(HOMEDIR) != 0){
            fprintf(stderr, BACKGROUND_RED BALD WHITE" changedir: error(homedir): "COLORENDS " ");
            perror(HOMEDIR);
        }
    } else {
        if (strcmp(arg, "~") == 0){
            if(chdir(HOMEDIR) != 0){
                fprintf(stderr, BACKGROUND_RED BALD WHITE" changedir: error(tilda): "COLORENDS " ");
                perror(arg);
            }
        } else if (chdir(arg) != 0){
                fprintf(stderr, BACKGROUND_RED BALD WHITE" changedir: error(arg): "COLORENDS " ");
                perror(arg);
            }
    }
}

void freearr(char** a, int i){
	int j;
	for (j = 0; j < i; j++){
		free(a[j]);
	}
	free(a);
}

void printarr(char** arr, int i){
    int j;
	for (j = 0; j <= i; j++)
	{
		printf("__%s__ ", arr[j]);
	}
    printf("\n");
}

int isinstr(char a, const char* s){
    int i;
    for(i = 0; i < sizeof(s); i++){
        if(s[i] == a) return 1;
    }
    return 0;
}

void add(char* w, char** arg, int size, int n, int* i){
    if (size < n + 1){
        w = realloc(w, n + 1);
    }
    w[n] = '\0';
    arg[*i] = strdup(w);
    
    (*i)++;
}

void deletestr(char** w, int* n, int* size){
    free(*w);
    *n = *size = 0;
    *w = malloc(1);
}

int checksizes(char** w, int* size, int n, char*** arg, int* size2, int i){
    if (*size2 <= i){
        *size2 = (*size2) * 64 + 1;
        *arg = realloc(*arg, (*size2)*sizeof(char**));
        if (*arg == NULL){
            fprintf(stderr, BACKGROUND_RED BALD RED" checksizes: memory error1 "COLORENDS " \n");
            return 1;
        } 
    }
    if (*size <= n){
        *size = 2 * (*size) + 1;
        *w = realloc(*w, (*size));
        if (*w == NULL) {
            fprintf(stderr, BACKGROUND_RED BALD RED" checksizes: memory error2 "COLORENDS " \n");
            return 1;
        }
    }
    return 0;
}

int checkpid(int pid){
    int f = -1;
    int i;
    for (i = 0; i < pidl; i++){
        if (pids[i] == pid) {
            f = i;
            break;
        }
    }
    if (f >= 0){
        pidl--;
        for (i = f; i < pidl; i++){
            pids[i] = pids[i + 1];
        }
        return f + 1;
    }
    return -1;
}

void ChildDead (int s){
    if(pidl > 0){
        int pid, status, k;
        while ((pid = waitpid(-1, &status, 1)) > 0){
            if ((k = checkpid(pid)) >= 0) {
                if (WIFEXITED(status)) {
                    printf("\n[%d] %d Done (%d)\n", k, pid, status);
                } else if (WIFSIGNALED(status)){
                    printf("\n[%d] %d Killed (%d)\n", k, pid, status);
                } else if (WIFSTOPPED(status)){
                    printf("\n[%d] %d Stopped (%d)\n", k, pid, status);
                } else if (WIFCONTINUED(status)){
                    printf("\n[%d] %d Continued (%d)\n", k, pid, status);
                } else printf("\n[%d] %d Exited (%d)\n", k, pid, status);
            } else printf("%d\n", pid);
        }
    }
}



void redir(char** progname, char* arrow, char* filename){
    int f;
    pid_t p = fork();
    int i = 0;
    switch (p){
    case -1:
        fprintf(stderr, BACKGROUND_RED BALD WHITE" redir: error(fork): "COLORENDS " ");
        perror(progname[0]);
        exit(2);
    case 0://child
        if (strcmp(arrow, ">") == 0){
            f = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            i = 1;
        } else if (strcmp(arrow, "<") == 0){
            f = open(filename, O_RDONLY);
            i = 0;
        } else if (strcmp(arrow, ">>") == 0){
            f = open(filename, O_WRONLY | O_APPEND | O_CREAT , 0644);
            i = 1;
        }
        if (f < 0){
            fprintf(stderr, BACKGROUND_RED BALD WHITE" redir: error(file): "COLORENDS " ");
            perror(filename);
            exit(3);
        }
        dup2(f, i);
        close(f);
        execvp(progname[0], progname);
        fprintf(stderr, BACKGROUND_RED BALD WHITE" redir: error(child): "COLORENDS " ");
        perror(progname[0]);
        exit(4);
    default://parent
        while(wait(NULL) != -1);
    }

}

int checkmetas(char* arg){
    if (arg == NULL) return 0;
    if (strcmp(arg, ">") == 0){
        return _OUT;
    } else if (strcmp(arg, ">>") == 0){
        return _AOUT;
    } else if (strcmp(arg, "<") == 0){
        return _IN;
    } else if (strcmp(arg, "|") == 0){
        return _PIPE;
    } else if (strcmp(arg, "&") == 0){
        return _BACK;
    } else if (strcmp(arg, ";") == 0){
        return _STOP;
    } else if (strcmp(arg, "||") == 0){
        return _OR;
    } else if (strcmp(arg, "&&") == 0){
        return _AND;
    }
    return 9;
}

void pipeline(struct shell* sh){
    int i = 0, j = 0, k = 0, met = 0;
    int pid, fd[2],  file, flag;
    while (i <= sh -> lenm){
        if(checkmetas(sh -> metas[i]) == _PIPE || i == sh -> lenm){
            pipe(fd);
            switch (pid = fork()){
            case -1:
                fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: fork error: "COLORENDS " ");
                exit(2);
            case 0:
                if (i != sh -> lenm) dup2(fd[1], 1);
                close(fd[0]); close(fd[1]);
                j = k;
                while(j != (sh -> lenm) && (met = checkmetas(sh -> metas[j])) != _PIPE && met != _BACK){
                    if (met == _OUT){
                        file = open(sh -> arg[j + 1][0], O_WRONLY | O_CREAT | O_TRUNC, 0644);
                        if(file < 0) fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: file error(child): "COLORENDS " ");
                        else flag = 1;
                    }
                    else if (met == _AOUT){
                        file = open(sh -> arg[j + 1][0], O_WRONLY | O_CREAT | O_APPEND, 0644);
                        if(file < 0) fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: file error(child): "COLORENDS " ");
                        else flag = 1;
                    }
                    else if (met == _IN){
                        file = open(sh -> arg[j + 1][0], O_RDONLY);
                        if(file < 0) fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: file error(child): "COLORENDS " ");
                        else flag = 0;
                    } else {
                        fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: syntax error(child): "COLORENDS " ");
                        perror(sh -> metas[j]);
                        exit(2);
                    }
                    dup2(file, flag);
                    close(file);
                    j++;
                }
                execvp(sh -> arg[k][0], sh -> arg[k]);
                fprintf(stderr, BACKGROUND_RED BALD WHITE" pipeline: exec error(child): "COLORENDS " ");
                perror(sh -> arg[k][0]);
                exit(1);
            }
            dup2(fd[0], 0);
            close(fd[0]); close(fd[1]);
            k = i + 1;
        }
        i++;
    }
    while(wait(NULL) != -1);
}

void runcommand(struct shell* sh){
    int status = 0;
    int met = 0;
    if (sh -> lenm == 0){
        if (fork()){
            wait(&status);
            if(status != 0) {
                fprintf(stderr, BACKGROUND_RED BALD WHITE" runcommand: error(parent): "COLORENDS " ");
                perror(sh -> arg[0][0]);
            } 
        } else{
            
            execvp(sh -> arg[0][0], sh -> arg[0]);
            //execv(getenv("PATH"),sh -> arg);
            fprintf(stderr, BACKGROUND_RED BALD WHITE" runcommand: error(child): "COLORENDS " ");
            perror(sh -> arg[0][0]);
            exit(2);
        }
    } else {
        int i = 0;
        int j = 0;
        pid_t pid;
        int bgflag = 0;
        if ((met = checkmetas(sh -> metas[i++])) != 0){
            
            switch (met){
            case _OUT: case _AOUT: // > >>
                while(checkmetas(sh -> metas[i]) == _OUT) i++;
                redir(sh -> arg[j], sh -> metas[i - 1], sh -> arg[i][0]);
                break;
            case _IN: // <
                
                while(checkmetas(sh -> metas[i]) == _IN) i++;
                if (checkmetas(sh -> metas[i]) == _PIPE){
                    if ((pid = fork())){
                        if(!(sh -> bgflag)) while (wait(NULL) != -1); else {
                            pids[pidl] = pid;
                            pidl++;
                            printf("[%d] %d Starts\n", pidl, pid);
                            sh -> bgflag = 0;
                        }
                    } else {
                        pipeline(sh);
                        exit(0); 
                    }
                } else{
                    redir(sh -> arg[j], sh -> metas[i - 1], sh -> arg[i][0]);
                }
                break;
            case _PIPE: // |
                if ((pid = fork())){
                    if(!(sh -> bgflag)) while (wait(NULL) != -1); else {
                        pids[pidl] = pid;
                        pidl++;
                        printf("[%d] %d Starts\n", pidl, pid);
                        sh -> bgflag = 0;
                    }
                } else {
                    pipeline(sh);
                    exit(0);
                }
                break;
            case _BACK:
                if ((pid = fork())){
                    pids[pidl] = pid;
                    pidl++;
                    printf("[%d] %d Starts\n", pidl, pid);
                } else {
                    execvp(sh -> arg[i - 1][j], sh -> arg[i - 1]);
                    fprintf(stderr, BACKGROUND_RED BALD WHITE" runcommand: error(child): "COLORENDS " ");
                    perror(sh -> arg[i - 1][j]);
                    exit(2);
                }
            default:
                break;
            }
        } 
    }
}

char host[128];
struct shell *sh = NULL;
char* w = NULL;


void printway(char* host){
    char buf[1024];
    getcwd(buf, sizeof(buf));
    printf(BALD BLUE "chrish:@%s " GREEN "%s " PURPLE "> " COLORENDS, host, buf);
}

void KillChild(int s);


const char meta[] = {'>', '<', '|', '&', '(', ')', ';', '$'};

int ch, size = 0, n = 0, inquotes = 0, size2 = 1, k = 0, j = 0, i = 0;

int init(){


    sh =(struct shell*)malloc(sizeof(*sh));
    
    //sh -> arg = NULL;
    sh -> arg = (char***) malloc(20 * sizeof(char**));
    sh -> arg[0] = (char**) malloc(64 * sizeof(char*));
    sh -> metas = (char**) malloc(64 * sizeof(char*));
    sh -> llen = (int*) malloc(64 * sizeof(int));
    sh -> len = 0;
    sh -> lenm = 0;
    sh -> bgflag = 0;

    pidl = 0;
    pids = (int*) malloc(64 * sizeof(int));

    
    
    //char buf[1024];
    gethostname(host, sizeof(host));
    printway(host);

    while ((ch = getchar()) != EOF){
        if (checksizes(&w, &size, n, &(sh -> arg[j]), &size2, i)) return 4;
        if (ch == '\n' && inquotes == 1) printf(PURPLE ">" COLORENDS " ");
        if (ch == '\n' && inquotes == 0){
            if (n == 0 && i == 0) {
                printway(host);
                continue;
            }
            if (n > 0) add(w, sh -> arg[j], size, n, &i);
            if (strcmp((sh -> arg[0])[0], "exit") == 0) {
                free((sh -> arg[0])[0]);
                free(sh -> arg[0]);
                free(sh -> arg);
                free(sh -> llen);
                free(sh -> metas);
                signal(SIGINT, SIG_DFL); 
                if (pidl != 0){
                    for (i = 0; i < pidl; i++) kill(pids[i], SIGINT);
                }
                free(pids);
                free(sh);
                free(w);
                exit(0);
            } else
            if (strcmp((sh -> arg[0])[0], "cd") == 0){
                changedir((sh -> arg[0])[1]);
            } else {
                (sh -> arg[j])[i] = NULL;
                sh -> llen[j] = i;
                sh -> len = j;
                runcommand(sh);
            }
            printway(host);
            sh -> metas[sh -> lenm] = NULL;
            freearr(sh -> metas, sh -> lenm + 1);
            for (k = 0; k <= j; k++) {
                freearr((sh -> arg[k]), sh -> llen[k] + 1);
                sh -> llen[k] = 0;
            }
            free(sh -> arg);
            deletestr(&w, &n, &size);

            j = sh -> len = sh -> lenm = i = inquotes =  0;
            
            sh -> arg = (char***) malloc(64 * sizeof(char**));
            sh -> arg[j] = (char**) malloc(64 * sizeof(char*));
            sh -> metas = (char**) malloc(64* sizeof(char*));
        } else
        if (ch == '"') {
            if (inquotes == 0) inquotes = 1; else inquotes = 0;
        } else
        if ((isspace(ch) || isinstr(ch, meta)) && (inquotes == 0)){
            if(n > 0){
                add(w, (sh -> arg[j]), size, n, &i);
                deletestr(&w, &n, &size);
            }
            if (isinstr(ch, meta)){
                if (ch == '&') sh -> bgflag = 1;
                int t;
                char formeta[3];
                t = getchar();
                formeta[0] = ch;
                (sh -> arg[j])[i] = NULL;
                sh -> llen[j] = i;
                j++;
                i = size2 = 0;
                checksizes(&w, &size, n, &(sh -> arg[j]), &size2, i);
                if (t == EOF || t == '\n') {
                    if (t == EOF) break;
                    add(formeta, (sh -> metas), 3, 1, &(sh ->lenm));
                    (sh -> arg[j])[i] = NULL;

                    //printarr((sh -> arg[j]), i);
                    if (ch == '|') {
                        printf(PURPLE ">" COLORENDS " ");
                        continue;
                    }
                    runcommand(sh);

                    printway(host);

                    freearr(sh -> metas, sh -> lenm + 1);
                    for (k = 0; k <= j; k++) {
                        freearr((sh -> arg[k]), sh -> llen[k] + 1);
                        sh -> llen[k] = 0;
                    }
                    free(sh -> arg);
                    deletestr(&w, &n, &size);

                    j = sh -> len = sh -> lenm = i = inquotes =  0;

                    sh -> arg = (char***) malloc(20 * sizeof(char**));
                    sh -> arg[j] = (char**) malloc(64 * sizeof(char*));
                    sh -> metas = (char**) malloc(64 * sizeof(char*));

                    formeta[0] = formeta[1] = formeta[2] = '\0';
                } else {
                    if (isinstr(t, meta)){
                        if (ch == t){
                            formeta[1] = t;
                            add(formeta, sh -> metas, 3, 2, &(sh ->lenm));
                            formeta[0] = formeta[1] = formeta[2] = '\0';
                        } else {
                            add(formeta, sh -> metas, 3, 1, &(sh ->lenm));
                            formeta[0] = t;
                            add(formeta, sh -> metas, 3, 1, &(sh ->lenm));
                            formeta[0] = formeta[1] = formeta[2] = '\0';
                        }
                    } else {
                            add(formeta, sh -> metas, 3, 1, &(sh ->lenm));
                            formeta[0] = formeta[1] = formeta[2] = '\0';
                            if (isspace(t)) continue;
                            size = sizeof(t);
                            w = realloc(w, size);
                            if (w == NULL){
                                fprintf(stderr, BACKGROUND_RED BALD RED"memory error3"COLORENDS "\n");
                                return 6;
                            }
                            w[n++] = t;
                        }
                 }
            }
        } else {
            w[n++] = ch;
        }
    }
    if (n > 0){
        add(w, sh -> arg[j], size, n, &i);
        if (strcmp((sh -> arg[j])[0], "exit") == 0) {
            free((sh -> arg[0])[0]);
            free(sh -> arg[0]);
            free(sh -> arg);
            free(sh -> llen);
            free(sh -> metas);
            free(sh);
            free(w);
            exit(0);
        }
        printf("\n");
        (sh -> arg[j])[i] = NULL;
        if (inquotes == 0) {
            //printarr(sh -> arg[j], i);
            runcommand(sh);
        } else fprintf(stderr,BALD RED"quote not closed\n"COLORENDS);
        
    }

    free((sh -> arg[0])[0]);
    free(sh -> arg[0]);
    free(sh -> arg);
    free(sh -> llen);
    free(sh -> metas);
    free(sh);
    free(w);
    printf("\n");
    exit(0);
}

void KillChild(int s){
    signal(SIGINT, KillChild); 
    printf("\n");
    if (sh -> lenm != 0) freearr(sh -> metas, sh -> lenm + 1);
    for (k = 0; k <= j; k++) {
        freearr((sh -> arg[k]), sh -> llen[k] + 1);
        sh -> llen[k] = 0;
    }
    free(sh -> arg);
    deletestr(&w, &n, &size);

    j = sh -> len = sh -> lenm = i = inquotes = size2 = 0;

    free(w);
    free(sh -> metas);
    sh -> arg = (char***) malloc(64 * sizeof(char**));
    sh -> arg[j] = (char**) malloc(64 * sizeof(char*));
    sh -> metas = (char**) malloc(64* sizeof(char*));
    w = (char *)malloc(64 * sizeof(char));
    checksizes(&w, &size, n, &(sh -> arg[j]), &size2, i);
    //printway(host);
}

int main(){
    signal(SIGINT, KillChild); 
    signal(SIGCHLD, ChildDead); 
    init();
    return 0;
}


