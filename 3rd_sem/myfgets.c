#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
/*
char *fgets(char *s, int size, FILE *stream);

fgets()  reads in at most one less than size characters from stream and stores them into the buffer pointed to by s.  Reading stops after an EOF or a newline.  If a newline is read, it is stored into the buffer.  A terminating null byte ('\0') is stored after the last character in the buffer.
*/

char *myfgets(int fd, char *pBuffer, size_t size){
    if (size <= 0) {
        return NULL;
    } else {
        int ch;
        int i = 0;
        int sizew = 0;
        char* word = NULL;
        lseek(fd,0,SEEK_SET);
        while (((ch = read(fd, pBuffer, 1)) != 0) && (size > (i + 1))) {
            if (ch == -1) {
                free(word);
                fprintf(stderr, "file error");
                return NULL; // fgets возвращает NULL при ошибке
            }
            if (sizew <= i){
                sizew = 2 * i + 1;
                word = realloc(word, sizew);
                if (word == NULL){
                    free(word);
                    fprintf(stderr, "memory error");
                    return NULL; // fgets возвращает NULL при ошибке
                }
            }
            word[i] = pBuffer[0];
            if (word[i] == '\n') {
                free(pBuffer);
                if (sizew <= i + 1){
                    sizew = 2 * i + 1;
                    word = realloc(word, sizew);
                    if (word == NULL){
                        free(word);
                        fprintf(stderr, "memory error");
                        return NULL; // fgets возвращает NULL при ошибке
                    }
                }
                word[i] = '\n';
                word[i+1] = '\0';
                pBuffer = strdup(word);
                free(word);
                return pBuffer;
            }
            i++;
        }

        if (sizew <= i + 1){
            word = realloc(word, i + 1);
            if (word == NULL){
                free(word);
                fprintf(stderr, "memory error");
                return NULL; // fgets возвращает NULL при ошибке
            }
        }
        free(pBuffer);
        word[i] = '\0';
        pBuffer = strdup(word);
        free(word);
    }
    return pBuffer;
}

int main() {

    int fd1 = open("test_file_for_myfgets.txt", O_RDONLY);
    char *str = (char *) malloc(sizeof(int));
    char *str2 = (char *) malloc(sizeof(int));
    FILE *f1 = fopen("test_file_for_myfgets.txt", "r");

    int num = 7;                    //размер для fgets и myfgets


    fgets(str, num, f1);
    printf("fgets prints: ");
    printf("%s\n", str);


    myfgets(fd1, str2, num);
    printf("myfgets prints: ");
    printf("%s\n", str2);

    fclose(f1);
    close(fd1);
    free(str);
    free(str2);
    return 0;
}
