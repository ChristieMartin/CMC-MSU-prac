#include<stdio.h>
#include<stdlib.h>

char* s1 = NULL; 
char* s2 = NULL;

int main()
{
    int size, ch, beg, end, max, bmax, emax;
    size = end = beg = max = 0;

    while ((ch = getchar()) != EOF){
        if (end == size){
            size = 2 * size + 1;
            s1 = realloc(s1, size);
            s2 = realloc(s2, size);
        }
        if (ch == '\n' && end - beg > max){
            max = end - beg;
            bmax = beg;
            emax = end;
            beg = end;
        }
        s1[end] = ch;
        end++;
    }

    if (end - beg > max){
            max = end - beg;       //in case if string ends with EOF(without \n)
            bmax = beg;
            emax = end;
        }

    printf("\n");
    if (max != 0) {
        for (int i = 0; i < max; i++){
            s2[i] = s1[bmax];
            putchar(s2[i]); 
            bmax++;
        }
    }
    
    free(s1);
    free(s2);
    printf("\n");
    return 0;
}