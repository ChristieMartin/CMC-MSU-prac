#include<stdio.h>
#include<stdlib.h>

char* s1 = NULL; 

int main()
{
    int size, ch, beg, end, max, bmax, emax;
    size = end = beg = max = 0;

    while ((ch = getchar()) != EOF){
        if (end == size){
            size = 2 * size + 1;
            s1 = realloc(s1, size);
        }
        if (ch == '\n'){
            if (end - beg > max){
                max = end - beg;
                bmax = beg;
                emax = end;
            }
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
        for (int i = bmax; i < emax; i++){
            putchar(s1[i]); 
        }
    }
    
    free(s1);
    printf("\n");
    return 0;
}
