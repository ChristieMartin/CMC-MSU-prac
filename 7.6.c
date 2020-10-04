#include<stdio.h>
#include<stdlib.h>
#include<string.h>

char* filecmp(FILE *, FILE *);

int main(int argc, char *argv[]) {
    FILE *file1;
    FILE *file2;
    
    if ((file1 = fopen(argv[argc - 2], "r")) == NULL) {   
        printf("Error: file can't be openned %s\n", *(argv - 2));
        return 1;
    } else if ((file2 = fopen(argv[argc - 1], "r")) == NULL) {
        printf("Error: file can't be openned %s\n", *(argv - 1));
        return 2;
    } else{
        printf("%s\n", filecmp(file1, file2));
        fclose(file1);
        fclose(file2);
    }
    return 0;

}

char* filecmp(FILE *f1, FILE *f2) {
    char *s1 = NULL;
    char *s2 = NULL;
    int ch1, ch2;
    int n = 0;
    int size = 0;
    while (( ch1 = fgetc(f1) ) != EOF && ( ch2 = fgetc(f2) ) != EOF) {
        if (size >= n) {
            size = size*2 + 1;
			s1 = realloc(s1, size);
            s2 = realloc(s2, size);
			if (s1 == NULL || s2 == NULL) {
				printf("Memory error");
				exit;
			}
        }
        if ((ch1 == '\n') || (ch2 == '\n')) {
            for (int i = 0; i < n; i++) {
                if (s1[i] != s2[i]) {
                    if (ch1 == '\n') {
                        return s1;
                    } else return s2;
                }
            }
            n = 0;
            free(s1);
            free(s2);
            s1 = s2 = NULL;
        } else {
            s1[n] = ch1;
            s2[n] = ch2;
            n++;
        }
    }
    if ((ch1 == EOF) && (ch2 != EOF)) {
        return s1; 
    }
    if ((ch1 != EOF) && (ch2 == EOF)){
        return s2;
    }
    return "Two files are equal";
}