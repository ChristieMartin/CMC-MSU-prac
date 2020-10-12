#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

struct Tree{
    char* word;
    int cnt;
    struct Tree *left;
    struct Tree *right;
    };

struct Tree *talloc(void)
{
 return (struct Tree *) malloc(sizeof(struct Tree));
}

struct Tree* treeadd(struct Tree* t, char *w) {
    int comp;
    if (t == NULL) {
        t = talloc();
        if (t == NULL) {
            printf("memory error");
            exit(2);
        }
        t -> word = strdup(w);
        t -> cnt = 1;
        t -> right = t -> left = NULL;
    } else if ((comp = strcmp(w, t -> word)) == 0) {
        t -> cnt++;
    } else if (comp > 0) {
        t -> right = treeadd(t -> right, w);
    } else 
        t -> left = treeadd(t -> left, w);

    return t;
}


int treecount(struct Tree* t) {
    if (t != NULL) {
        return t -> cnt + treecount(t->right) + treecount(t->left);
    } else return 0;
}

int treemax(struct Tree* t){
    int max = 0;
    if (t != NULL) {
        if (max < treemax(t -> right)) max = treemax(t -> right);
        if (max < treemax(t -> left))  max = treemax(t -> left);
        if (max < t -> cnt)            max = t -> cnt;
    }
    return max;
}

void printleaf(struct Tree* t, int num, int count, FILE* f){
    if (t != NULL){
        if (t -> cnt == num) {
            fprintf(f, "%s %4d     %f\n", t -> word, t -> cnt, (double)t -> cnt / count);
        }
        printleaf(t -> right, num, count, f);
        printleaf(t -> left, num, count, f);
    }
}

void treeprint(struct Tree* t, FILE* f) {
    int max = treemax(t);
    int count = treecount(t);
    for(int i = max; i >= 1; i--) {
        printleaf(t, i, count, f);
    }
}

void treedel(struct Tree* t) {
    if (t != NULL){
        treedel(t -> right);
        treedel(t -> left);
        free(t -> word);
        free(t);
    }
}

void readfile(FILE* f){

}

char* w = NULL;

int main(int argc, char *argv[]) {
    struct Tree *t = NULL;
    FILE *f1;
    FILE *f2;
    int numnum = 3;
	
    if (argc > 1) {
        if (strcmp(argv[1], "-i") == 0) {
            f1 = fopen(argv[2], "r");
            if (f1 == NULL) {
                printf("error with file %s", argv[2]);
            }
        } else{ 
            f1 = stdin;
            numnum -= 2;
        }
    } else f1 = stdin;
    if (strcmp(argv[numnum], "-o") == 0 && argc > 3) {
        f2 = fopen(argv[numnum + 1], "w");
        if (f2 == NULL) {
            printf("error with file %s", argv[numnum + 1]);
        }
    } else f2 = stdout;
	
    int ch;
    int size = 0, n = 0;

    while ((ch = fgetc(f1)) != EOF) {
        if (size >= n) {
            size = 2 * size + 1;
            w = realloc(w, size);
            if (w == NULL) {
				fprintf(stderr,"memory error");
				return 3;
			}
        }
        if (ispunct(ch) || isspace(ch)) {
            if (n > 0) {
                if (size < n + 1) {
                    size = n + 1;
                    w = realloc(w, n + 1);
                if (w == NULL) {
                    fprintf(stderr,"memory error");
                    return 3;
			    }
                }
                w[n] = '\0'; 
                t = treeadd(t, w);
                size = n = 0;
                free(w);     
                w = malloc(2);
            }
            if (ispunct(ch)){
                free(w);
                w = (char *)malloc(2);
                if (w == NULL) {
                    fprintf(stderr,"memory error");
                    return 3;
                }
                w[0] = ch;
                w[1] = '\0';
                t = treeadd(t, w);
                free(w);     
                w = (char *)malloc(2);
                size = n = 0;
            }
        } else {
            w[n] = ch;
            n++;
        }
    }
    if (n > 0) {
        if (size < n+1){
            size = n + 1;
            w = realloc(w, n + 1);
            if (w == NULL) {
				fprintf(stderr,"memory error");
				return 3;
			}
        }
        w[n] = '\0';
        t = treeadd(t, w);
        free(w);     
        w = (char *)malloc(2);
    }

    if (w != NULL) free(w);
	
    treeprint(t,f2);
    treedel(t);
    fclose(f1);
    fclose(f2);
    return 0;
}
