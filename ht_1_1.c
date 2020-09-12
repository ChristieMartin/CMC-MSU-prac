#include <stdio.h>

int main () {
    int n = 1024; // the quantity of numbers in sequence
    int j, i, t, a[n];

    printf("Enter the sequence of %d numbers.\n", n);

    for (i = 0; i < n; i++){
        scanf("%d", &a[i]);       // adding numbers of sequence into an array
    }
    printf("\n");
    
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++){
            if (a[i] < a[j]) {
                t = a[i];         // sorting the array (bubble sort)
                a[i] = a[j];
                a[j] = t;
            }
        }
    }

    for (i = 0; i < n; i++) {
        printf("%d ", a[i]);     // printing the array (sorted number of sequence)
    }

    printf("\n");
    return 0;
}