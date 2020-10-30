#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MASK1 0x80  //1000 0000
#define MASK2 0xC0  //1100 0000
#define MASK3 0xE0 // 1110 0000


int bomcheck(char *file){
    unsigned char num8_1, num8_2, num8_3; 
    num8_1 = num8_2 = num8_3 = 0;
    
    FILE *f1 = fopen(file, "r");
    if(f1 == 0){
        fprintf(stderr, "error with file \n");
        exit(5);
    }
    fread(&num8_1, 1, 1, f1);
    fread(&num8_2, 1, 1, f1);
    fread(&num8_3, 1, 1, f1);
    fclose(f1);

    if((num8_1 == 0xEF) && (num8_2 == 0xBB) && (num8_3 == 0xBF)){
        return 1;
    } else return 0;
}

void endian(unsigned char num8_1, unsigned char num8_2, FILE* f2){
    unsigned char newnum8_1, newnum8_2, newnum8_3;
    newnum8_1 = newnum8_2 = newnum8_3 = 0;
    unsigned short num16 = (num8_2 << 8) | num8_1;
    if (num16 <= 0x7f){ //0-127
        fwrite(&num8_1, 1, 1, f2);
    } else if (num16 <= 0x7ff){ //128-2047
        newnum8_2 = ((num8_2 & 0x7) << 2) | (num8_1 >> 6) | MASK2;
        newnum8_1 = (num8_1 & 0x3F) | MASK1; 

        fwrite(&newnum8_2, 1, 1, f2);
        fwrite(&newnum8_1, 1, 1, f2);
    } else if (num16 <= 0xffff) {//2048-65535
        newnum8_1 = (num8_1 & 0x3F) | MASK1;
        newnum8_2 = (((num8_2 & 0xF) << 2) | (num8_1 >> 6)) | MASK1;
        newnum8_3 = (num8_2 >> 4) | MASK3;

        fwrite(&newnum8_3, 1, 1, f2);
        fwrite(&newnum8_2, 1, 1, f2);
        fwrite(&newnum8_1, 1, 1, f2);
    } else fprintf(stderr, "not in range %x\n", num16);
}

int main(int argc, char *argv[]){

    int bom8;
    FILE* f1 = stdin;
    FILE* f2 = stdout;
    if (argc > 1){
        f1 = fopen(argv[1], "r");
        if (f1 == 0) {
            fprintf(stderr, "error with file \n");
            return 1;
        }
    }

    if (argc > 2){
        bom8 = bomcheck(argv[2]);
        
        f2 = fopen(argv[2], "w");
        if (bom8 == 1) {
            unsigned short num8 = 0xef;
            fwrite(&num8, 1, 1, f2);
            num8 = 0xbb;
            fwrite(&num8, 1, 1, f2);
            num8 = 0xbf;
            fwrite(&num8, 1, 1, f2);
        }
        if (f2 == 0) {
            fprintf(stderr, "error with file \n");
            return 1;
        }
    }
    
    unsigned char num8_1, num8_2;
    unsigned short num16;
    int fl = 0;
    fread(&num16, 2, 1, f1);
    if (num16 == 0xfffe){
        fl = 1; //big-endian
    } else if (num16 == 0xfeff){
        fl = 0; //littleendian
    } else {
        num8_1 = num16 >> 8;
        num8_2 = num16 ^ 0xff00;
        endian(num8_1, num8_2, f2);
        fprintf(stderr, "no bom \n");
        fl = 0;
    }
    int sum;
    while (feof(f1) == 0){
        sum = fread(&num8_1, 1, 1, f1) + fread(&num8_2, 1, 1, f1);
        if (sum != 2) {
            if (sum == 1){
                fprintf(stderr, "something's odd %x %x\n", num8_1, num8_2);
            }
            continue;
        }
        if (fl == 1) {
            endian(num8_2, num8_1, f2);
        } else endian(num8_1, num8_2, f2);
    }


    fclose(f1);
    return 0;
}
