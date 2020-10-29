#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MASK1 0x80  //1000 0000
#define MASK2 0xC0  //1100 0000
#define MASK3 0xE0 // 1110 0000
#define MASK4 0xF0 // 1111 0000

#define MASK5 0xFC // 1111 1100

int checkbom(char* filename){
    FILE* file = fopen(filename, "r");
    int i = 0;
    unsigned short num16;
    if (file == 0) {
        perror("file error");
        exit(1);
    }
    fread(&num16, 2, 1, file);
    fclose(file);
    if (num16 == 0xfffe){
        i = 0;
    } else if (num16 == 0xfeff){
        i = 1;
    }
    return i;
}

int main(int argc, char *argv[])
{
    unsigned char num8, num8_1, num8_2, num8_3;
    unsigned char num16_1, num16_2;
    int bom = 1;
    int bom16 = 0;
    num8_1 = num8_2 = num8_3 = 0;
    FILE* fin = stdin;
    FILE* fout = stdout;
    if (argc > 1){
        fin = fopen(argv[1], "r");
        if (fin == 0) {
            perror("error with file");
            return 1;
        }
    }

    if (argc > 2){
        bom16 = checkbom(argv[2]);
        num16_1 = 0xfe;
        num16_2 = 0xff;
        fout = fopen(argv[2], "w");
        if (fout == 0) {
            perror("error with file");
            return 1;
        }
        if (bom16 == 0){
            fwrite(&num16_2, 1, sizeof(num16_1), fout);
            fwrite(&num16_1, 1, sizeof(num16_2), fout);
        } else {
            fwrite(&num16_1, 1, sizeof(num16_1), fout);
            fwrite(&num16_2, 1, sizeof(num16_2), fout);
        }
    }
    num16_1 = 0;
    num16_2 = 0;
    while (fread(&num8, sizeof(num8), 1, fin) != 0){
        if ((num8 & MASK1) == 0){
            if (num8_2 != 0 || num8_3 != 0) {
                fprintf(stderr, "error with num8_2 != 0 || num8_3 != 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
            if (num8_1 == 0){
                if (bom16 == 1) {
                fwrite(&num8_1, 1, sizeof(num8), fout);
                fwrite(&num8, 1, sizeof(num8), fout);
                } else {
                    fwrite(&num8, 1, sizeof(num8), fout);
                    fwrite(&num8_1, 1, sizeof(num8), fout);
                }
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            } else {
                fprintf(stderr, "error with else num8_1 == 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
        } else if ((num8 & MASK4) == MASK3){
            if (num8_1 !=0 || num8_2 != 0) {
                fprintf(stderr, "error with num8_1 !=0 || num8_2 != 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
            if (num8_3 == 0){
                num8_3 = num8;
            } else {
                fprintf(stderr, "error with else num8_3 == 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
        } else if ((num8 & MASK3) == MASK2) {
            if (num8_1 != 0) {
                fprintf(stderr, "error with num8_1 != 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
            if (num8_2 == 0) {
                num8_2 = num8;
            } else {
                fprintf(stderr, "error with else num8_2 == 0, %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
        } else if ((num8 & MASK2) == MASK1){
            if (num8_1 == 0){
                num8_1 = num8;
            } else if (num8_2 == 0) {
                if (num8_3 == 0) {
                    fprintf(stderr, "error with num8_3 == 0, %x\n", num8);
                    num8_1 = num8_2 = num8_3 = 0;
                    continue;
                }
                num8_2 = num8;
            } else {
                fprintf(stderr, "error with else num8_1 == 0 && num8_2 == 0 %x\n", num8);
                num8_1 = num8_2 = num8_3 = 0;
                continue;
            }
        }
        if (num8_3 == 0 && num8_2 != 0 && num8_1 != 0){
            num16_1 = (num8_1 ^ MASK1) | ((num8_2 ^ MASK5) << 6);
            num16_2 = (num8_2 ^ MASK2) >> 2;
            if (bom16 == 1){
                num8_1 = num16_1;
                num16_1 = num16_2;
                num16_2 = num8_1;
            }
            fwrite(&num16_1, 1, sizeof(num16_1), fout);
            fwrite(&num16_2, 1, sizeof(num16_2), fout);
            num8_1 = num8_2 = num8_3 = 0;
        } else if (num8_3 != 0 && num8_2 != 0 && num8_1 != 0){
            if (bom) {
                if (num8_3 == 0xef && num8_1 == 0xbb && num8_2 == 0xbf){
                    num8_1 = num8_2 = num8_3 = 0;
                    bom = 0;
                    continue;
                }
                bom = 0;
            }
            num16_1 = (num8_2 ^ MASK1) | ((num8_1 ^ MASK5) << 6);
            num16_2 = ((num8_1 ^ MASK1) >> 2) | ((num8_3 ^ MASK3) << 4);
            if (bom16 == 1){
                num8_1 = num16_1;
                num16_1 = num16_2;
                num16_2 = num8_1;
            }
            fwrite(&num16_1, 1, sizeof(num16_1), fout);
            fwrite(&num16_2, 1, sizeof(num16_2), fout);
            num8_1 = num8_2 = num8_3 = 0;
        } 
    }
    return 0;
}
