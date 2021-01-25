#include <stdio.h>
#include <stdlib.h>
#include <string.h>


unsigned short Crc16Ccitt(char *bytes) {

    unsigned short i, j, k;
    unsigned short crc, poly;
    unsigned short table[256];

    poly = 0x1021;
    for (i = 0; i < 256; i++) {
        k = (i << 8);
        for (j = 0; j < 8; j++) {
            k = (k << 1) ^ (k >> 15 ? poly : 0);
        }
        table[i] = k;
    }

    crc = 0x0000;
    for (i = 0; i < strlen(bytes); ++i) {
        j = (crc >> 8) ^ bytes[i];
        crc = (crc << 8) ^ table[j];
    } 

    for (i = 0; i < 32; i++) {
        for (j = 0; j < 8; j++) {
            printf("0%02xh, ", table[j + i * 8] & 0xff);
        }
        printf("\n");
    }

    printf("\n");

    for (i = 0; i < 32; i++) {
        for (j = 0; j < 8; j++) {
            printf("0%02xh, ", table[j + i * 8] >> 8);
        }
        printf("\n");
    }

    return crc;
}

int main(int argc, char **argv) {

    char * data = "CRC16TESTDATA";
    unsigned short result = Crc16Ccitt(data);

    printf("result = %04x, should be 0x7f98\n", result);

}

