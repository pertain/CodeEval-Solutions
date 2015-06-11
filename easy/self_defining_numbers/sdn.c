/* sdn.c
 *
 * By William Ersing
 *
 * This is a programming challenge from codeeval.com.
 *
 * (EASY) Self Describing Numbers
 * https://www.codeeval.com/open_challenges/40/
 *
 * ===================================================
 *
 * The purpose is to read in a file containing one
 * "self describing number," (sdn) on each line and
 * determine if the number on each line of the file is
 * or is not a sdn. A sdn is a number (assuming digit
 * positions are labeled 0 to N-1) where the digit in
 * each position is equal to the number of times that
 * digit appears in the number.
 *
 * Example:
 *
 *      2020 is a Self Defining Number (sdn)
 *
 *          ->  0 appears 2 times
 *          ->  1 appears 0 times
 *          ->  2 appears 2 times
 *          ->  3 appears 0 times
 *
 * ===================================================
 */


#include <stdlib.h>     // needed for calloc(), free(), exit()
#include <stdio.h>      // needed for IO
#include <string.h>     // needed for string manipulation
#include <stdbool.h>    // needed for booleans

#define BUFSIZE 30

int main(int argc, char *argv[]){
    FILE *fptr;
    char buf[BUFSIZE] = {0};
    char digit_str[BUFSIZE] = {0};
    int *digit_count;

    // user entered (correctly) one argument
    if(argc == 2){
        fptr = fopen(argv[1], "r+");
        if(fptr == NULL){
            printf("File does not exist. Exiting...\n");
            exit(-1);
        }
        else{
            while(fgets(buf, BUFSIZE, fptr) != NULL){
                int digits = strlen(buf) - 1;
                int is_sdn = false;
                int i;
                int max_digit = 0;
                
                // determine max digit
                for(i = 0; i < digits; i++){
                    if((buf[i] - '0') > max_digit){
                        max_digit = buf[i] - '0';
                    }
                }

                if(digits >= max_digit){
                    digit_count = calloc(digits, sizeof(int));
                    for(i = 0; i < digits; i++){
                        int digit_val = buf[i] - '0';
                        digit_count[digit_val]++;
                    }

                    // convert int[] to char[]
                    memset(digit_str, 0, sizeof(digit_str));
                    for(i = 0; i < digits; i++){
                         digit_str[i] = digit_count[i] + '0';
                    }
                    digit_str[digits] = '\n';

                    // determine if digit count matches input number
                    if((strcmp(buf, digit_str)) == 0){
                        is_sdn = true;
                    }

                    free(digit_count);
                }

                // output answers to stdout
                printf("%d\n", is_sdn);
            }
        }
        fclose(fptr);
    }

    // Too many arguments provided by user
    else if(argc > 2){
        printf("Too many arguments\n");
        printf("usage: %s <filename>\n", argv[0]);
    }

    // Too few arguments provided by user
    else{
        printf("Too few arguments\n");
        printf("usage: %s <filename>\n", argv[0]);
    }

    return 0;
}
