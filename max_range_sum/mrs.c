/* mrs.c
 *
 * By William Ersing
 *
 * This is a programming challenge from CodeEval.com
 * It reads in a file where each line contains an
 * integer, N, representing a N-day period, a semicolon (;),
 * then a span of K separate days, D, representing daily
 * gains and losses over the given span (K).
 *
 * The format is shown below:
 *
 * 	N;D1 D2 D3 D4 ... DK	where:	0 < N < 10000
 * 									0 < K < 10000
 * 									-10000 < D < 10000
 *
 * 	example:
 * 	5;7 -3 -10 4 2 8 -2 4 -5 -2
 *
 * 	N = 5 day period
 * 	K = 10 day span
 *
 * The goal is to determine which N-day period in the
 * K-day span yields the highest total gain.
 *
 * From the example above, the highest 5-day gain:
 *
 * 	7 -3 -10 ((4 2 8 -2 4)) -5 -2
 *
 * 	Days 4-8 (4 2 8 -2 4) yield a total gain of 16.
 */

#include <stdlib.h>	// needed for malloc(), exit()
#include <stdio.h>	// needed for IO
#include <string.h>	// needed for strsep(), strtol()

#define ARRAY_SIZE_MAX 50000

int main(int argc, char *argv[]){

	// user entered correct number of args (i.e. 1)
	if(argc == 2){
		FILE *fptr = fopen(argv[1], "r");

		if(fptr == NULL){
			printf("File not found, Exiting...\n");
			exit(-1);
		}
		else{
			char *periodptr, *spanptr, *spantoken;
			long days[ARRAY_SIZE_MAX];	// gain/loss values for each day (D) in span (K)
			long period;	// number of days over which 'max gains' is sought (N)
			size_t buf_size = 10;
			char *buf = malloc(buf_size);

			while(getline(&buf, &buf_size, fptr) != -1){
				size_t span_length = 0;

				// temp buffer for strsep
				spanptr = buf;

				// grab up to ';' delim as string (input value for N)
				periodptr = strsep(&spanptr, ";");

				// convert period string to period long (N)
				period = strtol(periodptr, &periodptr, 10);

				// remove newline from span string (K)
				spanptr = strsep(&spanptr, "\n");

				// tokenize span (K) into days (D) - store in days[]
				while((spantoken = strsep(&spanptr, " ")) != NULL){
					days[span_length] = strtol(spantoken, &spantoken, 10);
					span_length++;
				}

				// find highest grossing period (N) in the given span (K)
				if(span_length >= period){
					size_t i = 0, j = 0;
					size_t k = period - 1;
					long period_tl = days[j];
					long period_hd = days[k];
					long tmp_gains = 0, max_gains = 0;

					for(i = 0; i < span_length; i++){
						if(i <= k){
							tmp_gains += days[i];
						}
						else{
							tmp_gains -= days[j];
							j++;
							k++;
							tmp_gains += days[k];
						}
						if(i == k && tmp_gains > max_gains){
							max_gains = tmp_gains;
						}
					}

					// print max gain for the given input
					printf("%ld\n", max_gains);
				}
				else{
					printf("Invalid Input:\tperiod (N) exceeds total days (K)\n");
				}

			}
			fflush(stdout);
			free(buf);
		}
		fclose(fptr);
	}

	// user entered too many args
	else if(argc > 2){
		printf("Too many arguments\n");
		printf("Correct Usage: %s <path to file>\n", argv[0]);
	}

	// user entered too few args
	else{
		printf("Too few arguments\n");
		printf("Correct Usage: %s <path to file>\n", argv[0]);
	}

	return 0;
}
