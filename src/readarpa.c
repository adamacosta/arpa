#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
//#include "ngram.h"

int readarpa(char *input, int verbosearg, int uskiparg, int umaxarg,
	int bskiparg, int bmaxarg, int tskiparg, int tmaxarg);

int main()
{
	char * filename = "../arpa/extdata/sample.arpa";
	readarpa(filename, 0, 7, 15, 17, 29, 31, 49);
}

/*
 * C implementation of the read.arpa function. As written, this will not
 * run on Windows, so someone will have to port it.
 */
int readarpa(char *input, int verbosearg, int uskiparg, int umaxarg, 
              int bskiparg, int bmaxarg, int tskiparg, int tmaxarg) 
{
	
	int verbose;
	int uskip;
	int umax;
	int bskip;
	int bmax;
	int tskip;
	int tmax;

	FILE * fd;
	size_t len;
	size_t linesize = 0;
	void *pa;
	void *eof;
	struct stat sbuf;
	const char *cur_ch;
	const char *lst_ch;
	int nlines;
	int lst_idx;
	int cur_idx;
	int i;

	/* Double-check input 
	if (!isLogical(verbosearg)) error("verbose must be Boolean type");
	if (!isInteger(uskiparg)) error("uskip must be Integer type");
	if (!isInteger(umaxarg)) error("umax must be Integer type");
	if (!isInteger(bskiparg)) error("bskip must be Integer type");
	if (!isInteger(bmaxarg)) error("bmax must be Integer type");
	if (!isInteger(tskiparg)) error("tskip must be Integer type");
	if (!isInteger(tmaxarg)) error("tmax must be Integer type");

	verbose = LOGICAL(verbosearg)[0];
	uskip = INTEGER(uskiparg)[0];
	umax = INTEGER(umaxarg)[0];
	bskip = INTEGER(bskiparg)[0];
	bmax = INTEGER(bmaxarg)[0];
	tskip = INTEGER(tskiparg)[0];
	tmax = INTEGER(tmaxarg)[0];
	*/

	verbose = verbosearg;
	uskip = uskiparg;
	umax = umaxarg;
	bskip = bskiparg;
	bmax = bmaxarg;
	tskip = tskiparg;
	tmax = tmaxarg;
	
	/* Open file */
	const char *filename = input;
	fd = fopen(filename, "r");

	int startline = uskip;
	int stopline = umax;
	const char NEWLINE = '\n';
	const char TAB = '\t';
	int j;
	char word1[100];
	char word2[100];
	char word3[100];
	char *line;
	float log = 0.0;

	//printf("%d %d %d %d %d %d\n", uskip, umax, bskip, bmax, tskip, tmax);

	nlines = 0;
	for (j = 1; j <= 3; j++)
	{
		if (j == 1)
		{
			startline = uskip;
			stopline = umax;
		}
		else if (j == 2)
		{
			startline = bskip;
			stopline = bmax;
		}
		else if (j == 3)
		{
			startline = tskip;
			stopline = tmax;
		}
		
		//printf("j = %d, nlines = %d, startline = %d, stopline = %d\n", j, nlines, startline, stopline);

		/* First, advance past skip */
		while (nlines < startline)
		{
			nlines++;
			getline(&line, &linesize, fd);
			printf("skipping line %d\n", nlines);
			
		}

		/* Now parse the file */
		while (nlines < stopline)
		{
			nlines++;
			getline(&line, &linesize, fd);
			//printf("%d\t%d\n", j, nlines);
			//printf("%s\n", line);

			if (j == 1)
			{	
				sscanf(line, "%f\t%s", &log, word1);
				printf("%f\t%s\n", log, word1);
			}
			else if (j == 2)
			{
				sscanf(line, "%f %s %s", &log, word1, word2);
				printf( "%f\t%s\t%s\n", log, word1, word2);
			}
			else if (j == 3)
			{
				sscanf(line, "%f %s %s %s", &log, word1, word2, word3);
				printf("%f\t%s\t%s\t%s\n", log, word1, word2, word3);
			}
			else
			{
				printf("What the fuckk\n");
			}
		}
		printf("Exiting while loop\n");
	}

	fclose(fd);

	return(0);
}