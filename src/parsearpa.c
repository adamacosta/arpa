#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int readchunk(char *start_ch, char *chunk);
/*
 * C implementation of the read.arpa function. As written, this will not
 * run on Windows, so someone will have to port it.
 */
int main()
{
	FILE *fd, *fp;
	int nlines;
	int i;

	/* Open and mmap file, checking to ensure everything works */
	fd = fopen("../inst/extdata/good.arpa", "r");
	if (!fd)
	{
		printf("Couldn't open file.\n");
		return 0;
	}
	/* Start read */
	nlines = 0;
	char word1[100] = "";
	char word2[100] = "";
	char word3[100] = "";
	char stop_string[100] = "\\1-grams:";
	int retval;
	int result;
	char c;
	float log;
	float crap;
	char *line = NULL;
	size_t len = 0;
	ssize_t read;

	fp = fd;

	while ((read = getline(&line, &len, fp)) != -1)
	{
		sscanf(line, "%s", word1);
		if (strcmp(word1, stop_string) == 0)
			break;

		//printf("%s\n", word1);
	}

	strcpy(stop_string, "\\2-grams:");
	while ((read = getline(&line, &len, fp)) != -1)
	{
		sscanf(line, "%s", word1);
		if (strcmp(word1, stop_string) == 0)
		{
			break;
		}

		sscanf(line, "%f\t%s", &log, word1);

		printf("%f\t%s\n", log, word1);
	}
	/**
	strcpy(stop_string, "\\3-grams:");
	while ((read = getline(&line, &len, fp)) != -1)
	{
		sscanf(line, "%s", word1);
		if (strcmp(word1, stop_string) == 0)
			break;

		sscanf(line, "%f\t%s%s", &log, word1, word2);

		printf("%f\t%s\t%s\n", log, word1, word2);
	}

	while ((read = getline(&line, &len, fp)) != -1)
	{
		sscanf(line, "%f\t%s%s%s", &log, word1, word2, word3);

		printf("%f\t%s\t%s\t%s\n", log, word1, word2, word3);
	}
	*/

	//munmap(fd, len);
	fclose(fp);
	return 0;
}

int readchunk(char *start_ch, char *chunk)
{
	char * ch;
	unsigned char c;
	int char_count;
	int i;

	ch = start_ch;
	i = 0;
	c = *ch;
	while (c != '\t' && c != ' ' && c != '\n' )
	{
		i++;
		ch++;
		c = *ch;
	}

	chunk = malloc(i + 1);
	ch = start_ch;
	i = 0;
	c = *ch;
	while (c != '\t' && c != ' ' && c != '\n')
	{
		chunk[i] = c;
		i++;
		ch++;
		c = *ch;
	}

	chunk[i + 1] = '\0';

	return i;
}
