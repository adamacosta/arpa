#ifndef NGRAM_H
#define NGRAM_H

#define CHECK(func, args) do {                    \
	int retval = func;                            \
	if (retval < 0) {                             \
		error(args);                              \
	}                                             \
} while (0)

void readarpa(char *filename, int *skip, int *lines, 
	          double *probs, char **ngrams);

void error(void *mmp, void *hp_ptr, int len, int fd, 
	       const char *restrict fmt, ...);

#endif