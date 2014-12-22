#pragma once

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define WORD_SIZE __WORDSIZE

#if WORD_SIZE == 32
# define MAGIC "ml32"
#else
# define MAGIC "ml64"
#endif
