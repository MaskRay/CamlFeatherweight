#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "error.h"
#include "value.h"

void raise_with_string(u8 tag, const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  abort();
}

void not_implemented(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  abort();
}

void invalid_argument(const char *msg)
{
  raise_with_string(INVALID_EXN, msg);
}

void fatal_error(const char *msg)
{
  fatal_error_fmt("%s\n", msg);
}

void fatal_error_fmt(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(2);
}
