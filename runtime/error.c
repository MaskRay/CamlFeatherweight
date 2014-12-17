#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "value.h"

void raise_with_string(u8 tag, const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  abort();
}

void invalid_argument(const char *msg)
{
  raise_with_string(INVALID_EXN, msg);
}
