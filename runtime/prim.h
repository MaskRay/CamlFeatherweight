#include "value.h"

int string_compare(value s1, value s2);

typedef value (*c_primitive)();
extern c_primitive cprims[];
extern const char *name_of_cprims[];
