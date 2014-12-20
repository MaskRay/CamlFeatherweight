#include "value.h"

u32 string_length(value s);
int string_compare(value s1, value s2);
u8 string_getitem(value s, u32 i);
void string_setitem(value s, u32 i, u8 c);
