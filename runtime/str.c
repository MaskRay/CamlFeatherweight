#include "str.h"

u32 string_length(value s)
{
  u32 size = String_wosize_val(s);
  return size*4 - ((uvalue)Field(s, size-1) >> sizeof(value)*8-8);
}

int string_compare(value s1, value s2)
{
  u32 len1 = string_length(s1),
      len2 = string_length(s2);
  u8 *x = (u8*)((value*)s1+2),
     *y = (u8*)((value*)s2+2);
  for (u32 len = len1 < len2 ? len1 : len2; len; len--) {
    if (*x != *y)
      return *x < *y ? -1 : 1;
    x++;
    y++;
  }
  if (len1 < len2) return -1;
  if (len1 > len2) return 1;
  return 0;
}

u8 string_getitem(value s, u32 i)
{
  return (uvalue)Field(s, i/sizeof(value)) >> i%sizeof(value)*8 & 0xFF;
}

void string_setitem(value s, u32 i, u8 c)
{
  uvalue x = (uvalue)Field(s, i/sizeof(value));
  u32 y = i%sizeof(value)*8;
  Field(s, i/sizeof(value)) = x - ((x >> y & 0xFF) << y) + (c << y);
}
