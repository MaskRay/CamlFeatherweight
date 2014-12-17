#include "str.h"

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
