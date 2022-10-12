#include "all.h"
/* arith.c S187c */
void checkarith(char operation, int32_t n, int32_t m, int precision) {
  int64_t nx = n;
  int64_t mx = m;
  int64_t result;
  switch (operation) {
    case '+': result = nx + mx; break;
    case '-': result = nx - mx; break;
    case '*': result = nx * mx; break;
    case '/': result = mx != 0 ? nx / mx : 0; break;
    default:  return;  /* other operations can't overflow */
  }

/* if [[result]] cannot be represented using [[precision]] signed bits, signal overflow S187d */
  assert(precision > 0 && precision < 64);  // shifts are defined
  if ((result << (64-precision)) >> precision != result) {
    runerror("Arithmetic overflow");
  }
}
