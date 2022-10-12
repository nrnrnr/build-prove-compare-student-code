#include "all.h"
/*
 * Only addition, subtraction, multiplication, and
 * division can cause overflow.
 * <arith.c>=
 */
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
  /*
   * A 64-bit [[result]] fits in k bits if it is unchanged
   * by sign-extending the least significant k bits.
   * Sign extension is achieved by two shifts. According
   * to the C standard, shifts on [[int64_t]] are defined
   * up to 63 bits.
   * <if [[result]] cannot be represented using [[precision]] signed bits,
                                                               signal overflow>=
   */
  assert(precision > 0 && precision < 64);  // shifts are defined
  if ((result << (64-precision)) >> precision != result) {
    runerror("Arithmetic overflow");
  }
}
