
#include "fix32.h"
#include "klee/klee.h"

fix32_t fix32_add(fix32_t inArg0, fix32_t inArg1) {
  return (inArg0 + inArg1);
}

fix32_t fix32_sub(fix32_t inArg0, fix32_t inArg1) {
  return (inArg0 - inArg1);
}

fix32_t fix32_mul(fix32_t inArg0, fix32_t inArg1) {
  // Each argument is divided to 32-bit parts.
  //					AB
  //			*	 CD
  // -----------
  //					BD	32 * 32 -> 64 bit products
  //				 CB
  //				 AD
  //				AC
  //			 |----| 64 bit product

  klee_open_merge();

  int64_t A = (inArg0 >> 32), C = (inArg1 >> 32);
  uint64_t B = (inArg0 & 0xFFFFFFFF), D = (inArg1 & 0xFFFFFFFF);

  int64_t AC = A*C;
  int64_t AD_CB = A*D + C*B;
  uint64_t BD = B*D;

  int64_t product_hi = AC + (AD_CB >> 32);

  // Handle carry from lower 32 bits to upper part of result.
  uint64_t ad_cb_temp = AD_CB << 32;
  uint64_t product_lo = BD + ad_cb_temp;
  if (product_lo < BD)
    product_hi++;

  // The upper 17 bits should all be the same (the sign).
//  if (product_hi >> 63 != product_hi >> 31)
//    klee_silent_exit(0);

  fix32_t result = (product_hi << 32) | (product_lo >> 32);

  klee_close_merge();
  return result;
}

fix32_t fix32_div(fix32_t a, fix32_t b) {

  fix32_t result;
  klee_make_symbolic(&result, sizeof(result), "fix32_div_result");
  klee_assume(fix32_mul(result, b) == a);
  return result;
}

fix32_t fix32_mod(fix32_t x, fix32_t y) {
  /* Note that in C90, the sign of result of the modulo operation is
   * undefined. in C99, it's the same as the dividend (aka numerator).
   */
  x %= y;
  return x;
}

int fix32_isnan(fix32_t a) {
  return 0;
}

int fix32_isinf(fix32_t a) {
  return 0;
}

fix32_t fix32_sqrt(fix32_t inValue) {

  fix32_t result;
  klee_make_symbolic(&result, sizeof(result), "fix32_sqrt_result");
  klee_assume(inValue >= 0);
  klee_assume(fix32_mul(result, result) == inValue);
  return result;
}

fix32_t fix32_rad_to_deg(fix32_t radians) {
  return fix32_mul(radians, fix32_rad_to_deg_mult);
}

fix32_t fix32_deg_to_rad(fix32_t degrees) {
  return fix32_mul(degrees, fix32_deg_to_rad_mult);
}

fix32_t fix32_sq(fix32_t x) {
  return fix32_mul(x, x);
}

fix32_t fix32_from_int(int a) {
  return a * fix32_one;
}

fix32_t fix32_abs(fix32_t x) {
  return (x < 0 ? -x : x);
}

fix32_t fix32_floor(fix32_t x) {
  return (x & 0xFFFFFFFF00000000ULL);
}

fix32_t fix32_ceil(fix32_t x) {
  return (x & 0xFFFFFFFF00000000ULL) + (x & 0x00000000FFFFFFFFULL ? fix32_one : 0);
}

fix32_t fix32_min(fix32_t x, fix32_t y) {
  return (x < y ? x : y);
}

fix32_t fix32_max(fix32_t x, fix32_t y) {
  return (x > y ? x : y);
}

fix32_t fix32_clamp(fix32_t x, fix32_t lo, fix32_t hi) {
  return fix32_min(fix32_max(x, lo), hi);
}

fix32_t fix32_sin(fix32_t inAngle) {
  return 0;
}
