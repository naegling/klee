
#include "fix32.h"
#include "klee/klee.h"

int fix32_rounding_dir;

static const fix32_t fix32_maximum = 0x7FFFFFFFFFFFFFFE;     // the maximum value of fix32_t
static const fix32_t fix32_mimimum = 0x8000000000000001;     // the maximum value of fix32_t
static const fix32_t fix32_inf     = 0x7FFFFFFFFFFFFFFE;     // value selected to represent inf (only positive at this time)
static const fix32_t fix32_nan     = 0x8000000000000000;     // value selected to represent nan

fix32_t fix32_add(fix32_t a, fix32_t b) {

  if (a == fix32_nan | b == fix32_nan) return fix32_nan;

  // Use unsigned integers because overflow with signed integers is
  // an undefined operation (http://www.airs.com/blog/archives/120).
  uint64_t _a = a, _b = b;
  uint64_t sum = _a + _b;

  // Overflow can only happen if sign of a == sign of b, and then
  // it causes sign of sum != sign of a.
  klee_assume(!((!((_a ^ _b) & 0x8000000000000000) & ((_a ^ sum) & 0x8000000000000000))));
  fix32_t result = sum;
  klee_assume((result < fix32_maximum) & (result > fix32_mimimum));
  return result;
}

fix32_t fix32_sub(fix32_t a, fix32_t b) {

  if (a == fix32_nan | b == fix32_nan) return fix32_nan;

  uint64_t _a = a, _b = b;
  uint64_t diff = _a - _b;

  // Overflow can only happen if sign of a != sign of b, and then
  // it causes sign of diff != sign of a.
  klee_assume(!(((_a ^ _b) & 0x8000000000000000) & ((_a ^ diff) & 0x8000000000000000)));

  fix32_t result = diff;
  klee_assume((result < fix32_maximum) & (result > fix32_mimimum));
  return result;
}

fix32_t fix32_mul(fix32_t a, fix32_t b) {

  if (a == fix32_nan | b == fix32_nan) return fix32_nan;

  int64_t A = (a >> 32), C = (b >> 32);
  uint64_t B = (a & 0xFFFFFFFF), D = (b & 0xFFFFFFFF);

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
  klee_assume(product_hi >> 63 != product_hi >> 31);

  fix32_t result = (product_hi << 32) | (product_lo >> 32);
  klee_assume((result < fix32_maximum) & (result > fix32_mimimum));
  return result;
}

fix32_t fix32_div(fix32_t a, fix32_t b) {

  if (a == fix32_nan | b == fix32_nan) return fix32_nan;

  fix32_t result;
  klee_make_symbolic(&result, sizeof(result), "fix32_div_result");
  klee_assume(fix32_mul(result, b) == a);
  return result;
}

fix32_t fix32_mod(fix32_t a, fix32_t b) {

  if (a == fix32_nan | b == fix32_nan) return fix32_nan;

  /* Note that in C90, the sign of result of the modulo operation is
   * undefined. in C99, it's the same as the dividend (aka numerator).
   */
  a %= b;
  return a;
}

int fix32_isnan(fix32_t a) {
  return (a == fix32_nan);
}

int fix32_isinf(fix32_t a) {
  return (a == fix32_inf);
}

int fix32_isfinite(fix32_t a) {
  return !(fix32_isnan(a) | fix32_isinf(a));
}

fix32_t fix32_sqrt(fix32_t a) {

  if (a < 0) return fix32_nan;

  fix32_t result;
  klee_make_symbolic(&result, sizeof(result), "fix32_sqrt_result");
  klee_assume(fix32_mul(result, result) == a);
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

int fix32_fegetround() {
  return fix32_rounding_dir;
}

int fix32_fesetround(int rdir) {
  fix32_rounding_dir = rdir;
  return 0;
}
