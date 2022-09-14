
#include "fix32.h"
#include "klee/klee.h"

#define DIV_SOLVER  0
#define SQRT_SOLVER 0

int fix32_rounding_dir;

static const fix32_t fix32_maximum = 0x7FFFFFFFFFFFFFFE;     // the maximum value of fix32_t
static const fix32_t fix32_minimum = 0x8000000000000001;     // the maximum value of fix32_t
static const fix32_t fix32_inf     = 0x7FFFFFFFFFFFFFFE;     // value selected to represent inf (only positive at this time)
static const fix32_t fix32_nan     = 0x8000000000000000;     // value selected to represent nan

fix32_t fix32_add(fix32_t a, fix32_t b) {

  fix32_t result;
  klee_open_merge();

  if (a == fix32_nan | b == fix32_nan) {
    result = fix32_nan;
  } else {
    // Use unsigned integers because overflow with signed integers is
    // an undefined operation (http://www.airs.com/blog/archives/120).
    uint64_t _a = a, _b = b;
    uint64_t sum = _a + _b;

    // Overflow can only happen if sign of a == sign of b, and then
    // it causes sign of sum != sign of a.
    klee_assume(((_a ^ _b) & 0x8000000000000000) | !((_a ^ sum) & 0x8000000000000000));

    result = sum;
    klee_assume((result < fix32_maximum) & (result > fix32_minimum));
  }
  klee_close_merge();
  return result;
}

fix32_t fix32_sub(fix32_t a, fix32_t b) {

  fix32_t result;
  klee_open_merge();

  if (a == fix32_nan | b == fix32_nan) {
    result = fix32_nan;
  } else {
    uint64_t _a = a, _b = b;
    uint64_t diff = _a - _b;

    // Overflow can only happen if sign of a != sign of b, and then
    // it causes sign of diff != sign of a.
    klee_assume(!((_a ^ _b) & 0x8000000000000000) | !((_a ^ diff) & 0x8000000000000000));

    result = diff;
    klee_assume((result < fix32_maximum) & (result > fix32_minimum));
  }
  klee_close_merge();
  return result;
}

fix32_t fix32_mul(fix32_t a, fix32_t b) {

  fix32_t result;
  klee_open_merge();

  if (a == fix32_nan | b == fix32_nan) {
    result = fix32_nan;
  } else {

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

    // The upper bits should all be the same (the sign).
    klee_assume(product_hi >> 63 == product_hi >> 31);

    result = (product_hi << 32) | (product_lo >> 32);
    klee_assume((result < fix32_maximum) & (result > fix32_minimum));
  }
  klee_close_merge();
  return result;
}

#if DIV_SOLVER != 0

fix32_t fix32_div(fix32_t a, fix32_t b) {

  fix32_t result;
  klee_open_merge();
  if (a == fix32_nan | b == fix32_nan) {
    result = fix32_nan;
  } else {
    klee_make_symbolic(&result, sizeof(result), "fix32_div_result");
    klee_assume(fix32_mul(result, b) == a);
  }
  klee_close_merge();
  return result;
}

#else

uint8_t clz(uint64_t x) {
	uint8_t result = 0;
	if (x == 0) return 64;
	while (!(x & 0xF000000000000000)) { result += 4; x <<= 4; }
	while (!(x & 0x8000000000000000)) { result += 1; x <<= 1; }
	return result;
}

fix32_t fix32_div(fix32_t a, fix32_t b) {
	// This uses a hardware 64/64 bit division multiple times, until we have
	// computed all the bits in (a<<33)/b. Usually this takes 1-3 iterations.

  fix32_t result;
  klee_open_merge();
  if (a == fix32_nan | b == fix32_nan) {
    result = fix32_nan;
  } else if (b == 0) {
    result = fix32_inf;
  } else {
    uint64_t remainder = (a >= 0) ? a : (-a);
    uint64_t divider = (b >= 0) ? b : (-b);
    uint64_t quotient = 0;
    int bit_pos = 33;

    // Kick-start the division a bit.
    // This improves speed in the worst-case scenarios where N and D are large
    // It gets a lower estimate for the result by N/(D >> 33 + 1).
    if (divider & 0xFFF0000000000000) {
      uint64_t shifted_div = ((divider >> 33) + 1);
      quotient = remainder / shifted_div;
      remainder -= ((uint64_t)quotient * divider) >> 17;
    }

    // If the divider is divisible by 2^n, take advantage of it.
    while (!(divider & 0xF) && bit_pos >= 4) {
      divider >>= 4;
      bit_pos -= 4;
    }

    while (remainder && bit_pos >= 0) {
      // Shift remainder as much as we can without overflowing
      int shift = clz(remainder);
      if (shift > bit_pos) {
        shift = bit_pos;
      }
      remainder <<= shift;
      bit_pos -= shift;

      uint64_t div = remainder / divider;
      remainder = remainder % divider;
      quotient += div << bit_pos;

      klee_assume((div & ~(0xFFFFFFFFFFFFFFFF >> bit_pos)) == 0);
      remainder <<= 1;
      bit_pos--;
    }

    // Quotient is always positive so rounding is easy
    quotient++;
    result = quotient >> 1;

    // Figure out the sign of the result
    if ((a ^ b) & 0x8000000000000000) {
      result = -result;
    }
    klee_assume((result < fix32_maximum) & (result > fix32_minimum));
  }
  klee_close_merge();
	return result;
}

#endif

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
  return (a != fix32_nan) & (a != fix32_inf);
}

#if SQRT_SOLVER != 0

fix32_t fix32_sqrt_solver(fix32_t a) {

  fix32_t result;
  klee_open_merge();
  if (a == fix32_nan | a < 0) {
    result = fix32_nan;
  } else {
    klee_make_symbolic(&result, sizeof(result), "fix32_sqrt_result");
    klee_assume(fix32_mul(result, result) == a);
  }
  klee_close_merge();
  return result;
}

#else

fix32_t fix32_sqrt_direct(fix32_t a) {

  fix32_t result;
  klee_open_merge();
  if (a == fix32_nan | a < 0) {
    result = fix32_nan;
  } else {

    uint8_t  neg = (a < 0);
    uint64_t num = (neg ? -a : a);
    uint64_t uval = 0;
    uint64_t bit;
    uint8_t  n;

    bit = (uint64_t) 1 << 62;

    while (bit > num) {
      bit >>= 2;
    }

    // The main part is executed twice, in order to avoid
    // > 64 bit values in computations.
    for (n = 0; n < 2; n++) {
      // First we get the top 24 bits of the answer.
      while (bit) {
        if (num >= uval + bit) {
          num -= uval + bit;
          uval = (uval >> 1) + bit;
        } else {
          uval = (uval >> 1);
        }
        bit >>= 2;
      }

      if (n == 0) {
        // Then process it again to get the lowest bits.
        if (num > 4294967295) {
          // The remainder 'num' is too large to be shifted left
          // by 32, so we have to add 1 to result manually and
          // adjust 'num' accordingly.
          // num = a - (result + 0.5)^2
          //	 = num + result^2 - (result + 0.5)^2
          //	 = num - result - 0.5
          num -= uval;
          num = (num << 32) - 0x80000000;
          uval = (uval << 32) + 0x80000000;
        } else {
          num <<= 32;
          uval <<= 32;
        }
        bit = 1 << 30;
      }
    }
    // Finally, if next bit would have been 1, round the result upwards.
    if (num > uval) {
      uval++;
    }
    result = (neg ? -(fix32_t) uval : (fix32_t) uval);
  }
  klee_close_merge();
  return result;
}

#endif

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
