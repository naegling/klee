#ifndef __libfixmath_fix32_h__
#define __libfixmath_fix32_h__

#include <stdint.h>

typedef int32_t fix16_t;
typedef int64_t fix32_t;


static const fix32_t FOUR_DIV_PI = 0x145F306DD;               /*!< Fix32 value of 4/PI */
static const fix32_t _FOUR_DIV_PI2 = 0xFFFFFFFF983f4277;       /*!< Fix32 value of -4/PI² */
static const fix32_t X4_CORRECTION_COMPONENT = 0x3999999A;    /*!< Fix32 value of 0.225 */
static const fix32_t PI_DIV_4 = 0xC90FDAA2;                   /*!< Fix32 value of PI/4 */
static const fix32_t THREE_PI_DIV_4 = 0x25B2F8FE6;            /*!< Fix32 value of 3PI/4 */

static const fix32_t fix32_pi = 13493037704;                  /*!< fix32_t value of pi */
static const fix32_t fix32_e  = 11674931555;                  /*!< fix32_t value of e */
static const fix32_t fix32_one = 0x0000000100000000;          /*!< fix32_t value of 1 */
static const fix32_t fix32_deg_to_rad_mult = 74947179;
static const fix32_t fix32_rad_to_deg_mult = 246101626061;

/* Macro for defining fix32_t constant values.
         The functions above can't be used from e.g. global variable initializers,
         and their names are quite long also. This macro is useful for constants
         springled alongside code, e.g. F32(1.234).

         Note that the argument is evaluated multiple times, and also otherwise
         you should only use this for constant values. For runtime-conversions,
         use the functions above.
*/
#define F32(x) ((fix32_t)(((x) >= 0) ? ((x) * 4294967296.0 + 0.5) : ((x) * 4294967296.0 - 0.5)))


#ifdef NEVER
/* Conversion functions between fix32_t and float/integer.
 * These are inlined to allow compiler to optimize away constant numbers
 */
static inline float   fix32_to_float(fix32_t a) { return (float)a / fix32_one; }
static inline double  fix32_to_dbl(fix32_t a)   { return (double)a / fix32_one; }
#endif

fix32_t fix32_from_int(int a);

int fix32_isnan(fix32_t a);
int fix32_isinf(fix32_t a);
fix32_t fix32_add(fix32_t inArg0, fix32_t inArg1);
fix32_t fix32_sub(fix32_t inArg0, fix32_t inArg1);
fix32_t fix32_mul(fix32_t inArg0, fix32_t inArg1);
fix32_t fix32_div(fix32_t inArg0, fix32_t inArg1);
fix32_t fix32_mod(fix32_t x, fix32_t y);

fix32_t fix32_abs(fix32_t x);
fix32_t fix32_floor(fix32_t x);
fix32_t fix32_ceil(fix32_t x);
fix32_t fix32_min(fix32_t x, fix32_t y);
fix32_t fix32_max(fix32_t x, fix32_t y);
fix32_t fix32_clamp(fix32_t x, fix32_t lo, fix32_t hi);

fix32_t fix32_sin_parabola(fix32_t inAngle);
fix32_t fix32_sin(fix32_t inAngle);
fix32_t fix32_cos(fix32_t inAngle);
fix32_t fix32_tan(fix32_t inAngle);
fix32_t fix32_asin(fix32_t inValue);
fix32_t fix32_acos(fix32_t inValue);
fix32_t fix32_atan(fix32_t inValue);
fix32_t fix32_atan2(fix32_t inY, fix32_t inX);

fix32_t fix32_rad_to_deg(fix32_t radians);
fix32_t fix32_deg_to_rad(fix32_t degrees);
fix32_t fix32_sqrt(fix32_t inValue);
fix32_t fix32_sq(fix32_t x);
fix32_t fix32_exp(fix32_t inValue);
fix32_t fix32_log(fix32_t inValue);
fix32_t fix32_log2(fix32_t x);
fix32_t fix32_slog2(fix32_t x);

void fix32_to_str(fix32_t value, char *buf, int decimals);
fix32_t fix32_from_str(const char *buf);

#endif
