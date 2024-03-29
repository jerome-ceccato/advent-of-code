#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
#include <inttypes.h>
typedef intmax_t bigint;
#define BIGINT_FMT "%" PRIdMAX

#if !defined(__GNUC__)
#  define __attribute__(a)
#endif /* !__GNUC__ */

#ifndef min
#  define min(a, b)     \
    ({                  \
__typeof__(a) _a = (a); \
__typeof__(b) _b = (b); \
_a < _b ? _a : _b;      \
    })
#endif
#ifndef max
#  define max(a, b)     \
    ({                  \
__typeof__(a) _a = (a); \
__typeof__(b) _b = (b); \
_a > _b ? _a : _b;      \
    })
#endif

#endif
