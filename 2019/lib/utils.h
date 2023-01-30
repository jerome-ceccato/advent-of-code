#ifndef UTILS_H
#define UTILS_H

#if !defined(__GNUC__)
#  define __attribute__(a)
#endif /* !__GNUC__ */

#ifndef min
#  define min(x, y) (((x) < (y)) ? (x) : (y))
#endif
#ifndef max
#  define max(x, y) (((x) > (y)) ? (x) : (y))
#endif

#endif
