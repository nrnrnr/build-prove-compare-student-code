#include "all.h"
/*
 * \qbreak Function [[validate]] is used freely in the
 * interpreter to make sure all values are good. Calling
 * [[validate(v)]] returns [[v]], unless [[v]] is
 * invalid, in which case it causes an assertion
 * failure.
 * <validate.c>=
 */
Value validate(Value v) {
    assert(v.alt != INVALID);
    return v;
}
