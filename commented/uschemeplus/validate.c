#include "all.h"
/*
 * Interpreters defined in \crefgc.chap check value
 * pointers for validity. But in \crefschemes.chap, no
 * validation is needed.
 * <validate.c>=
 */
Value validate(Value v) {
    return v;
}
