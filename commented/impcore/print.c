#include "all.h"
/*
 * Function [[=alist?]] makes it possible to implement
 * sets of association lists, but it doesn't dictate how
 * . The wrong way to do it is to write a new version of
 * [[member?]] which uses [[=alist?]] instead of
 * [[equal?]]; it could be called [[al-member?]]. Then
 * because [[add-element]] calls [[member?]], we would
 * need a new version of [[add-element]], which would
 * use [[al-member?]] instead of [[member?]]. And so on.
 * The wrong path leads to a destination at which
 * everything except [[emptyset]] is reimplemented. And
 * then if anyone wants sets of sets, all the operations
 * have to be reimplemented again. The destination is a
 * maintainer's hell, where there are several different
 * implementations of sets, all using nearly identical
 * code and all broken in the same way. For example, the
 * implementation above performs badly on large sets,
 * and if better performance is needed, any improvement
 * has to be reimplemented N times. Instead of
 * collecting monomorphic implementations of sets, a
 * better way is to use higher-order functions to write
 * one implementation that's polymorphic.
 * 
 * Approaches\cullchap to polymorphism in Scheme
 * 
 * [*]
 * 
 * In Scheme, polymorphic set functions can be
 * implemented in three styles. All three use
 * higher-order functions; instead of using [[equal?]],
 * the set functions use an equality predicate that is
 * stored in a data structure or passed as a parameter.
 * The style is identified by the location in which the
 * predicate is stored:
 * 
 *   • In the simplest style, a new parameter, the
 *  equality predicate [[my-equal?]], is added to
 *  every function. The modified functions look like
 *  this:
 * <print.c>=
 */
void bprint(Printbuf output, const char *fmt, ...) {
    va_list_box box;

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(output, fmt, &box);
    va_end(box.ap);
}
/*
 * Function [[print]] buffers, then prints. It keeps a
 * buffer in a cache. \implabelprint
 * <print.c>=
 */
void print(const char *fmt, ...) {
    va_list_box box;
    static Printbuf stdoutbuf;

    if (stdoutbuf == NULL)
        stdoutbuf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(stdoutbuf, fmt, &box);
    va_end(box.ap);
    fwritebuf(stdoutbuf, stdout);
    bufreset(stdoutbuf);
    fflush(stdout);
}
/*
 * Function [[fprint]] caches its own buffer. \implabel
 * fprint
 * <print.c>=
 */
void fprint(FILE *output, const char *fmt, ...) {
    static Printbuf buf;
    va_list_box box;

    if (buf == NULL)
        buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, output);
    fflush(output);
    freebuf(&buf);
}
/*
 * <print.c>=
 */
static Printer *printertab[256];

void vbprint(Printbuf output, const char *fmt, va_list_box *box) {
    const unsigned char *p;
    bool broken = false; // made true on seeing an unknown conversion specifier
    for (p = (const unsigned char*)fmt; *p; p++) {
        if (*p != '%') {
            bufput(output, *p);
        } else {
            if (!broken && printertab[*++p])
                printertab[*p](output, box);
            else {
                broken = true;  /* box is not consumed */
                bufputs(output, "<pointer>");
            }
        }
    }
}
/*
 * The [[va_arg]] interface is unsafe, and if a printing
 * function takes the wrong thing from [[box]], a memory
 * error could ensue. So if [[vbprint]] ever sees a
 * conversion specifier that it doesn't recognize,
 * it stops calling printing functions.
 */

/*
 * Function [[installprinter]] simply stores to the
 * private table. \implabelinstallprinter
 * <print.c>=
 */
void installprinter(unsigned char c, Printer *take_and_print) {
    printertab[c] = take_and_print;
}
