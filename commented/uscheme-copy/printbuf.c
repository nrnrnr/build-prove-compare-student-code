#include "all.h"
/*
 * <printbuf.c>=
 */
struct Printbuf {
    char *chars;  // start of the buffer
    char *limit;  // marks one past end of buffer
    char *next;   // where next character will be buffered
    // invariants: all are non-NULL
    //             chars <= next <= limit
    //             if chars <= p < limit, then *p is writeable
};
/*
 * A buffer initially holds 100 characters.
 * <printbuf.c>=
 */
Printbuf printbuf(void) {
   Printbuf buf = malloc(sizeof(*buf));
   assert(buf);
   int n = 100;
   buf->chars = malloc(n);
   assert(buf->chars);
   buf->next  = buf->chars;
   buf->limit = buf->chars + n;
   return buf;
}
/*
 * <printbuf.c>=
 */
void freebuf(Printbuf *bufp) {
   Printbuf buf = *bufp;
   assert(buf && buf->chars);
   free(buf->chars);
   free(buf);
   *bufp = NULL;
}
/*
 * Calling [[grow]] makes a buffer 30% larger, or at
 * least 1 byte larger.
 * <printbuf.c>=
 */
static void grow(Printbuf buf) {
    assert(buf && buf->chars && buf->next && buf->limit);
    unsigned n = buf->limit - buf->chars;
    n = 1 + (n * 13) / 10;   // 30% size increase
    unsigned i = buf->next - buf->chars;
    buf->chars = realloc(buf->chars, n);
    assert(buf->chars);
    buf->next  = buf->chars + i;
    buf->limit = buf->chars + n;
}
/*
 * <printbuf.c>=
 */
void bufput(Printbuf buf, char c) {
    assert(buf && buf->next && buf->limit);
    if (buf->next == buf->limit) {
        grow(buf);
        assert(buf && buf->next && buf->limit);
        assert(buf->limit > buf->next);
    }
    *buf->next++ = c;
}
/*
 * Function [[bufputs]], which writes an entire string,
 * first ensures that the buffer is large enough to hold
 * the string, then copies the string by calling
 * [[memcpy]].
 * <printbuf.c>=
 */
void bufputs(Printbuf buf, const char *s) {
    assert(buf);
    int n = strlen(s);
    while (buf->limit - buf->next < n)
        grow(buf);
    memcpy(buf->next, s, n);
    buf->next += n;
}
/*
 * <printbuf.c>=
 */
void bufreset(Printbuf buf) {
    assert(buf && buf->next);
    buf->next = buf->chars;
}
/*
 * \qbreak To use the buffer, client code may want to
 * know how many characters are in it.
 * <printbuf.c>=
 */
static int nchars(Printbuf buf) {
    assert(buf && buf->chars && buf->next);
    return buf->next - buf->chars;
}
/*
 * The last solver function,
 * [[find-lit-true-assignment]], succeeds either when
 * the current assignment already satisfies its literal,
 * or when it can be made to satisfy the literal by
 * adding a binding. There is at most one way to
 * succeed, so the resumption continuation passed to
 * [[succeed]] is always [[fail]]. {llaws*} \monolaw[,]
 * (find-lit-true-assignment lit cur fail succeed)
 * (succeed cur fail) \lawlinewhen [[cur]] satisfies
 * [[lit]] \monolaw[,](find-lit-true-assignment lit cur
 * fail succeed) (succeed (bind x v cur) fail) \lawline
 * when x is [[lit]]'s variable, [[cur]] does not bind x
 * , and [[lit]] is satisfied by {x |->v} \monolaw[,
 * otherwise](find-lit-true-assignment lit cur fail
 * succeed) (fail) {llaws*} To determine that [[cur]]
 * does not bind x, the solver function uses [[find-c]]
 * with yet another pair of continuations:
 * <printbuf.c>=
 */
char *bufcopy(Printbuf buf) {
   assert(buf);
   int n = nchars(buf);
   char *s = malloc(n+1);
   assert(s);
   memcpy(s, buf->chars, n);
   s[n] = '\0';
   return s;
}
/*
 * <printbuf.c>=
 */
void fwritebuf(Printbuf buf, FILE *output) {
    assert(buf && buf->chars && buf->limit);
    assert(output);
    int n = fwrite(buf->chars, sizeof(*buf->chars), nchars(buf), output);
    assert(n == nchars(buf));
}
