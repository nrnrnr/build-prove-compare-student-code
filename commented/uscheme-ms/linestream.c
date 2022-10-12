#include "all.h"
/*
 * The stream-creator functions do the minimum needed to
 * establish the invariants of a [[Linestream]]. To
 * clear fields that should be zero, they use the
 * standard C function [[calloc]].
 * <linestream.c>=
 */
Linestream stringlines(const char *stringname, const char *s) {
    Linestream lines = calloc(1, sizeof(*lines));
    assert(lines);
    lines->source.sourcename = stringname;
    /*
     * <check to see that [[s]] is empty or ends in a newline>=
     */
    {   int n = strlen(s);
        assert(n == 0 || s[n-1] == '\n');
    }
    lines->s = s;
    return lines;
}
/*
 * <linestream.c>=
 */
Linestream filelines(const char *filename, FILE *fin) {
    Linestream lines = calloc(1, sizeof(*lines));
    assert(lines);
    lines->source.sourcename = filename;
    lines->fin = fin;
    return lines;
}
/*
 * \qbreak Function [[getline_]] returns a pointer to
 * the next line from the input, which is held in 
 * [[buf]], a buffer that is reused on subsequent calls.
 * Function [[growbuf]] makes sure the buffer is at
 * least [[n]] bytes long.
 * <linestream.c>=
 */
static void growbuf(Linestream lines, int n) {
    assert(lines);
    if (lines->bufsize < n) {
        lines->buf = realloc(lines->buf, n);
        assert(lines->buf != NULL);
        lines->bufsize = n;
    }
}
/*
 * Here's a secret: I've tweaked [[getline_]] to check
 * and see if the line read begins with the special
 * string [[;#]]. If so, the line is printed. This
 * string is a special comment that helps me test all
 * the \LAtranscript\RA examples in the book. \implabel
 * getline_
 * <linestream.c>=
 */
char* getline_(Linestream lines, const char *prompt) {
    assert(lines);
    if (prompt)
        print("%s", prompt);

    lines->source.line++;
    if (lines->fin)
        /*
         * To get a line from a file, [[getline_]] calls the C
         * standard library function [[fgets]]. If the buffer is
         * big enough, [[fgets]] returns exactly the next line.
         * If the buffer isn't big enough, [[getline_]] grows
         * the buffer and calls [[fgets]] again, to get more of
         * the line. This process is iterated until the last
         * character in the buffer is a newline. Then
         * [[getline_]] chops off the terminating newline by
         * overwriting it with [['\0']].
         * <set [[lines->buf]] to next line from file [[lines->fin]], or return
                                               [[NULL]] if lines are exhausted>=
         */
        {
            int n; /* number of characters read into the buffer */

            for (n = 0; n == 0 || lines->buf[n-1] != '\n'; n = strlen(lines->buf
                                                                            )) {
                growbuf(lines, n+512);
                if (fgets(lines->buf+n, 512, lines->fin) == NULL)
                    break;
            }
            if (n == 0)
                return NULL;
            if (lines->buf[n-1] == '\n')
                lines->buf[n-1] = '\0';
        }
    else if (lines->s)
        /*
         * <set [[lines->buf]] to next line from string [[lines->s]], or return
                                               [[NULL]] if lines are exhausted>=
         */
        {
            const char *p = strchr(lines->s, '\n');
            if (p == NULL)
                return NULL;
            p++;
            int len = p - lines->s;
            growbuf(lines, len);
            strncpy(lines->buf, lines->s, len);
            lines->buf[len-1] = '\0';   /* no newline */
            lines->s = p;
        }
    else
        assert(0);

    if (lines->buf[0] == ';' && lines->buf[1] == '#')
        print("%s\n", lines->buf);

    return lines->buf;
}
