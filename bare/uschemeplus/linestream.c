#include "all.h"
/* linestream.c S166b */
Linestream stringlines(const char *stringname, const char *s) {
    Linestream lines = calloc(1, sizeof(*lines));
    assert(lines);
    lines->source.sourcename = stringname;
    /* check to see that [[s]] is empty or ends in a newline S166d */
    {   int n = strlen(s);
        assert(n == 0 || s[n-1] == '\n');
    }
    lines->s = s;
    return lines;
}
/* linestream.c S166c */
Linestream filelines(const char *filename, FILE *fin) {
    Linestream lines = calloc(1, sizeof(*lines));
    assert(lines);
    lines->source.sourcename = filename;
    lines->fin = fin;
    return lines;
}
/* linestream.c S167a */
static void growbuf(Linestream lines, int n) {
    assert(lines);
    if (lines->bufsize < n) {
        lines->buf = realloc(lines->buf, n);
        assert(lines->buf != NULL);
        lines->bufsize = n;
    }
}
/* linestream.c S167b */
char* getline_(Linestream lines, const char *prompt) {
    assert(lines);
    if (prompt)
        print("%s", prompt);

    lines->source.line++;
    if (lines->fin)

/* set [[lines->buf]] to next line from file [[lines->fin]], or return [[NULL]] if lines are exhausted S167c */
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

/* set [[lines->buf]] to next line from string [[lines->s]], or return [[NULL]] if lines are exhausted S168a */
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
