#include "all.h"
/* unicode.c S188b */
void fprint_utf8(FILE *output, unsigned code_point) {
    if ((code_point & 0x1fffff) != code_point)
        runerror("%d does not represent a Unicode code point", (int)code_point);
    if (code_point > 0xffff) {     // 21 bits
        putc(0xf0 |  (code_point >> 18),         output);
        putc(0x80 | ((code_point >> 12) & 0x3f), output);
        putc(0x80 | ((code_point >>  6) & 0x3f), output);
        putc(0x80 | ((code_point      ) & 0x3f), output);
    } else if (code_point > 0x7ff) { // 16 bits
        putc(0xe0 | (code_point >> 12),         output);
        putc(0x80 | ((code_point >> 6) & 0x3f), output);
        putc(0x80 | ((code_point     ) & 0x3f), output);
    } else if (code_point > 0x7f) { // 12 bits
        putc(0xc0 | (code_point >> 6),         output);
        putc(0x80 | (code_point & 0x3f),       output);
    } else {                        // 7 bits
        putc(code_point, output);
    }
}
/* unicode.c S188c */
void print_utf8(unsigned code_point) {
    fprint_utf8(stdout, code_point);
}
