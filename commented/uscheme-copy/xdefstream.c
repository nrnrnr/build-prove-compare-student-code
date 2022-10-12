#include "all.h"
/*
 * Streams of extended definitions
 * 
 * [*] Layered on top of a [[Parstream]] is an
 * [[XDefstream]]. One [[Par]] in the input corresponds
 * exactly to one [[XDef]], so the only state needed in
 * an [[XDefstream]] is the [[Parstream]] it is made
 * from. \implabelXDefstream
 * <xdefstream.c>=
 */
struct XDefstream {
    Parstream pars;                  /* where input comes from */
};
/*
 * To make an [[XDefstream]], allocate and initialize. \
 * implabelxdefstream \intlabelxdefstream
 * <xdefstream.c>=
 */
XDefstream xdefstream(Parstream pars) {
    XDefstream xdefs = malloc(sizeof(*xdefs));
    assert(xdefs);
    assert(pars);
    xdefs->pars = pars;
    return xdefs;
}
/*
 * The code in \crefimpcore.chap doesn't even know that
 * [[Parstream]]s exist. It builds [[XDefstream]]s by
 * calling [[filexdefs]] or [[stringxdefs]]. In their
 * turn, those functions build [[XDefstream]]s by
 * combining [[xdefstream]] and [[parstream]] with
 * either [[filelines]] or [[stringlines]],
 * respectively. \implabelfilexdefs\implabelstringxdefs
 * <xdefstream.c>=
 */
XDefstream filexdefs(const char *filename, FILE *input, Prompts prompts) {
    return xdefstream(parstream(filelines(filename, input), prompts));
}
XDefstream stringxdefs(const char *stringname, const char *input) {
    return xdefstream(parstream(stringlines(stringname, input), NOT_PROMPTING));
}
/*
 * To get an extended definition from an [[XDefstream]],
 * get a [[Par]] and parse it. The heavy lifting is done
 * by [[parsexdef]], which is the subject of \cref
 * cparse.chap. \implabelgetxdef
 * <xdefstream.c>=
 */
XDef getxdef(XDefstream xdr) {
    Par p = getpar(xdr->pars);
    if (p == NULL) 
        return NULL;
    else
        return parsexdef(p, parsource(xdr->pars));
}
