#include "all.h"
/*
 * <name.c>=
 */
struct Name {
    const char *s;
};
/*
 * Returning the string associated with a name is
 * trivial.
 * <name.c>=
 */
const char* nametostr(Name np) {
    assert(np != NULL);
    return np->s;
}
/*
 * <name.c>=
 */
Name strtoname(const char *s) {
    static Namelist all_names;
    assert(s != NULL);

    for (Namelist unsearched = all_names; unsearched; unsearched = unsearched->
                                                                             tl)
        if (strcmp(s, unsearched->hd->s) == 0)
            return unsearched->hd;

    /*
     * A faster implementation might use a search tree or a
     * hash table, not a simple list. Such an implementation
     * is described by \citet[chapter 3]hanson:interfaces.
     * 
     * If the string [[s]] isn't associated with any name on
     * the list [[all_names]], then [[strtoname]] makes a
     * new name and adds it.
     * <allocate a new name, add it to [[all_names]], and return it>=
     */
    Name np = malloc(sizeof(*np));
    assert(np != NULL);
    np->s = malloc(strlen(s) + 1);
    assert(np->s != NULL);
    strcpy((char*)np->s, s);
    all_names = mkNL(np, all_names);
    return np;
}
