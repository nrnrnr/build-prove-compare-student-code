#include "all.h"
/* loc.c 162b */
Value* allocate(Value v) {
    Value *loc = malloc(sizeof(*loc));
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/* loc.c S317b */
void initallocate(Env *globals) {
    (void)globals;
}
