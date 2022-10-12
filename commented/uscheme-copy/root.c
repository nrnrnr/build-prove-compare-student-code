#include "all.h"
/*
 * <root.c>=
 */
struct Roots roots = { { NULL, { NULL } }, NULL, NULL };
/*
 * Register roots are pushed and popped by [[pushreg]]
 * and [[popreg]].
 * <root.c>=
 */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg) {
    roots.registers = mkRL(reg, roots.registers);
}
/*
 * When a register is popped, [[popreg]] insists that
 * the register being popped actually be present at the
 * head of the register list.
 * <root.c>=
 */
void popreg(Value *reg) {
    Registerlist regs = roots.registers;
    assert(regs != NULL);
    assert(reg == regs->hd);
    roots.registers = regs->tl;
    free(regs);
}
#endif /*OMIT*/
/*
 * When a list of registers is pushed, that list must be
 * popped in the opposite order (last in, first out).
 * Here registers are pushed left to right and popped
 * right to left.
 * <root.c>=
 */
void pushregs(Valuelist regs) {
    for (; regs; regs = regs->tl)
        pushreg(&regs->hd);
}

void popregs (Valuelist regs) {
    if (regs != NULL) {
        popregs(regs->tl);
        popreg(&regs->hd);
    }
}
