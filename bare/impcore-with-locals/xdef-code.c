#include "all.h"
XDef mkDef(Def def) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEF;
    n->def = def;
    return n;
}

XDef mkUse(Name use) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = USE;
    n->use = use;
    return n;
}

XDef mkTest(UnitTest test) {
    XDef n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = TEST;
    n->test = test;
    return n;
}

struct XDef mkDefStruct(Def def) {
    struct XDef n;
    
    n.alt = DEF;
    n.def = def;
    return n;
}

struct XDef mkUseStruct(Name use) {
    struct XDef n;
    
    n.alt = USE;
    n.use = use;
    return n;
}

struct XDef mkTestStruct(UnitTest test) {
    struct XDef n;
    
    n.alt = TEST;
    n.test = test;
    return n;
}

UnitTest mkCheckExpect(Exp check, Exp expect) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_EXPECT;
    n->check_expect.check = check;
    n->check_expect.expect = expect;
    return n;
}

UnitTest mkCheckAssert(Exp check_assert) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_ASSERT;
    n->check_assert = check_assert;
    return n;
}

UnitTest mkCheckError(Exp check_error) {
    UnitTest n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CHECK_ERROR;
    n->check_error = check_error;
    return n;
}

struct UnitTest mkCheckExpectStruct(Exp check, Exp expect) {
    struct UnitTest n;
    
    n.alt = CHECK_EXPECT;
    n.check_expect.check = check;
    n.check_expect.expect = expect;
    return n;
}

struct UnitTest mkCheckAssertStruct(Exp check_assert) {
    struct UnitTest n;
    
    n.alt = CHECK_ASSERT;
    n.check_assert = check_assert;
    return n;
}

struct UnitTest mkCheckErrorStruct(Exp check_error) {
    struct UnitTest n;
    
    n.alt = CHECK_ERROR;
    n.check_error = check_error;
    return n;
}

