#include "all.h"
Def mkVal(Name name, Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAL;
    n->val.name = name;
    n->val.exp = exp;
    return n;
}

Def mkExp(Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = EXP;
    n->exp = exp;
    return n;
}

Def mkDefine(Name name, Lambda lambda) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEFINE;
    n->define.name = name;
    n->define.lambda = lambda;
    return n;
}

Def mkDefs(Deflist defs) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEFS;
    n->defs = defs;
    return n;
}

struct Def mkValStruct(Name name, Exp exp) {
    struct Def n;
    
    n.alt = VAL;
    n.val.name = name;
    n.val.exp = exp;
    return n;
}

struct Def mkExpStruct(Exp exp) {
    struct Def n;
    
    n.alt = EXP;
    n.exp = exp;
    return n;
}

struct Def mkDefineStruct(Name name, Lambda lambda) {
    struct Def n;
    
    n.alt = DEFINE;
    n.define.name = name;
    n.define.lambda = lambda;
    return n;
}

struct Def mkDefsStruct(Deflist defs) {
    struct Def n;
    
    n.alt = DEFS;
    n.defs = defs;
    return n;
}

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

Exp mkLiteral(Value literal) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LITERAL;
    n->literal = literal;
    return n;
}

Exp mkVar(Name var) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAR;
    n->var = var;
    return n;
}

Exp mkSet(Name name, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = SET;
    n->set.name = name;
    n->set.exp = exp;
    return n;
}

Exp mkIfx(Exp cond, Exp truex, Exp falsex) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = IFX;
    n->ifx.cond = cond;
    n->ifx.truex = truex;
    n->ifx.falsex = falsex;
    return n;
}

Exp mkWhilex(Exp cond, Exp body) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = WHILEX;
    n->whilex.cond = cond;
    n->whilex.body = body;
    return n;
}

Exp mkBegin(Explist begin) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = BEGIN;
    n->begin = begin;
    return n;
}

Exp mkLetx(Letkeyword let, Namelist xs, Explist es, Exp body) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LETX;
    n->letx.let = let;
    n->letx.xs = xs;
    n->letx.es = es;
    n->letx.body = body;
    return n;
}

Exp mkLambdax(Lambda lambdax) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LAMBDAX;
    n->lambdax = lambdax;
    return n;
}

Exp mkApply(Exp fn, Explist actuals) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = APPLY;
    n->apply.fn = fn;
    n->apply.actuals = actuals;
    return n;
}

Exp mkBreakx(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = BREAKX;
    
    return n;
}

Exp mkContinuex(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = CONTINUEX;
    
    return n;
}

Exp mkReturnx(Exp returnx) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = RETURNX;
    n->returnx = returnx;
    return n;
}

Exp mkThrow(Name label, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = THROW;
    n->throw.label = label;
    n->throw.exp = exp;
    return n;
}

Exp mkTryCatch(Exp body, Name label, Exp handler) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = TRY_CATCH;
    n->try_catch.body = body;
    n->try_catch.label = label;
    n->try_catch.handler = handler;
    return n;
}

Exp mkLongLabel(Name label, Exp body) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LONG_LABEL;
    n->long_label.label = label;
    n->long_label.body = body;
    return n;
}

Exp mkLongGoto(Name label, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LONG_GOTO;
    n->long_goto.label = label;
    n->long_goto.exp = exp;
    return n;
}

Exp mkEnv(Env contents, SavedEnvTag tag) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = ENV;
    n->env.contents = contents;
    n->env.tag = tag;
    return n;
}

Exp mkHole(void) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = HOLE;
    
    return n;
}

Exp mkLowered(Exp before, Exp after) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LOWERED;
    n->lowered.before = before;
    n->lowered.after = after;
    return n;
}

Exp mkLoopback(Exp loopback) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LOOPBACK;
    n->loopback = loopback;
    return n;
}

struct Exp mkLiteralStruct(Value literal) {
    struct Exp n;
    
    n.alt = LITERAL;
    n.literal = literal;
    return n;
}

struct Exp mkVarStruct(Name var) {
    struct Exp n;
    
    n.alt = VAR;
    n.var = var;
    return n;
}

struct Exp mkSetStruct(Name name, Exp exp) {
    struct Exp n;
    
    n.alt = SET;
    n.set.name = name;
    n.set.exp = exp;
    return n;
}

struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex) {
    struct Exp n;
    
    n.alt = IFX;
    n.ifx.cond = cond;
    n.ifx.truex = truex;
    n.ifx.falsex = falsex;
    return n;
}

struct Exp mkWhilexStruct(Exp cond, Exp body) {
    struct Exp n;
    
    n.alt = WHILEX;
    n.whilex.cond = cond;
    n.whilex.body = body;
    return n;
}

struct Exp mkBeginStruct(Explist begin) {
    struct Exp n;
    
    n.alt = BEGIN;
    n.begin = begin;
    return n;
}

struct Exp mkLetxStruct(Letkeyword let,
    Namelist xs,
    Explist es,
    Exp body) {
    struct Exp n;
    
    n.alt = LETX;
    n.letx.let = let;
    n.letx.xs = xs;
    n.letx.es = es;
    n.letx.body = body;
    return n;
}

struct Exp mkLambdaxStruct(Lambda lambdax) {
    struct Exp n;
    
    n.alt = LAMBDAX;
    n.lambdax = lambdax;
    return n;
}

struct Exp mkApplyStruct(Exp fn, Explist actuals) {
    struct Exp n;
    
    n.alt = APPLY;
    n.apply.fn = fn;
    n.apply.actuals = actuals;
    return n;
}

struct Exp mkBreakxStruct(void) {
    struct Exp n;
    
    n.alt = BREAKX;
    
    return n;
}

struct Exp mkContinuexStruct(void) {
    struct Exp n;
    
    n.alt = CONTINUEX;
    
    return n;
}

struct Exp mkReturnxStruct(Exp returnx) {
    struct Exp n;
    
    n.alt = RETURNX;
    n.returnx = returnx;
    return n;
}

struct Exp mkThrowStruct(Name label, Exp exp) {
    struct Exp n;
    
    n.alt = THROW;
    n.throw.label = label;
    n.throw.exp = exp;
    return n;
}

struct Exp mkTryCatchStruct(Exp body, Name label, Exp handler) {
    struct Exp n;
    
    n.alt = TRY_CATCH;
    n.try_catch.body = body;
    n.try_catch.label = label;
    n.try_catch.handler = handler;
    return n;
}

struct Exp mkLongLabelStruct(Name label, Exp body) {
    struct Exp n;
    
    n.alt = LONG_LABEL;
    n.long_label.label = label;
    n.long_label.body = body;
    return n;
}

struct Exp mkLongGotoStruct(Name label, Exp exp) {
    struct Exp n;
    
    n.alt = LONG_GOTO;
    n.long_goto.label = label;
    n.long_goto.exp = exp;
    return n;
}

struct Exp mkEnvStruct(Env contents, SavedEnvTag tag) {
    struct Exp n;
    
    n.alt = ENV;
    n.env.contents = contents;
    n.env.tag = tag;
    return n;
}

struct Exp mkHoleStruct(void) {
    struct Exp n;
    
    n.alt = HOLE;
    
    return n;
}

struct Exp mkLoweredStruct(Exp before, Exp after) {
    struct Exp n;
    
    n.alt = LOWERED;
    n.lowered.before = before;
    n.lowered.after = after;
    return n;
}

struct Exp mkLoopbackStruct(Exp loopback) {
    struct Exp n;
    
    n.alt = LOOPBACK;
    n.loopback = loopback;
    return n;
}

