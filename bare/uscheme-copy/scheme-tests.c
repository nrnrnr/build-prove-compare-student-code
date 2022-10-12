#include "all.h"
/* scheme-tests.c S332c */
void process_tests(UnitTestlist tests, Env rho) {
    set_error_mode(TESTING);
    int npassed = number_of_good_tests(tests, rho);
    set_error_mode(NORMAL);
    int ntests  = lengthUL(tests);
    report_test_results(npassed, ntests);
}
/* scheme-tests.c S333b */
int number_of_good_tests(UnitTestlist tests, Env rho) {
    if (tests == NULL)
        return 0;
    else {
        int n = number_of_good_tests(tests->tl, rho);
        switch (test_result(tests->hd, rho)) {
        case TEST_PASSED: return n+1;
        case TEST_FAILED: return n;
        default:          assert(0);
        }
    }
}
/* scheme-tests.c S333d */
TestResult test_result(UnitTest t, Env rho) {
    switch (t->alt) {
    case CHECK_EXPECT:
        /* run [[check-expect]] test [[t]], returning [[TestResult]] S334a */
        {   if (setjmp(testjmp)) {

/* report that evaluating [[t->check_expect.check]] failed with an error S335b */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->check_expect.check, t->check_expect.expect,
                               t->check_expect.check, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value check = eval(testexp(t->check_expect.check),  rho);
            if (setjmp(testjmp)) {

/* report that evaluating [[t->check_expect.expect]] failed with an error S335c */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->check_expect.check, t->check_expect.expect,
                               t->check_expect.expect, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            pushreg(&check);
            Value expect = eval(testexp(t->check_expect.expect), rho);
            popreg(&check);

            if (!equalpairs(check, expect)) {
                /* report failure because the values are not equal S335a */
                fprint(stderr,
                           "Check-expect failed: expected %e to evaluate to %v",
                       t->check_expect.check, expect);
                if (t->check_expect.expect->alt != LITERAL)
                    fprint(stderr, " (from evaluating %e)", t->
                                                           check_expect.expect);
                fprint(stderr, ", but it's %v.\n", check);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ASSERT:
        /* run [[check-assert]] test [[t]], returning [[TestResult]] S334b */
        {   if (setjmp(testjmp)) {

     /* report that evaluating [[t->check_assert]] failed with an error S335e */
                fprint(stderr,
                    "Check-assert failed: evaluating %e causes an error: %s.\n",
                               t->check_assert, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value v = eval(testexp(t->check_assert), rho);

            if (v.alt == BOOLV && !v.boolv) {
                /* report failure because the value is false S335d */
                fprint(stderr, "Check-assert failed: %e evaluates to #f.\n", t->
                                                                  check_assert);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ERROR:
        /* run [[check-error]] test [[t]], returning [[TestResult]] S334c */
        {   if (setjmp(testjmp)) {
                bufreset(errorbuf);
                return TEST_PASSED; // error occurred, so the test passed
            }
            Value check = eval(testexp(t->check_error),  rho);

        /* report that evaluating [[t->check_error]] produced [[check]] S335f */
            fprint(stderr,
                    "Check-error failed: evaluating %e was expected to produce "
                           "an error, but instead it produced the value %v.\n",
                           t->check_error, check);
            return TEST_FAILED;
        }    
    default:
        assert(0);
    }
}
/* scheme-tests.c S335h */
bool equalpairs(Value v, Value w) {
    if (v.alt != w.alt)
        return false;
    else
        switch (v.alt) {
        case PAIR:
            return equalpairs(*v.pair.car, *w.pair.car) &&
                   equalpairs(*v.pair.cdr, *w.pair.cdr);
        case NUM:
            return v.num   == w.num;
        case BOOLV:
            return v.boolv == w.boolv;
        case SYM:
            return v.sym   == w.sym;
        case NIL:
            return true;
        default:
            return false;
        }
}
