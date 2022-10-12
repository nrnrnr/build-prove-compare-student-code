#include "all.h"
/*
 * Code to run unit tests
 * 
 * [*] Running a list of unit tests is the job of the
 * function [[process_tests]]:
 * <imptests.c>=
 */
void process_tests(UnitTestlist tests, Valenv globals, Funenv functions) {
    set_error_mode(TESTING);
    int npassed = number_of_good_tests(tests, globals, functions);
    set_error_mode(NORMAL);
    int ntests  = lengthUL(tests);
    report_test_results(npassed, ntests);
}
/*
 * Function [[number_of_good_tests]] runs each test,
 * last one first, and counts the number that pass.
 * So it can catch errors during testing, it expects the
 * error mode to be [[TESTING]]; calling
 * [[number_of_good_tests]] when the error mode is
 * [[NORMAL]] is an unchecked run-time error. \iintlabel
 * number_of_good_tests
 */

/*
 * The list of tests coming in contains the last test
 * first, but the first test is the one that must be run
 * first. Function [[number_of_good_tests]] therefore
 * recursively runs [[tests->tl]] before calling
 * [[test_result]] on [[tests->hd]]. It returns the
 * number of tests passed.
 * <imptests.c>=
 */
int number_of_good_tests(UnitTestlist tests, Valenv globals, Funenv functions) {
    if (tests == NULL)
        return 0;
    else {
        int n = number_of_good_tests(tests->tl, globals, functions);
        switch (test_result(tests->hd, globals, functions)) {
        case TEST_PASSED: return n+1;
        case TEST_FAILED: return n;
        default:          assert(0);
        }
    }
}
/*
 * If the list [[tests]] were very long, this recursion
 * might blow the C stack. But the list is only as long
 * as the number of tests written in the source code,
 * I don't expect more than dozens of tests, for which
 * default stack space should be adequate. A huge test
 * suite might have to be broken into multiple files.
 * 
 * The heavy lifting is done by function
 * [[test_result]], which returns a value of type
 * [[TestResult]]. \iilabelTestResult
 */

/*
 * Function [[test_result]] handles every kind of unit
 * test. In Impcore there are three kinds:
 * [[check-expect]], [[check-assert]], and
 * [[check-error]]. Typed languages, starting with Typed
 * Impcore in \creftypesys.chap, have more. [*]
 * <imptests.c>=
 */
TestResult test_result(UnitTest t, Valenv globals, Funenv functions) {
    switch (t->alt) {
    case CHECK_EXPECT:
        /*
         * A [[check-expect]] test is run by running both the
         * ``check'' and the ``expect'' expressions, each under
         * the protection of an error handler. If an error
         * occurs under either evaluation, the test fails.
         * Otherwise values [[check]] and [[expect]] are
         * compared. If they differ, the test fails; if not, the
         * test passes. All failures trigger error messages.
         * <run [[check-expect]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                /*
                 * <report that evaluating [[t->check_expect.check]] failed with
                                                                      an error>=
                 */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->check_expect.check, t->check_expect.expect,
                               t->check_expect.check, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value check = eval(t->check_expect.check, globals, functions,
                                                                     empty_env);

            if (setjmp(testjmp)) {
                /*
                 * <report that evaluating [[t->check_expect.expect]] failed
                                                                 with an error>=
                 */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->check_expect.check, t->check_expect.expect,
                               t->check_expect.expect, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value expect = eval(t->check_expect.expect, globals, functions,
                                                                     empty_env);

            if (check != expect) {
                /*
                 * Error-reporting code is voluminous but uninteresting.
                 * <report failure because the values are not equal>=
                 */
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
        /*
         * \qvfilbreak1.2in
         * 
         * A [[check-assert]] test is run by evaluating just one
         * expression, which should evaluate, without error, to
         * a nonzero value.
         * <run [[check-assert]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                /*
                 * <report that evaluating [[t->check_assert]] failed with an
                                                                         error>=
                 */
                fprint(stderr,
                    "Check-assert failed: evaluating %e causes an error: %s.\n",
                               t->check_assert, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value v = eval(t->check_assert, globals, functions, empty_env);

            if (v == 0) {
                /*
                 * <report failure because the value is zero>=
                 */
                fprint(stderr, "Check-assert failed: %e evaluated to 0.\n", t->
                                                                  check_assert);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ERROR:
        /*
         * \qbreak A [[check-error]] error test is also run by
         * evaluating an expression under the protection of an
         * error handler, but this time, if an error occurs, the
         * test passes. If not, the test fails.
         * <run [[check-error]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                bufreset(errorbuf);
                return TEST_PASSED; // error occurred, so the test passed
            }
            Value check = eval(t->check_error, globals, functions, empty_env);
            /*
             * [*]
             * <report that evaluating [[t->check_error]] produced [[check]]>=
             */
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
