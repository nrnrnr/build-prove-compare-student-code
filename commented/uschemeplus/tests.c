#include "all.h"
/*
 * Test processing and reporting
 * 
 * Code that runs unit tests has to call
 * [[process_test]], which is language-dependent. That
 * code is found in \crefimpcorea.chap,schemea.chap. But
 * the code that reports the results is
 * language-independent and is found here: [*]
 * <tests.c>=
 */
void report_test_results(int npassed, int ntests) {
    switch (ntests) {
    case 0: break; /* no report */
    case 1:
        if (npassed == 1)
            printf("The only test passed.\n");
        else
            printf("The only test failed.\n");
        break;
    case 2:
        switch (npassed) {
        case 0: printf("Both tests failed.\n"); break;
        case 1: printf("One of two tests passed.\n"); break;
        case 2: printf("Both tests passed.\n"); break;
        default: assert(0); break;
        }
        break;
    default:
        if (npassed == ntests)
            printf("All %d tests passed.\n", ntests);
        else if (npassed == 0) 
            printf("All %d tests failed.\n", ntests);
        else
            printf("%d of %d tests passed.\n", npassed, ntests);
        break;
    }
}
