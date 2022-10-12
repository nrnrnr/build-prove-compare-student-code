#include "all.h"
/* overflow.c S186b */
static volatile char *low_water_mark = NULL;

#define N 600 // fuel in units of 10,000

static int default_eval_fuel = N * 10000;
static int eval_fuel         = N * 10000;
static bool throttled = 1;
static bool env_checked = 0;

int checkoverflow(int limit) {
  volatile char c;
  if (!env_checked) {
      env_checked = 1;
      const char *options = getenv("BPCOPTIONS");
      if (options == NULL)
          options = "";
      throttled = strstr(options, "nothrottle") == NULL;
  }
  if (low_water_mark == NULL) {
    low_water_mark = &c;
    return 0;
  } else if (low_water_mark - &c >= limit) {
    runerror("recursion too deep");
  } else if (throttled && eval_fuel-- <= 0) {
    eval_fuel = default_eval_fuel;
    runerror("CPU time exhausted");
  } else {
    return (low_water_mark - &c);
  }
}

extern void reset_overflow_check(void) {
  eval_fuel = default_eval_fuel;
}
