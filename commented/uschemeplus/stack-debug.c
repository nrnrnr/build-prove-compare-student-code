#include "all.h"
/*
 * Tracing machine state using the stack
 * 
 * The state of the stack can be displayed at every step
 * of an evaluation. The steps are identified by
 * variables [[etick]] and [[vtick]], which count the
 * number of state transitions involving an expression
 * or a variable as the current item, respectively.
 * <stack-debug.c>=
 */
static int etick, vtick;  // number of times saw a current expression or value
/*
 * Tracing is controlled by the value of the \
 * uschemeplus variable [[ --- trace-stack]]. Placing
 * control in a \uschemeplus variable enables \
 * uschemeplus code to turn tracing on and off during a
 * single call to [[eval]]. If the value of [[ ---
 * trace-stack]] is a number, then pointer
 * [[trace_countp]] points to that value.
 * <stack-debug.c>=
 */
static int *trace_countp; // if not NULL, points to value of &trace-stack
/*
 * All three variables are initialized by function
 * [[stack_trace_init]], which receives a pointer to [[
 * --- trace-stack]] if it exists and is a number.
 * <stack-debug.c>=
 */
void stack_trace_init(int *countp) { 
    etick = vtick = 0; 
    trace_countp = countp;
}
/*
 * The [[etick]] number is shown when tracing a current
 * expression. So are the expression, a pointer to the
 * environment, and the stack. And every time a trace is
 * shown, the trace count is decremented.
 * <stack-debug.c>=
 */
void stack_trace_current_expression(Exp e, Env rho, Stack s) {
    if (trace_countp && *trace_countp != 0) {
        (*trace_countp)--;
        etick++;
        fprint(stderr, "exp  %d = %e\n", etick, e);
        fprint(stderr, "env  %R\n", rho);
        fprint(stderr, "stack\n%S\n", s);
    }
}
/*
 * When tracing a current value, the [[vtick]] number is
 * used in the same way. And the empty stack is rendered
 * to show that when there is a current value and an
 * empty stack, evaluation is complete.
 * <stack-debug.c>=
 */
void stack_trace_current_value(Value v, Env rho, Stack s) {
    if (trace_countp && *trace_countp != 0) {
        (*trace_countp)--;
        vtick++;
        fprint(stderr, "val  %d = %v\n", vtick, v);
        fprint(stderr, "env  %R\n", rho);
        if (topframe(s)) 
            fprint(stderr, "stack\n%S\n", s);
        else 
            fprint(stderr, " (final answer from stack-based eval)\n");
    }
}
