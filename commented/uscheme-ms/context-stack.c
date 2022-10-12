#include "all.h"
/*
 * The stack grows toward larger addresses, so a stack 
 * [[s]] satisfies these invariants:
 * 
 *   • The number of frames on the stack is \monoboxs->
 *  sp - s->frames.
 *   • \monoboxs->frames <=\monoboxs->sp <=\monoboxs->
 *  frames + \monoboxs->size\text.
 * 
 * Instrumentation is stored in three global variables.
 * Tail-call optimization is on by default; showing the
 * high stack mark is not.
 * <context-stack.c>=
 */
/*
 * Supporting material for \titleuschemeplus
 * 
 * [*] [*] \invisiblelocaltableofcontents[*]
 * 
 * This appendix presents all the C code that didn't
 * make sense to show in \crefschemes.chap. It also
 * includes a deleted scene and a couple of bonus
 * exercises.
 * 
 * The evaluation stack
 * 
 * This section shows the implementation of the
 * [[Stack]] of evaluation contexts and its
 * instrumentation.
 * 
 * Implementing the stack
 * 
 * In \chaprefschemes, the representation of a [[Stack]]
 * is private to this module. In \chaprefgcs, the
 * representation is exposed to the garbage collector.
 * <representation of [[struct Stack]]>=
 */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};

bool optimize_tail_calls = true;
int  high_stack_mark;   // max number of frames used in the current evaluation
bool show_high_stack_mark;
/*
 * A fresh, empty stack can hold 8 frames.
 * <context-stack.c>=
 */
Stack emptystack(void) {
    Stack s;
    s = malloc(sizeof *s);
    assert(s);
    s->size = 8;
    s->frames = malloc(s->size * sizeof(*s->frames));
    assert(s->frames);
    s->sp = s->frames;
    return s;
}
/*
 * A stack that has already been allocated can be
 * emptied by calling [[clearstack]]. For example,
 * [[evalstack]] may be emptied if a call to [[eval]] is
 * terminated prematurely (with a nonempty stack) by a
 * call to [[error]].
 * <context-stack.c>=
 */
void clearstack (Stack s) {
    s->sp = s->frames;
}
/*
 * \qbreak The frame on top of the stack (that is, the
 * young end) is returned by [[topframe]]. A top frame
 * is present unless the [[sp]] and [[frames]] fields
 * point to the same memory.
 * <context-stack.c>=
 */
Frame *topframe (Stack s) {
    assert(s);
    if (s->sp == s->frames)
        return NULL;
    else
        return s->sp - 1;
}
/*
 * To implement tail-call optimization when [[return]]
 * has been lowered (\crefschemes.ex.lower-return in \
 * crefschemes.chap), you will need to find the frame
 * nearest the top (the youngest frame) that is not a
 * label.
 * <context-stack.c>=
 */
Frame *topnonlabel (Stack s) {
    Frame *p;
    for (p = s->sp; p > s->frames && p[-1].form.alt == LONG_LABEL; p--)
        ;
    if (p > s->frames)
        return p-1;
    else
        return NULL;
}
/*
 * A frame is pushed, whether by [[pushframe]] or
 * [[pushenv_opt]], using the private function [[push]].
 * Function [[push]] returns a pointer to the frame just
 * pushed.
 * <context-stack.c>=
 */
static Frame *push (Frame f, Stack s) {
    assert(s);
    /*
     * \qbreak Pushing a frame must not cause a stack to
     * grow without bound. Ten thousand stack frames ought
     * to be enough for anybody.
     * <if stack [[s]] is full, enlarge it>=
     */
    if (s->sp - s->frames == s->size) {
        unsigned newsize = 2 * s->size;
        if (newsize > 10000) {
            clearstack(s);
            runerror("recursion too deep");
        }
        s->frames = realloc(s->frames, newsize * sizeof(*s->frames));
        assert(s->frames);
        s->sp = s->frames + s->size;
        s->size = newsize;
    }
    *s->sp++ = f;
    /*
     * <set [[high_stack_mark]] from stack [[s]]>=
     */
    {   int n = s->sp - s->frames;
        if (n > high_stack_mark)
            high_stack_mark = n;
    }
    return s->sp - 1;
}
/*
 * Code in \crefschemes.chap is simplified by using
 * [[pushframe]], which pushes syntax.
 * <context-stack.c>=
 */
static Frame mkExpFrame(struct Exp e) {
  Frame fr;
  fr.form = e;
  fr.syntax = NULL;
  return fr;
}

Exp pushframe(struct Exp e, Stack s) {
  Frame *fr;
  assert(s);
  fr = push(mkExpFrame(e), s);
  return &fr->form;
}
/*
 * Any frame that is pushed can be popped. But since
 * popping doesn't require allocating memory or updating
 * the high stack mark, it doesn't require memory
 * management or instrumentation.
 * <context-stack.c>=
 */
void popframe (Stack s) {
    assert(s->sp - s->frames > 0);
    s->sp--;
}
/*
 * <context-stack.c>=
 */
void printframe (Printbuf output, Frame *fr) {
    bprint(output, "%*: ", (void *) fr);
    bprint(output, "[%e]", &fr->form);
}
/*
 * <context-stack.c>=
 */
void printoneframe(Printbuf output, va_list_box *box) {
    Frame *fr = va_arg(box->ap, Frame*);
    printframe(output, fr);
}
/*
 * \qbreak
 * <context-stack.c>=
 */
void printstack(Printbuf output, va_list_box *box) {
    Stack s = va_arg(box->ap, Stack);
    Frame *fr;

    for (fr = s->sp-1; fr >= s->frames; fr--) {
        bprint(output, "  ");
        printframe(output, fr);
        bprint(output, ";\n");
    }
}
/*
 * <context-stack.c>=
 */
void printnoenv(Printbuf output, va_list_box* box) {
    Env env = va_arg(box->ap, Env);
    bprint(output, "@%*", (void *)env);
}
