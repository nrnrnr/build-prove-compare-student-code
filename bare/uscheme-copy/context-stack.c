#include "all.h"
/* context-stack.c S343b */
/* representation of [[struct Stack]] S343a */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};

bool optimize_tail_calls = true;
int  high_stack_mark;   // max number of frames used in the current evaluation
bool show_high_stack_mark;
/* context-stack.c S343c */
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
/* context-stack.c S344a */
void clearstack (Stack s) {
    s->sp = s->frames;
}
/* context-stack.c S344c */
Frame *topframe (Stack s) {
    assert(s);
    if (s->sp == s->frames)
        return NULL;
    else
        return s->sp - 1;
}
/* context-stack.c S344d */
Frame *topnonlabel (Stack s) {
    Frame *p;
    for (p = s->sp; p > s->frames && p[-1].form.alt == LONG_LABEL; p--)
        ;
    if (p > s->frames)
        return p-1;
    else
        return NULL;
}
/* context-stack.c S344e */
static Frame *push (Frame f, Stack s) {
    assert(s);
    /* if stack [[s]] is full, enlarge it S345a */
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
    /* set [[high_stack_mark]] from stack [[s]] S346f */
    {   int n = s->sp - s->frames;
        if (n > high_stack_mark)
            high_stack_mark = n;
    }
    return s->sp - 1;
}
/* context-stack.c S345b */
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
/* context-stack.c S345c */
void popframe (Stack s) {
    assert(s->sp - s->frames > 0);
    s->sp--;
}
/* context-stack.c S346a */
void printframe (Printbuf output, Frame *fr) {
    bprint(output, "%*: ", (void *) fr);
    bprint(output, "[%e]", &fr->form);
}
/* context-stack.c S346b */
void printoneframe(Printbuf output, va_list_box *box) {
    Frame *fr = va_arg(box->ap, Frame*);
    printframe(output, fr);
}
/* context-stack.c S346c */
void printstack(Printbuf output, va_list_box *box) {
    Stack s = va_arg(box->ap, Stack);
    Frame *fr;

    for (fr = s->sp-1; fr >= s->frames; fr--) {
        bprint(output, "  ");
        printframe(output, fr);
        bprint(output, ";\n");
    }
}
/* context-stack.c S346d */
void printnoenv(Printbuf output, va_list_box* box) {
    Env env = va_arg(box->ap, Env);
    bprint(output, "@%*", (void *)env);
}
