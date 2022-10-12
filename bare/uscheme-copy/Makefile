#
# Makefile for uscheme-copy
#

SOURCES  = arith.c ast-code.c context-lists.c context-stack.c\
           copy.c emscheme.c env.c error.c eval-stack.c\
           evaldef.c extend-syntax.c gcdebug.c lex.c\
           linestream.c list-code.c loc.c lower.c name.c\
           options.c overflow.c par-code.c parse.c prim.c\
           print.c printbuf.c printfuns.c root.c\
           scheme-tests.c scheme.c stack-debug.c\
           tableparsing.c tests.c unicode.c validate.c\
           value-code.c value.c xdefstream.c
HEADERS  = all.h prim.h
OBJECTS  = $(SOURCES:.c=.o)
RESULT   = uscheme-copy

CC = gcc -std=c11 -pedantic -Wall -Werror -Wextra -Wno-overlength-strings
CFLAGS = -g 
LDFLAGS = -g
CPPFLAGS = -I.
RM = rm -f 

.SUFFIXES:
.SUFFIXES: .c .o

$(RESULT): $(OBJECTS)
	$(CC) -o $@ $(LDFLAGS) $(OBJECTS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

clean:
	$(RM) $(RESULT) *.o *.core core *~

env.o: env.c $(HEADERS)
printfuns.o: printfuns.c $(HEADERS)
parse.o: parse.c $(HEADERS)
error.o: error.c $(HEADERS)
lex.o: lex.c $(HEADERS)
linestream.o: linestream.c $(HEADERS)
name.o: name.c $(HEADERS)
overflow.o: overflow.c $(HEADERS)
arith.o: arith.c $(HEADERS)
print.o: print.c $(HEADERS)
printbuf.o: printbuf.c $(HEADERS)
tableparsing.o: tableparsing.c $(HEADERS)
tests.o: tests.c $(HEADERS)
unicode.o: unicode.c $(HEADERS)
xdefstream.o: xdefstream.c $(HEADERS)
evaldef.o: evaldef.c $(HEADERS)
loc.o: loc.c $(HEADERS)
prim.o: prim.c $(HEADERS)
scheme.o: scheme.c $(HEADERS)
scheme-tests.o: scheme-tests.c $(HEADERS)
value.o: value.c $(HEADERS)
context-lists.o: context-lists.c $(HEADERS)
context-stack.o: context-stack.c $(HEADERS)
eval-stack.o: eval-stack.c $(HEADERS)
extend-syntax.o: extend-syntax.c $(HEADERS)
lower.o: lower.c $(HEADERS)
options.o: options.c $(HEADERS)
stack-debug.o: stack-debug.c $(HEADERS)
validate.o: validate.c $(HEADERS)
emscheme.o: emscheme.c $(HEADERS)
gcdebug.o: gcdebug.c $(HEADERS)
root.o: root.c $(HEADERS)
copy.o: copy.c $(HEADERS)
value-code.o: value-code.c $(HEADERS)
ast-code.o: ast-code.c $(HEADERS)
par-code.o: par-code.c $(HEADERS)
list-code.o: list-code.c $(HEADERS)
