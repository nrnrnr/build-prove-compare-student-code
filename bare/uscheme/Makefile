#
# Makefile for uscheme
#

SOURCES  = arith.c ast-code.c env.c error.c eval.c evaldef.c\
           lex.c linestream.c list-code.c loc.c name.c\
           overflow.c par-code.c parse.c prim.c print.c\
           printbuf.c printfuns.c scheme-tests.c scheme.c\
           tableparsing.c tests.c unicode.c value-code.c\
           value.c xdefstream.c
HEADERS  = all.h prim.h
OBJECTS  = $(SOURCES:.c=.o)
RESULT   = uscheme

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
eval.o: eval.c $(HEADERS)
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
value-code.o: value-code.c $(HEADERS)
ast-code.o: ast-code.c $(HEADERS)
par-code.o: par-code.c $(HEADERS)
list-code.o: list-code.c $(HEADERS)
