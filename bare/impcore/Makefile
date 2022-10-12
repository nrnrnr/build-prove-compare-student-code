#
# Makefile for impcore
#

SOURCES  = arith.c definition-code.c env.c error.c eval.c\
           exp-code.c fun-code.c impcore.c imptests.c lex.c\
           linestream.c list-code.c name.c overflow.c\
           par-code.c parse.c print.c printbuf.c printfuns.c\
           tableparsing.c tests.c unicode.c xdef-code.c\
           xdefstream.c
HEADERS  = all.h
OBJECTS  = $(SOURCES:.c=.o)
RESULT   = impcore

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
impcore.o: impcore.c $(HEADERS)
imptests.o: imptests.c $(HEADERS)
exp-code.o: exp-code.c $(HEADERS)
definition-code.o: definition-code.c $(HEADERS)
xdef-code.o: xdef-code.c $(HEADERS)
fun-code.o: fun-code.c $(HEADERS)
list-code.o: list-code.c $(HEADERS)
par-code.o: par-code.c $(HEADERS)
