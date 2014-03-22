# -*- makefile -*-

CC=gcc
TARGET=main

##################################

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)
GLFLAGS=-lGL -lGLU -lglut -lGLEW
CFLAGS=-Wall -Wextra $(GLFLAGS)

##################################

all: $(TARGET)

# linking
$(TARGET): $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

# .c -> .o
%.o: %.c
	$(CC) -o $@ -c $<

clean:
	rm -f *.o
	rm -f *~

run: all
	./$(TARGET)

.PHONY: clean all run

# References
# http://pt.wikibooks.org/wiki/Programar_em_C/Makefiles
