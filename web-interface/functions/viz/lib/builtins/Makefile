CC=gcc
CFLAGS=-I.
OBJ = print.o cast.o strings.o list.o

.PHONY: default
default: viz_builtins.a

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

viz_builtins.a: $(OBJ)
	ar rcs $@ $^

.PHONY: clean
clean:
	rm -f *.o viz_builtins.a