CFLAGS=-W
CC=ghc
BIN=lc
SRC=lc.hs

all: $(BIN)

$(BIN): $(SRC)
	$(CC) $(CFLAGS) $^ -o $@

check: $(BIN)
	./$(BIN) < tests/misc > tests/out
	diff tests/misc-exp tests/out
	./$(BIN) < tests/errors > tests/out
	diff tests/errors-exp tests/out
	$(RM) tests/out

clean:
	$(RM) *.o *.hi

.PHONY: all clean
