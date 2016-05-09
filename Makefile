CFLAGS=-W
CC=ghc
BIN=lc
SRC=lc.hs

all: $(BIN)

$(BIN): $(SRC)
	$(CC) $(CFLAGS) $^ -o $@

check: $(BIN)
	./$(BIN) < examples > out
	diff expected out

clean:
	$(RM) *.o *.hi

.PHONY: all clean
