CFLAGS=-W
CC=ghc

all: lc

lc: lc.hs
	$(CC) $(CFLAGS) $< -o $@
