CC        = gcc
PKGCONFIG = $(shell which pkg-config)
CFLAGS    = $(shell $(PKGCONFIG) --cflags gtk+-3.0)
LIBS      = $(shell $(PKGCONFIG) --libs gtk+-3.0)
SRC       = $(wildcard *.c)
OBJS      = $(SRC:.c=.o)
OUT       = bin/wemacs

test: all
	$(OUT)

all: $(OUT)

%.o: %.c Makefile 
	@$(CC) -c -o $@ $(CFLAGS) $<
	@printf "CC\t $@\n"

$(OUT): Makefile bin $(OBJS)
	@$(CC) -o $@ $(OBJS) $(LIBS)
	@printf "LD\t $@\n"

clean:
	rm -f $(OBJS)
	rm -f exampleapp

bin:
	mkdir bin
