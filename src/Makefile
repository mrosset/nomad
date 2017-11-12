CC        = gcc
PKGCONFIG = $(shell which pkg-config)
CFLAGS    = $(shell $(PKGCONFIG) --cflags gtk+-3.0 guile-2.2)
LIBS      = $(shell $(PKGCONFIG) --libs gtk+-3.0 guile-2.2)
SRC       = $(wildcard *.c)
OBJS      = $(SRC:.c=.o)
BIN       = bin/wemacs
GEN       = scheme.x resources.c

test: clean all
	$(BIN) -l ../test.scm -c "(display (wemacs-version))"

all: $(BIN)

%.o: %.c $(GEN) Makefile 
	@$(CC) -c -o $@ $(CFLAGS) $<
	@printf "CC\t $@\n"

$(BIN): Makefile bin $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)
	@printf "LD\t $@\n"

clean:
	-rm -f $(OBJS)
	-rm -rf bin
	-rm resources.c scheme.x

scheme.x: scheme.c
	guile-snarf -o $@ $< $(CFLAGS)

resources.c: window.ui
	glib-compile-resources .gresource.xml --target=resources.c --generate-source

bin:
	mkdir bin
