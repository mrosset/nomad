export PKG_CONFIG_PATH = $(HOME)/local/stow/vte.git/lib/pkgconfig
CC            = gcc
PKGS          = gtk+-3.0 webkit2gtk-4.0 guile-2.2 vte-2.91
CFLAGS        = -pipe -O2 -Werror `pkg-config --cflags $(PKGS)`
LIBS          = `pkg-config --libs $(PKGS)`
SRC           = $(wildcard *.c)
EMACS_VERSION = 26.0.9
NAME          = wemacs
BATCH         = emacs -Q -L ./lib -l wemacs
PREFIX        = $(HOME)/local

test: bin/wemacs install
	TERM=xterm-256color bin/wemacs -L $(PWD) -l test.scm -c "(start-repl) (wemacs-start)"

install: $(PREFIX)/bin/wemacs

$(PREFIX)/bin/wemacs: bin/wemacs
	install -m755 $< $(PREFIX)/bin/wemacs

bin/wemacs: $(SRC) bin Makefile
	$(CC) $(CFLAGS) $(LIBS) $(SRC) -o $@

lib/$(NAME).so: $(SRC) lib Makefile emacs-module.h
	gtags
	$(CC) $(CFLAGS) -L $(LDFLAGS) $(SRC) -o $@

clean:
	-rm -r bin

bin:
	mkdir $@
lib:
	mkdir $@
