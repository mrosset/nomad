CC            = gcc
PKGS          = gtk+-3.0 webkit2gtk-4.0 guile-2.0
CFLAGS        = -pipe -O2 -Werror `pkg-config --cflags $(PKGS)`
LIBS          = `pkg-config --libs $(PKGS)`
SRC           = $(wildcard *.c)
EMACS_VERSION = 26.0.9
NAME          = wemacs
BATCH         = emacs -Q -L ./lib -l wemacs

test: bin/wemacs
	 bin/wemacs  --no-auto-compile  -l test.scm -c "(wemacs-test)"

bin/wemacs: $(SRC) bin Makefile
		gtags
		$(CC) $(CFLAGS) $(LIBS) $(SRC) -o $@

lib/$(NAME).so: $(SRC) lib Makefile emacs-module.h
		gtags
		$(CC) $(CFLAGS) -L $(LDFLAGS) $(SRC) -o $@

clean:
		-rm main *.o emacs-module.h
		-rm -r lib

emacs-module.h:
		scp orion:src/build/emacs-$(EMACS_VERSION)/src/$@ $@

bin:
		mkdir $@
lib:
		mkdir $@
