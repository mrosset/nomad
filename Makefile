CC            = gcc
CFLAGS        = -Werror -shared -fPIC
SRC           = $(wildcard *.c)
LDFLAGS       =
EMACS_VERSION = 26.0.9
NAME          = wemacs

lib/$(NAME).so: main.o lib
		$(CC) $(CFLAGS) $< -o $@

main.o: $(SRC) Makefile emacs-module.h
		$(CC) $(CFLAGS) -c $< -o $@

clean:
		-rm main *.o emacs-module.h
		-rm -r lib

emacs-module.h:
		scp orion:src/build/emacs-$(EMACS_VERSION)/src/$@ $@

test: lib/$(NAME).so
		 emacs -Q -L ./lib -batch -l wemacs --eval "(message (wemacs-version))"

.PHONY:
lib:
		mkdir $@
