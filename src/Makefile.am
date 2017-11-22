AUTOMAKE_OPTIONS = gnu

bin_PROGRAMS = wemacs

.PHONY: test format

test:
	gcc  $(GUILE_CFLAGS) test.c -o $@
	./test

wemacs_SOURCES = main.c \
	app.c \
	app.h \
	gresourec.xml \
	resources.c \
	scheme.c \
	scheme.h \
	scheme.x \
	vte.c \
	vte.h \
	window.c \
	window.h

AM_CFLAGS = -Wall -Werror -Wdeclaration-after-statement -rdynamic \
	$(GTK_FLAGS) \
	$(GUILE_CFLAGS) \
	$(VTE_CFLAGS) \
	$(WEBKIT_CFLAGS) \
	$(GTK_SOURCEVIEW_CFLAGS) \
	-DWEMACS_WEB_EXTENSIONS_DIR=\""$(libexecdir)/wemacs/"\" \
	-DWEMACS_SCHEME_INIT=\""$(HOME)/src/wemacs/test.scm"\"

wemacs_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(VTE_LIBS) $(WEBKIT_LIBS) $(GTK_SOURCEVIEW_LIBS)

DOT_X_FILES = scheme.x

BUILT_SOURCES = $(DOT_X_FILES) resources.c

CLEANFILES = $(DOT_X_FILES) resources.c

format:
	 clang-format -i *.c *.h

resources.c: gresource.xml window.ui
	glib-compile-resources $< --target=$@ --generate-source

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
.c.x:
	guile-snarf$(GUILE_EFFECTIVE_VERSION) -o $@ $< $(snarfcppopts) $(AM_FLAGS)
