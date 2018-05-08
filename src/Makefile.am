AUTOMAKE_OPTIONS = gnu

bin_PROGRAMS = nomad

.PHONY: test format

test:
	gcc  $(GUILE_CFLAGS) test.c -o $@
	./test

EXTRA_DIST = gresource.xml window.ui

nomad_SOURCES = main.c \
	app.c \
	app.h \
	resources.c \
	scheme.c \
	scheme.h \
	scheme.x \
	vte.c \
	vte.h \
	window.c \
	window.h

AM_CFLAGS = -Wall -Werror -Wno-unused-function -Wdeclaration-after-statement \
	$(GTK_FLAGS) \
	$(GUILE_CFLAGS) \
	$(VTE_CFLAGS) \
	$(WEBKIT_CFLAGS) \
	$(GTK_SOURCEVIEW_CFLAGS) \
	-DNOMAD_WEB_EXTENSIONS_DIR=\""$(libexecdir)/nomad/"\"

AM_LDFLAGS = -export-dynamic

nomad_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(VTE_LIBS) $(WEBKIT_LIBS) $(GTK_SOURCEVIEW_LIBS)

DOT_X_FILES = scheme.x

BUILT_SOURCES = $(DOT_X_FILES) resources.c

CLEANFILES = $(DOT_X_FILES) resources.c

format:
	 clang-format -i *.c *.h

resources.c: gresource.xml window.ui
	$(GLIB_COMPILE_R) $< --target=$@ --generate-source

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts) $(AM_FLAGS)
