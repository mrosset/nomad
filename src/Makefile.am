AUTOMAKE_OPTIONS = gnu

bin_PROGRAMS = wemacs

AM_CFLAGS = $(GTK_FLAGS) $(GUILE_CFLAGS) $(VTE_CFLAGS) $(WEBKIT_CFLAGS)

DOT_X_FILES = scheme.x

BUILT_SOURCES = $(DOT_X_FILES) resources.c

CLEANFILES = $(DOT_X_FILES) resources.c

resources.c: gresource.xml
	glib-compile-resources $< --target=$@ --generate-source

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS)

SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts) $(AM_FLAGS)

wemacs_SOURCES = main.c \
	scheme.h \
	scheme.x \
	scheme.c \
	resources.c \
	gresourec.xml \
	wemacsapp.c \
	wemacsapp.h \
	wemacsappwin.h \
	wemacsappwin.c 

wemacs_LDADD = $(GTK_LIBS) $(GUILE_LIBS) $(VTE_LIBS) $(WEBKIT_LIBS)
