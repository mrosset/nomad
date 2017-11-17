ACLOCAL_AMFLAG = -I m4

libwemacs_LTLIBRARIES = libwemacs.la
libwemacsdir = $(libexecdir)/wemacs/
libwemacs_la_SOURCES = ext.c
libwemacs_la_CFLAGS = $(WEB_EXTENSION_CFLAGS) $(GUILE_CFLAGS)
libwemacs_la_LIBADD = $(WEB_EXTENSION_LIBS) $(GUILE_LIBS) $(GLIB_LIBS)
libwemacs_la_LDFLAGS = -module -avoid-version -no-undefined0
